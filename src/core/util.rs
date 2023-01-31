use owo_colors::OwoColorize;
use rustpython_parser::ast::Location as SrcLocation;
use std::{
    collections::{BTreeMap, HashMap},
    error, fmt,
    rc::Rc,
};

/// Match and extract against a single pattern, panicking if no match.
#[macro_export]
macro_rules! match1 {
    ($obj:expr, $var:pat => $up:expr) => {
        match $obj {
            $var => $up,
            obj => panic!("match1 failed: received {:?}", obj),
        }
    };
}

#[derive(Debug, Clone)]
pub struct CoreError {
    message: (String, String),
    src: Option<Rc<String>>,
    loc: Option<SrcLocation>,
}

impl CoreError {
    pub fn located(self, loc: Location) -> Self {
        Self {
            loc: Some(loc.loc),
            src: Some(loc.src),
            ..self
        }
    }

    pub fn updated(self, loc: &Location) -> Self {
        Self {
            loc: Some(self.loc.unwrap_or_else(|| loc.loc.clone())),
            src: Some(self.src.unwrap_or_else(|| loc.src.clone())),
            ..self
        }
    }

    pub fn with_loc(self, loc: SrcLocation) -> Self {
        Self {
            loc: Some(loc),
            ..self
        }
    }

    pub fn with_src(self, src: Rc<String>) -> Self {
        Self {
            src: Some(src),
            ..self
        }
    }

    pub fn make_raw<H, F>(header: H, footer: F) -> Self
    where
        H: ToString,
        F: ToString,
    {
        Self {
            message: (header.to_string(), footer.to_string()),
            src: None,
            loc: None,
        }
    }
}

impl fmt::Display for CoreError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self {
                message: (header, footer),
                src: Some(src),
                loc: Some(loc),
            } => {
                let line = format!("{} |", loc.row());
                let context = src.lines().nth(loc.row() - 1).map(|s| s.to_string());

                if footer.len() > 0 {
                    write!(
                        f,
                        "{}: {}.\n{} {}\n{: >col$}\n{}\n",
                        "Error".red().bold(),
                        header.bold(),
                        line,
                        context.unwrap(),
                        "^".bold(),
                        footer,
                        col = line.len() + 1 + loc.column()
                    )
                } else {
                    write!(
                        f,
                        "{}: {}.\n{} {}\n{: >col$}\n",
                        "Error".red().bold(),
                        header.bold(),
                        line,
                        context.unwrap(),
                        "^".bold(),
                        col = line.len() + 1 + loc.column()
                    )
                }
            }
            Self {
                message: (header, footer),
                ..
            } => {
                write!(f, "Error: {}.\n{}", header, footer)
            }
        }
    }
}

impl error::Error for CoreError {}

pub type CResult<T> = Result<T, CoreError>;

/// Generic trait that performs a more general version of TryInto.
///
/// Fallibly performs a pass turning T into a U, using self as an object with necessary context.
/// Self is taken as a mutable reference, since the transformation might accumulate some information
/// in self.
pub trait TryPass<T, U>
where
    Self: Sized,
    T: Sized,
    U: Sized,
{
    fn try_pass(&mut self, t: T) -> CResult<U>;
}

#[derive(Debug, Clone)]
pub struct Location {
    pub src: Rc<String>,
    pub loc: SrcLocation,
}

impl Location {
    pub fn new(src: &Rc<String>, loc: SrcLocation) -> Self {
        Self {
            src: src.clone(),
            loc,
        }
    }
}

/// Generic type to encapsulate syntax elements with a source code location.
///
/// Some elements always carry location information with them. These are newtyped as an alias for
/// Located<T>, then by convention given the suffix "Obj".
#[derive(Clone, Debug)]
pub struct Located<T>(pub Location, pub T);

/// Generic type for a string-indexed tree.
#[derive(Clone, Debug)]
pub enum Tree<T> {
    Node(HashMap<String, Tree<T>>),
    Leaf(T),
}

impl<T> Tree<T> {
    /// Map every leaf element with a function, consuming this tree and returning a new one with
    /// the same structure.
    pub fn map<U, F>(self, f: F) -> Tree<U>
    where
        F: Fn(T) -> U,
    {
        self._map(Rc::new(f))
    }

    fn _map<U, F>(self, f: Rc<F>) -> Tree<U>
    where
        F: Fn(T) -> U,
    {
        match self {
            Self::Node(node) => Tree::Node(HashMap::from_iter(
                node.into_iter().map(|(k, v)| (k, v._map(f.clone()))),
            )),
            Self::Leaf(leaf) => Tree::Leaf((*f)(leaf)),
        }
    }

    /// Like map, but gives you an argument containing the leaf's path.
    pub fn map_with_path<U, F>(self, f: F) -> Tree<U>
    where
        F: Fn(T, &Vec<String>) -> U,
    {
        self._map_with_path(Rc::new(f), &mut vec![])
    }

    pub fn _map_with_path<U, F>(self, f: Rc<F>, path: &mut Vec<String>) -> Tree<U>
    where
        F: Fn(T, &Vec<String>) -> U,
    {
        match self {
            Self::Node(node) => {
                let mut node_ = HashMap::new();
                for (k, v) in node.into_iter() {
                    path.push(k.clone());
                    node_.insert(k, v._map_with_path(f.clone(), path));
                    path.pop();
                }

                Tree::Node(node_)
            }
            Self::Leaf(leaf) => Tree::Leaf((*f)(leaf, path)),
        }
    }

    pub fn zip<U>(self, tree: Tree<U>) -> Tree<(T, U)> {
        match (self, tree) {
            (Self::Node(node), Tree::Node(mut node_)) => {
                let mut zipped = HashMap::new();
                for (key, val) in node.into_iter() {
                    let val_ = node_.remove(&key).unwrap();
                    zipped.insert(key, val.zip(val_));
                }
                Tree::Node(zipped)
            }
            (Self::Leaf(leaf), Tree::Leaf(leaf_)) => Tree::Leaf((leaf, leaf_)),
            _ => panic!(),
        }
    }

    /// Get a reference to a node in the tree at the given `path`.
    pub fn get<'a>(&'a self, path: &Vec<String>) -> Option<&'a Self> {
        let mut tree = self;

        for part in path.iter() {
            match tree {
                Self::Node(node) => match node.get(part) {
                    Some(tree_) => {
                        tree = tree_;
                    }
                    None => {
                        return None;
                    }
                },
                _ => {
                    return None;
                }
            }
        }

        return Some(tree);
    }

    /// Get a reference to a leaf in the tree at the given `path`.
    pub fn get_leaf<'a>(&'a self, path: &Vec<String>) -> Option<&'a T> {
        match self.get(path) {
            Some(Self::Leaf(leaf)) => Some(leaf),
            _ => None,
        }
    }

    /// Get a mutable reference to a node in the tree at the given `path`.
    pub fn get_mut<'a>(&'a mut self, path: &Vec<String>) -> Option<&'a mut Self> {
        let mut tree = self;

        for part in path.iter() {
            match tree {
                Self::Node(node) => match node.get_mut(part) {
                    Some(tree_) => {
                        tree = tree_;
                    }
                    _ => {
                        return None;
                    }
                },
                _ => {
                    return None;
                }
            }
        }

        return Some(tree);
    }

    /// Insert a subtree into the tree at the given path.
    ///
    /// Returns whether the operation completed successfully - will fail if the insertion would have
    /// to override a preexisting part of the tree.
    pub fn insert(&mut self, mut path: Vec<String>, tree: Tree<T>, allow_overwrite: bool) -> bool {
        let last = match path.pop() {
            Some(last) => last,
            _ => {
                return false;
            }
        };

        return self._insert(path.into_iter(), last, tree, allow_overwrite);
    }

    fn _insert<I: Iterator<Item = String>>(
        &mut self,
        mut path: I,
        last: String,
        tree: Self,
        allow_overwrite: bool,
    ) -> bool {
        match self {
            Self::Node(node) => match path.next() {
                Some(part) => {
                    if !node.contains_key(&part) {
                        node.insert(part.clone(), Tree::Node(HashMap::new()));
                    }
                    let next = node.get_mut(&part).unwrap();

                    next._insert(path, last, tree, allow_overwrite)
                }
                None => {
                    if !allow_overwrite && node.contains_key(&last) {
                        return false;
                    }

                    node.insert(last, tree);
                    true
                }
            },
            _ => false,
        }
    }

    /// Return whether the tree contains a leaf at the given `path`.
    pub fn contains_leaf(&self, path: &Vec<String>) -> bool {
        match self.get(path) {
            Some(Self::Leaf(..)) => true,
            _ => false,
        }
    }

    /// Remove the subtree at the given `path`. Returns the removed subtree, if one exists.
    pub fn remove(&mut self, path: &Vec<String>) -> Option<Self> {
        if path.len() == 0 {
            return None;
        }

        let mut tree = self;
        for part in path.iter().take(path.len() - 1) {
            match tree {
                Self::Leaf(..) => {
                    return None;
                }
                Self::Node(node) => {
                    if !node.contains_key(part) {
                        return None;
                    }

                    tree = node.get_mut(part).unwrap();
                }
            }
        }

        return match tree {
            Self::Leaf(..) => None,
            Self::Node(node) => node.remove(path.last().unwrap()),
        };
    }

    /// Return whether this tree is "dead" (has no leaves).
    pub fn is_dead(&self) -> bool {
        match self {
            Self::Leaf(..) => false,
            Self::Node(node) => {
                if node.len() == 0 {
                    true
                } else {
                    node.values().all(|tree| tree.is_dead())
                }
            }
        }
    }
}

impl<T, E> Tree<Result<T, E>> {
    /// Turns a Tree<Result<T, E>> into a Result<Tree<T>, Vec<E>>.
    ///
    /// Only one error is returned. Multiple may have been generated, but whichever one is reached
    /// first gets returned.
    pub fn transpose(self) -> Result<Tree<T>, E> {
        match self {
            Self::Leaf(Ok(t)) => Ok(Tree::Leaf(t)),
            Self::Leaf(Err(e)) => Err(e),
            Self::Node(node) => {
                let mut node_ = HashMap::new();
                for (k, v) in node.into_iter() {
                    let v = v.transpose()?;
                    node_.insert(k, v);
                }

                Ok(Tree::Node(node_))
            }
        }
    }
}

impl<T> Tree<Option<T>> {
    /// Turn a tree of Option<T> into a tree of T by pruning `None` leaves.
    pub fn prune(self) -> Tree<T> {
        match self {
            Self::Node(node) => {
                let mut node_ = HashMap::new();
                for (key, val) in node.into_iter() {
                    match val {
                        node @ Self::Node(..) => {
                            node_.insert(key, node.prune());
                        }
                        Self::Leaf(Some(leaf)) => {
                            node_.insert(key, Tree::Leaf(leaf));
                        }
                        _ => {}
                    }
                }

                Tree::Node(node_)
            }
            _ => panic!("Cannot prune a leaf"),
        }
    }
}

impl<T> Tree<HashMap<String, T>> {
    /// Get the "leaf of a leaf" from a tree.
    pub fn get_leaf_ext<'a>(&'a self, path: &Vec<String>) -> Option<&'a T> {
        let mut path = path.clone();
        let name = path.pop()?;

        return self.get_leaf(&path).and_then(|leaf| leaf.get(&name));
    }
}

impl<T> Tree<BTreeMap<String, T>> {
    /// Get the "leaf of a leaf" from a tree.
    pub fn get_leaf_ext<'a>(&'a self, path: &Vec<String>) -> Option<&'a T> {
        let mut path = path.clone();
        let name = path.pop()?;

        return self.get_leaf(&path).and_then(|leaf| leaf.get(&name));
    }
}

/// Helper function to wait for 1 second, in case debug output is going by too quickly.
pub fn wait() {
    std::thread::sleep(std::time::Duration::from_secs(1));
}
