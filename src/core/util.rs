use owo_colors::OwoColorize;
pub use rustpython_parser::ast::Location;
use std::{collections::HashMap, error, fmt, rc::Rc};

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
pub enum CoreError {
    Raw {
        message: (String, String),
        location: Option<Location>,
    },
    WithContext {
        message: (String, String),
        location: Option<Location>,
        context: Option<String>,
    },
}

impl CoreError {
    pub fn located(self, location: Location) -> Self {
        match self {
            Self::Raw { message, .. } => Self::Raw {
                message,
                location: Some(location),
            },
            _ => panic!(),
        }
    }

    pub fn make_raw<H, F>(header: H, footer: F) -> Self
    where
        H: ToString,
        F: ToString,
    {
        Self::Raw {
            message: (header.to_string(), footer.to_string()),
            location: None,
        }
    }

    pub fn with_context(self, source: &String) -> Self {
        match self {
            Self::Raw {
                message,
                location: Some(location),
            } => {
                let context = source
                    .lines()
                    .nth(location.row() - 1)
                    .map(|s| s.to_string());
                Self::WithContext {
                    message,
                    context,
                    location: Some(location),
                }
            }
            Self::Raw { message, .. } => Self::WithContext {
                message,
                location: None,
                context: None,
            },
            with_context => with_context,
        }
    }
}

impl fmt::Display for CoreError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::WithContext {
                message: (header, footer),
                location: None,
                context: None,
            } => {
                write!(f, "Error: {}.\n{}", header, footer)
            }
            Self::WithContext {
                message: (header, footer),
                location: Some(location),
                context: Some(context),
            } => {
                let line = format!("{} |", location.row());
                if footer.len() > 0 {
                    write!(
                        f,
                        "{}: {}.\n{} {}\n{: >col$}\n{}\n",
                        "Error".red().bold(),
                        header.bold(),
                        line,
                        context,
                        "^".bold(),
                        footer,
                        col = line.len() + 1 + location.column()
                    )
                } else {
                    write!(
                        f,
                        "{}: {}.\n{} {}\n{: >col$}\n",
                        "Error".red().bold(),
                        header.bold(),
                        line,
                        context,
                        "^".bold(),
                        col = line.len() + 1 + location.column()
                    )
                }
            }
            s => {
                panic!(
                    "Attempted to display an unformattable error message ({:?})",
                    s
                )
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

/// Helper function to wait for 1 second, in case debug output is going by too quickly.
pub fn wait() {
    std::thread::sleep(std::time::Duration::from_secs(1));
}
