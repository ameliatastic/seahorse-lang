// TODO split so that we don't just have all the code in mod.rs
use crate::core::{
    clean::{ast as ca, clean},
    parse::parse,
    util::*,
    CoreError,
};
use std::{
    collections::HashMap,
    ffi::OsStr,
    fs::{read_dir, File},
    io::Read,
    path::PathBuf,
};

enum Error {
    CouldNotAddModule,
    CouldNotFind(ComboPath),
    PathOutsideRoot(ComboPath, usize),
    Os,
}

impl Error {
    fn core(&self) -> CoreError {
        match self {
            Self::CouldNotAddModule => CoreError::make_raw(
                "could not add module", //
                "",                     //
            ),
            Self::CouldNotFind(path) => CoreError::make_raw(
                format!("could not find import at {:?}", path), //
                "",                                             //
            ),
            Self::PathOutsideRoot(path, level) => CoreError::make_raw(
                "relative import would go outside of source code root", //
                format!(
                    "Hint: you can't go up {} directory levels from {:?}.",
                    level, path
                ), //
            ),
            Self::Os => CoreError::make_raw(
                "OS error", //
                "",         //
            ),
        }
    }
}

#[derive(Clone, Debug)]
pub struct ModuleRegistry {
    pub tree: Tree<Module>,
    pub origin: Vec<String>,
    pub order: Vec<String>,
}

impl ModuleRegistry {
    /// Get an absolute path. The registry search behavior is as such:
    ///     1. Check if from+ext is a valid path (searches relative area first)
    ///     2. For each `src` (specified by `self.order`), check if src+ext is a valid path.
    ///
    /// Returns None if a valid path could not be found.
    pub fn get_abs_path(&self, from: &Vec<String>, ext: &Vec<String>) -> Option<Vec<String>> {
        let mut path = from.clone();
        path.append(&mut ext.clone());

        if self.tree.get(&path).is_some() {
            return Some(path);
        }

        for src in self.order.iter() {
            let mut path = vec![src.clone()];
            path.append(&mut ext.clone());

            if self.tree.get(&path).is_some() {
                return Some(path);
            }
        }

        return None;
    }
}

/// Load a module from path, parse and clean it.
/// TODO unused so far, will be used when imports are really added
fn load_module(path: PathBuf) -> CResult<Module> {
    let mut input = File::open(path.clone())
        .map_err(|_| CoreError::make_raw(format!("could not open file {}", path.display()), ""))?;
    let mut py_src = String::new();
    input
        .read_to_string(&mut py_src)
        .map_err(|_| CoreError::make_raw("I/O error", ""))?;

    let parsed = parse(py_src.clone())?;
    let module = clean(parsed, py_src)?;

    return Ok(Module::Python(module));
}

fn from_os_string(os: &OsStr) -> String {
    os.to_str().unwrap().to_string()
}

#[derive(Clone, Debug)]
pub enum Module {
    Python(ca::Module),
    SeahorsePrelude,
}

/// A combined registry tree + filesystem path.
///
/// Usually registry tree paths are represented as a single vector of strings, here it's split into
/// the first element (`base`) and the remainder (`path`) for convenience. `fs_base` is the file-
/// system path of `base` (for example, if `base` is "dot", then `fs_base` is going to end in
/// "/programs_py", since that's where all of the "dot" files are located within the Seahorse
/// project.
///
/// The filesystem path of this path depends on whether the path will be used to retrieve a module
/// or a package. Modules are located at `fs_base/base/(...path).py`, and packages are located at
/// `fs_base/base/...path`.
#[derive(Clone, Debug)]
struct ComboPath {
    base: String,
    path: Vec<String>,
    fs_base: PathBuf,
}

impl ComboPath {
    fn new(mut path: Vec<String>, fs_base: PathBuf) -> Self {
        let base = path.remove(0);

        return Self {
            base,
            path,
            fs_base,
        };
    }

    /// Get the "full" path (base + path).
    fn get_path(&self) -> Vec<String> {
        let mut path = self.path.clone();
        path.insert(0, self.base.clone());
        return path;
    }

    /// Get the filesystem path for a module at this path
    fn get_fs_module_path(&self) -> PathBuf {
        let mut fs_path = self.fs_base.clone();
        for part in self.path.iter().take(self.path.len() - 1) {
            fs_path.push(part);
        }
        fs_path.push(format!("{}.py", self.path.last().unwrap()));

        return fs_path;
    }

    /// Get the filesystem path for a package at this path
    fn get_fs_package_path(&self) -> PathBuf {
        let mut fs_path = self.fs_base.clone();
        for part in self.path.iter() {
            fs_path.push(part);
        }

        return fs_path;
    }

    fn push(&mut self, part: String) {
        self.path.push(part);
    }

    fn pop(&mut self) {
        self.path.pop();
    }

    fn is_module(&self) -> bool {
        let fs_path = self.get_fs_module_path();
        return fs_path.is_file();
    }

    fn is_package(&self) -> bool {
        let fs_path = self.get_fs_package_path();
        let init_fs_path = fs_path.join("__init__.py");

        return fs_path.is_dir() && init_fs_path.is_file();
    }

    fn is_builtin(&self) -> bool {
        return self.base == "dot" && self.path.len() >= 1 && self.path.get(0).unwrap() == "seahorse"
    }
}

struct ModuleTreeBuilder {
    tree: Tree<Module>,
}

impl ModuleTreeBuilder {
    fn new() -> Self {
        Self {
            tree: Tree::Node(HashMap::new()),
        }
    }

    /// Add a module.
    fn add_module(&mut self, module: Module, path: ComboPath) -> CResult<()> {
        match module {
            Module::Python(module) => {
                // Insert something as a placeholder, to detect overwriting during recursive calls
                if !self
                    .tree
                    .insert(path.get_path(), Tree::Leaf(Module::SeahorsePrelude), false)
                {
                    return Err(Error::CouldNotAddModule.core());
                }

                for Located(loc, obj) in module.statements.iter() {
                    match obj {
                        ca::TopLevelStatementObj::Import { symbols } => {
                            for ca::ImportSymbol { symbol, .. } in symbols.iter() {
                                let mut path = path.clone();
                                path.pop();
                                path.push(symbol.clone());

                                if path.is_builtin() {
                                    continue;
                                }

                                if path.is_module() {
                                    let module = load_module(path.get_fs_module_path())?;
                                    self.add_module(module, path)?;
                                } else if path.is_package() {
                                    self.add_package(path)?;
                                }
                                // TODO check for ext. modules
                                else {
                                    return Err(Error::CouldNotFind(path)
                                        .core()
                                        .located(loc.clone()));
                                }
                            }
                        }
                        ca::TopLevelStatementObj::ImportFrom {
                            level,
                            path: symbol_path,
                            ..
                        } => {
                            if *level > path.path.len() {
                                return Err(Error::PathOutsideRoot(path, *level)
                                    .core()
                                    .located(loc.clone()));
                            }

                            let mut path = path.clone();
                            // Pop the module name
                            path.pop();
                            for _ in 0..*level {
                                path.pop();
                            }

                            'outer: loop {
                                for part in symbol_path.iter() {
                                    path.push(part.clone());

                                    if path.is_builtin() {
                                        break 'outer;
                                    }

                                    if path.is_module() {
                                        let module = load_module(path.get_fs_module_path())?;
                                        self.add_module(module, path)?;
                                        break 'outer;
                                    }
                                }

                                if path.is_package() {
                                    self.add_package(path)?;
                                } else {
                                    return Err(Error::CouldNotFind(path)
                                        .core()
                                        .located(loc.clone()));
                                }

                                break;
                            }
                        }
                        _ => {}
                    }
                }

                self.tree
                    .insert(path.get_path(), Tree::Leaf(Module::Python(module)), true);
            }
            module => {
                self.tree.insert(path.get_path(), Tree::Leaf(module), false);
            }
        }

        return Ok(());
    }

    /// Add a package (by path).
    ///
    /// Assumes that `path` definitely points to a filesystem package path.
    fn add_package(&mut self, path: ComboPath) -> CResult<()> {
        let fs_path = path.get_fs_package_path();
        for entry in read_dir(&fs_path).map_err(|_| Error::Os.core())? {
            let entry = entry.map_err(|_| Error::Os.core())?;
            let name = from_os_string(entry.file_name().as_os_str());

            if entry.path().is_file() && name.ends_with(".py") && name != "__init__.py" {
                let name = name.strip_suffix(".py").unwrap().to_string();
                let mut path = path.clone();
                path.push(name);

                let module = load_module(path.get_fs_module_path())?;
                self.add_module(module, path)?;
            } else if entry.path().is_dir() {
                let mut path = path.clone();
                path.push(name);

                if path.is_module() {
                    self.add_package(path)?;
                }
            }
        }

        return Ok(());
    }
}

/// Preprocess the source module by loading its dependencies and packaging them into a registry.
pub fn preprocess(module: ca::Module, working_dir: PathBuf) -> Result<ModuleRegistry, CoreError> {
    // TODO just assuming this structure for now:
    // programs_py (dot)
    // |\_ seahorse
    // |   |\_ prelude.py
    // |\_ program_name.py (program)
    //
    // In the future there will be other sources for modules (besides `dot`) - will need to make
    // sure that these are ordered in some way
    let mut builder = ModuleTreeBuilder::new();

    builder.add_module(
        Module::SeahorsePrelude,
        ComboPath::new(
            vec![
                "dot".to_string(),
                "seahorse".to_string(),
                "prelude".to_string(),
            ],
            PathBuf::new(),
        ),
    )?;

    builder.add_module(
        Module::Python(module),
        ComboPath::new(vec!["dot".to_string(), "program".to_string()], working_dir),
    )?;

    let registry = ModuleRegistry {
        tree: builder.tree,
        origin: vec!["dot".to_string(), "program".to_string()],
        order: vec!["dot".to_string()],
    };

    return Ok(registry);
}
