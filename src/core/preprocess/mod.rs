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
    fmt::{self, Display, Formatter},
    fs::{read_dir, File},
    io::Read,
    path::PathBuf,
};

use super::compile::builtin::prelude::path_to_string;

enum Error {
    CouldNotAddModule,
    CouldNotFind(ComboPath),
    RelativeSeahorseImport,
    PathOutsideRoot(ComboPath, usize),
    Os,
}

impl Error {
    fn core(&self) -> CoreError {
        match self {
            Self::CouldNotAddModule => CoreError::make_raw(
                "could not add module",
                "",
            ),
            Self::CouldNotFind(path) => CoreError::make_raw(
                format!("could not find import at {}", path),
                "",
            ),
            Self::RelativeSeahorseImport => CoreError::make_raw(
                "attempted to import local Seahorse files",
                "Help: `seahorse` is a builtin package, you should import it like a regular Python package:\n\n    from seahorse.prelude import *"
            ),
            Self::PathOutsideRoot(path, level) => CoreError::make_raw(
                "relative import would go outside of source code root",
                format!(
                    "Hint: you can't go up {} directory levels from {}.",
                    level, path
                ),
            ),
            Self::Os => CoreError::make_raw(
                "OS error",
                "",
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
    SeahorsePyth,
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
        return fs_path.is_dir();
    }
}

impl Display for ComboPath {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{} + {}",
            self.fs_base.display(),
            path_to_string(&self.path)
        )
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
                // (This also performs the check to see if the module is already added)
                if !self
                    .tree
                    .insert(path.get_path(), Tree::Leaf(Module::SeahorsePrelude), false)
                {
                    return Ok(());
                }

                for Located(loc, obj) in module.statements.iter() {
                    match obj {
                        ca::TopLevelStatementObj::Import { symbols } => {
                            for ca::ImportSymbol { symbol, .. } in symbols.iter() {
                                // Try a Seahorse import
                                {
                                    let mut path =
                                        ComboPath::new(vec!["sh".to_string()], PathBuf::new());
                                    path.push(symbol.clone());

                                    if self.tree.get(&path.get_path()).is_some() {
                                        continue;
                                    }
                                }

                                // Try a local import
                                {
                                    let mut path = path.clone();
                                    path.pop();
                                    path.push(symbol.clone());

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

                            if *level == path.path.len()
                                && symbol_path.get(0).unwrap() == "seahorse"
                            {
                                return Err(Error::RelativeSeahorseImport
                                    .core()
                                    .located(loc.clone()));
                            }

                            // (Loop used so that nested scopes can immediately break here)
                            'done: loop {
                                // Try a Seahorse import
                                if *level == 0 {
                                    let mut path =
                                        ComboPath::new(vec!["sh".to_string()], PathBuf::new());
                                    for part in symbol_path.iter() {
                                        path.push(part.clone());

                                        if self.tree.get(&path.get_path()).is_some() {
                                            break 'done;
                                        }
                                    }
                                }

                                // Try a local import
                                {
                                    let mut path = path.clone();
                                    // Pop the module name
                                    path.pop();
                                    for _ in 0..*level {
                                        path.pop();
                                    }

                                    for part in symbol_path.iter() {
                                        path.push(part.clone());

                                        if path.is_module() {
                                            let module = load_module(path.get_fs_module_path())?;
                                            self.add_module(module, path)?;
                                            break 'done;
                                        }
                                    }

                                    if path.is_package() {
                                        self.add_package(path)?;
                                    } else {
                                        return Err(Error::CouldNotFind(path)
                                            .core()
                                            .located(loc.clone()));
                                    }
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

            if entry.path().is_file() && name.ends_with(".py") {
                let name = name.strip_suffix(".py").unwrap().to_string();
                let mut path = path.clone();
                path.push(name);

                let module = load_module(path.get_fs_module_path())?;
                self.add_module(module, path)?;
            } else if entry.path().is_dir() {
                let mut path = path.clone();
                path.push(name);

                if path.is_package() {
                    self.add_package(path)?;
                }
            }
        }

        return Ok(());
    }
}

/// Preprocess the source module by loading its dependencies and packaging them into a registry.
pub fn preprocess(module: ca::Module, working_dir: PathBuf) -> Result<ModuleRegistry, CoreError> {
    // The file system should have this structure:
    // programs_py
    // |\_ seahorse
    // |   |\_ prelude.py
    // |\_ program_name.py
    //
    // Which will be turned into this:
    // (root)
    // |\_ sh
    // |   |\_ seahorse
    // |       |\_ prelude
    // |\_ dot
    //     |\_ program
    //
    // sh and dot are the two registry sources. sh represents Seahorse builtins, and dot represents
    // local files.
    let mut builder = ModuleTreeBuilder::new();

    builder.add_module(
        Module::SeahorsePrelude,
        ComboPath::new(
            vec![
                "sh".to_string(),
                "seahorse".to_string(),
                "prelude".to_string(),
            ],
            PathBuf::new(),
        ),
    )?;

    builder.add_module(
        Module::SeahorsePyth,
        ComboPath::new(
            vec!["sh".to_string(), "seahorse".to_string(), "pyth".to_string()],
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
        order: vec!["sh".to_string(), "dot".to_string()],
    };

    return Ok(registry);
}
