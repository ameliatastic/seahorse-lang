// TODO split so that we don't just have all the code in mod.rs
use crate::core::{
    clean::{ast as ca, clean},
    parse::parse,
    util::*,
    CoreError, TryPass,
};
use std::{collections::HashMap, env::current_dir, fs::File, io::Read, path::PathBuf};

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

#[derive(Clone, Debug)]
pub enum Module {
    Python(ca::Module),
    SeahorsePrelude,
}

/// Load a module from path, parse and clean it.
/// TODO unused so far, will be used when imports are really added
fn _load_module(path: PathBuf) -> Result<Module, CoreError> {
    let mut input = File::open(path.clone())
        .map_err(|_| CoreError::make_raw(format!("could not open file {}", path.display()), ""))?;
    let mut py_src = String::new();
    input
        .read_to_string(&mut py_src)
        .map_err(|_| CoreError::make_raw("I/O error", ""))?;

    let parsed = parse(py_src)?;
    let module = clean(parsed)?;

    return Ok(Module::Python(module));
}

/// Preprocess the source module by loading its dependencies and packaging them into a registry.
pub fn preprocess(module: ca::Module) -> Result<ModuleRegistry, CoreError> {
    // TODO just assuming this structure for now:
    // programs_py (dot)
    // |\_ seahorse
    // |   |\_ prelude.py
    // |\_ program_name.py (program)
    //
    // In the future there will be other sources for modules (besides `dot`) - will need to make
    // sure that these are ordered in some way
    let tree = Tree::Node(HashMap::from([(
        "dot".to_string(),
        Tree::Node(HashMap::from([
            (
                "seahorse".to_string(),
                Tree::Node(HashMap::from([(
                    "prelude".to_string(),
                    Tree::Leaf(Module::SeahorsePrelude),
                )])),
            ),
            ("program".to_string(), Tree::Leaf(Module::Python(module))),
        ])),
    )]));

    let registry = ModuleRegistry {
        tree,
        origin: vec!["dot".to_string(), "program".to_string()],
        order: vec!["dot".to_string()],
    };

    return Ok(registry);
}
