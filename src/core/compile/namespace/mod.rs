// TODO just throwing everything into mod.rs for now, don't want to deal with keeping things clean
// yet
use crate::core::{
    clean::ast as ca,
    compile::builtin::*,
    compile::check::*,
    preprocess::{self as pre, Module},
    util::*,
};
use crate::match1;
use std::collections::{BTreeMap, VecDeque};

enum Error {
    ImportNotFound(Vec<String>),
    CircularImport(Vec<String>),
    SymbolNotFound(String),
    AutomaticImportsNotExported(Vec<String>),
    NotDefType(Vec<String>),
    NoSuchSymbol(Vec<String>),
}

impl Error {
    fn core(self) -> CoreError {
        match self {
            Self::ImportNotFound(ext) => {
                CoreError::make_raw(format!("module \"{}\" not found", path_to_string(&ext)), "")
            }
            Self::CircularImport(path) => CoreError::make_raw(
                "circular import",
                format!(
                    "Path: \"{}\".",
                    path_to_string(&path.into_iter().skip(1).collect::<Vec<_>>())
                ),
            ),
            Self::SymbolNotFound(symbol) => {
                CoreError::make_raw(format!("symbol \"{}\" not found", symbol), "")
            }
            Self::AutomaticImportsNotExported(path) => {
                CoreError::make_raw(
                    "automatic import are not exported",
                    format!(
                        concat!(
                            "Help: this is an automatic import, which does not get exported from other modules.\n",
                            "You still have access to this object, just use its name: {}"
                        ),
                        path.last().unwrap()
                    )
                )
            }
            Self::NotDefType(path) => {
                CoreError::make_raw(format!("\"{}\" is not a class", path_to_string(&path)), "")
            }
            Self::NoSuchSymbol(rel) => {
                CoreError::make_raw(format!("symbol \"{}\" not found", path_to_string(&rel)), "")
            }
        }
    }
}

pub fn path_to_string(path: &Vec<String>) -> String {
    if path.len() == 0 {
        return "".to_string();
    }

    let mut path_string = path[0].clone();
    for part in path.iter().skip(1) {
        path_string.push('.');
        path_string.push_str(part.as_str());
    }

    return path_string;
}

/// The output of the "namespace" step. Transforms modules into namespaces, which are simply maps
/// from export name to export item.
///
/// An export can be a re-imported item from another module, or a defined object.
#[derive(Clone, Debug)]
pub struct NamespaceOutput {
    pub preprocessed: pre::ModuleRegistry,
    pub tree: Tree<Namespace>,
}

/// The (global) namespace for a module.
pub type Namespace = BTreeMap<String, NamespacedObject>;

impl Tree<Namespace> {
    /// Get an item given an absolute path.
    // pub fn get_item<'a>(&'a self, abs: &Vec<String>) -> Option<&'a Item> {
    //     let mut curr = self;

    //     for (i, part) in abs.iter().enumerate() {
    //         match curr {
    //             Self::Node(package) => {
    //                 curr = package.get(part)?;
    //             }
    //             Self::Leaf(namespace) => {
    //                 if i == abs.len() - 1 {
    //                     return namespace.get(part).and_then(|export| match export {
    //                         NamespacedObject::Item(item) => Some(item),
    //                         _ => None,
    //                     });
    //                 }
    //             }
    //         }
    //     }

    //     return None;
    // }

    /// Advance a relative path as far as possible.
    ///
    /// Two paths are returned: the resulting absolute path and the remainder of the relative path,
    /// which may be needed to index into the resulting item.
    pub fn advance_path(
        &self,
        rel: &Vec<String>,
        abs: &Vec<String>,
    ) -> Option<(Vec<String>, Vec<String>)> {
        self._advance_path(rel.clone().into(), abs)
    }

    fn _advance_path(
        &self,
        mut rel: VecDeque<String>,
        abs: &Vec<String>,
    ) -> Option<(Vec<String>, Vec<String>)> {
        let next = match rel.pop_front() {
            Some(next) => next,
            None => {
                return Some((abs.clone(), vec![]));
            }
        };

        match self.get(abs) {
            Some(Tree::Leaf(namespace)) => match namespace.get(&next) {
                Some(NamespacedObject::Automatic(..) | NamespacedObject::Item(..)) => {
                    let mut abs = abs.clone();
                    abs.push(next);
                    Some((abs, rel.into()))
                }
                Some(NamespacedObject::Import(Located(_, import))) => match import.import_type {
                    ImportType::Symbol => {
                        let mut abs = import.path.clone();
                        rel.push_front(abs.pop().unwrap());
                        self._advance_path(rel, &abs)
                    }
                    ImportType::Module | ImportType::Package => match self.get(&import.path) {
                        Some(..) => self._advance_path(rel, &import.path),
                        None => None,
                    },
                },
                None => None,
            },
            Some(Tree::Node(package)) => match package.get(&next) {
                Some(..) => {
                    let mut abs = abs.clone();
                    abs.push(next);
                    self._advance_path(rel, &abs)
                }
                None => None,
            },
            None => None,
        }
    }
}

/// Module export.
#[derive(Clone, Debug)]
pub enum NamespacedObject {
    Automatic(Builtin),
    Import(Import),
    Item(Item),
}

/// Module import. Just a path that is distinguished between an imported symbol, module, or package.
/// Guaranteed to exist in the tree.
pub type Import = Located<ImportObj>;
#[derive(Clone, Debug)]
pub struct ImportObj {
    pub path: Vec<String>,
    pub import_type: ImportType,
    pub is_builtin: bool,
}

#[derive(Clone, Debug)]
pub enum ImportType {
    Symbol,
    Module,
    Package,
}

#[derive(Clone, Debug)]
pub enum Item {
    Defined(ca::TopLevelStatement),
    Builtin(Builtin),
}

/// "Work-in-progress" namespace, used for circular import detection.
enum Wip {
    Empty,
    Pending,
    Done(Namespace),
}

impl TryFrom<pre::ModuleRegistry> for NamespaceOutput {
    type Error = CoreError;

    fn try_from(registry: pre::ModuleRegistry) -> CResult<NamespaceOutput> {
        let mut wip = registry.tree.clone().map(|module| match module {
            Module::Python(..) => Wip::Empty,
            Module::SeahorsePrelude => Wip::Done(prelude::namespace()),
            Module::SeahorsePyth => Wip::Done(pyth::namespace()),
        });

        build_namespace(&mut wip, &registry, &registry.origin)?;

        let tree = wip.map(|namespace| match namespace {
            Wip::Done(namespace) => namespace,
            _ => panic!(),
        });

        let registry = NamespaceOutput {
            preprocessed: registry,
            tree,
        };

        return Ok(registry);
    }
}

impl Tree<Namespace> {
    pub fn build_ty(&self, ty_expr: &ca::TyExpression, abs: &Vec<String>) -> CResult<Ty> {
        let Located(loc, obj) = ty_expr;

        match obj {
            ca::TyExpressionObj::Generic { base, params } => {
                let params = params
                    .iter()
                    .map(|param| self.build_ty(param, abs))
                    .collect::<Result<Vec<_>, _>>()?;

                let base = match self.advance_path(base, abs) {
                    Some((path, rem)) => {
                        if rem.len() == 0 {
                            match self.get_leaf_ext(&path).unwrap() {
                                NamespacedObject::Automatic(builtin) => {
                                    if !path.starts_with(&abs) {
                                        Err(Error::AutomaticImportsNotExported(path)
                                            .core()
                                            .located(loc.clone()))
                                    } else {
                                        Ok(TyName::Builtin(builtin.clone()))
                                    }
                                }
                                NamespacedObject::Item(Item::Defined(def)) => {
                                    let Located(_, obj) = def;
                                    match obj {
                                        ca::TopLevelStatementObj::ClassDef { .. } => {
                                            Ok(TyName::Defined(path, DefinedType::Struct))
                                        }
                                        _ => {
                                            Err(Error::NotDefType(path).core().located(loc.clone()))
                                        }
                                    }
                                }
                                NamespacedObject::Item(Item::Builtin(builtin)) => {
                                    builtin
                                        .as_instance(&params)
                                        .map_err(|err| err.located(loc.clone()))?;

                                    Ok(TyName::Builtin(builtin.clone()))
                                }
                                _ => panic!(),
                            }
                        } else {
                            Err(Error::NoSuchSymbol(base.clone())
                                .core()
                                .located(loc.clone()))
                        }
                    }
                    _ => Err(Error::NoSuchSymbol(base.clone())
                        .core()
                        .located(loc.clone())),
                }?;

                Ok(Ty::Generic(base, params))
            }
            ca::TyExpressionObj::Const(n) => Ok(Ty::Const(*n)),
        }
        .map_err(|err: Error| err.core().located(loc.clone()))
    }
}

// TODO after moving some things around, there's just a bunch of random functions lying around.
// should find commonalities between them and give them a Context struct or something

/// Recursively build the namespace associated with `path`.
fn build_namespace(
    wip: &mut Tree<Wip>,
    registry: &pre::ModuleRegistry,
    path: &Vec<String>,
) -> CResult<()> {
    let node = wip.get(path).unwrap();
    match &node {
        &Tree::Leaf(Wip::Pending) => return Err(Error::CircularImport(path.clone()).core()),
        &Tree::Leaf(Wip::Done(..)) => return Ok(()),
        &Tree::Leaf(Wip::Empty) => {
            *wip.get_mut(path).unwrap() = Tree::Leaf(Wip::Pending);

            let module = match1!(registry.tree.get(path).unwrap(), Tree::Leaf(module) => module);
            let namespace = match module {
                pre::Module::Python(module) => build_python_namespace(wip, registry, path, module)?,
                _ => panic!(),
            };

            *wip.get_mut(path).unwrap() = Tree::Leaf(Wip::Done(namespace));
        }
        // TODO when asked to build the namespace of a package, recursively builds each of its
        // constituent modules/packages. This is a bit outside of what Python does, needs work
        &Tree::Node(package) => {
            // Can't just iterate over keys, since that contains an immutable borrow to wip.
            let keys = package.keys().map(|key| key.clone()).collect::<Vec<_>>();
            for key in keys.iter() {
                let mut path_ = path.clone();
                path_.push(key.clone());
                build_namespace(wip, registry, &path_)?;
            }

            return Ok(());
        }
    }

    Ok(())
}

/// Build the namespace for a Python module at `path`.
fn build_python_namespace(
    wip: &mut Tree<Wip>,
    registry: &pre::ModuleRegistry,
    path: &Vec<String>,
    module: &ca::Module,
) -> CResult<Namespace> {
    let mut namespace = BTreeMap::new();

    // Automatic imports
    namespace.append(&mut prelude::namespace());
    namespace.append(&mut python::namespace());

    for statement in module.statements.iter() {
        let Located(loc, obj) = statement;
        match obj {
            ca::TopLevelStatementObj::Import { symbols } => {
                for ca::ImportSymbol { symbol, alias } in symbols.iter() {
                    // TODO i think symbols here can be deep imports (like `import
                    // seahorse.prelude`)?
                    let ext = vec![symbol.clone()];
                    // Safe unwrap - preprocessor has already resolved all imports
                    let abs = registry.get_abs_path(path, &ext).ok_or(
                        Error::ImportNotFound(ext.clone())
                            .core()
                            .located(loc.clone()),
                    )?;

                    build_namespace(wip, registry, &abs)?;

                    let obj =
                        get_import_obj(wip, &abs, None).map_err(|err| err.located(loc.clone()))?;
                    let name = alias.clone().unwrap_or(symbol.clone());
                    namespace.insert(name, NamespacedObject::Import(Located(loc.clone(), obj)));
                }
            }
            ca::TopLevelStatementObj::ImportFrom {
                // TODO relative imports
                path: ext,
                symbols,
                ..
            } => {
                let abs = registry.get_abs_path(path, ext).ok_or(
                    Error::ImportNotFound(ext.clone())
                        .core()
                        .located(loc.clone()),
                )?;

                build_namespace(wip, registry, &abs)?;

                for ca::ImportSymbol { symbol, alias } in symbols.iter() {
                    if symbol.as_str() == "*" {
                        let glob = match wip.get(&abs).unwrap() {
                            Tree::Leaf(Wip::Done(namespace)) => {
                                namespace
                                    .iter()
                                    .filter_map(|(name, object)| {
                                        // Filter out the automatic imports
                                        if let NamespacedObject::Automatic(..) = object {
                                            None
                                        } else {
                                            Some(name)
                                        }
                                    })
                                    .collect::<Vec<_>>()
                            }
                            Tree::Node(package) => package.keys().collect::<Vec<_>>(),
                            _ => panic!(),
                        };

                        for symbol in glob.into_iter() {
                            let obj = get_import_obj(wip, &abs, Some(symbol))
                                .map_err(|err| err.located(loc.clone()))?;
                            namespace.insert(
                                symbol.clone(),
                                NamespacedObject::Import(Located(loc.clone(), obj)),
                            );
                        }
                    } else {
                        let obj = get_import_obj(wip, &abs, Some(symbol))
                            .map_err(|err| err.located(loc.clone()))?;
                        let name = alias.clone().unwrap_or(symbol.clone());
                        namespace.insert(name, NamespacedObject::Import(Located(loc.clone(), obj)));
                    }
                }
            }
            ca::TopLevelStatementObj::Constant { name, .. }
            | ca::TopLevelStatementObj::ClassDef { name, .. }
            | ca::TopLevelStatementObj::FunctionDef(ca::FunctionDef { name, .. }) => {
                let export =
                    NamespacedObject::Item(Item::Defined(Located(loc.clone(), obj.clone())));
                namespace.insert(name.clone(), export);
            }
            ca::TopLevelStatementObj::Expression(..) => {}
        }
    }

    return Ok(namespace);
}

/// Finds an imported object given a path within the namespace tree and optional extra symbol.
///
/// Requires the caller to make sure that the namespace at `path` has been built already.
fn get_import_obj(
    wip: &Tree<Wip>,
    path: &Vec<String>,
    symbol: Option<&String>,
) -> CResult<ImportObj> {
    match wip.get(path).unwrap() {
        Tree::Leaf(Wip::Done(namespace)) => match symbol {
            Some(symbol) => {
                if let Some(object) = namespace.get(symbol) {
                    let is_builtin = match object {
                        NamespacedObject::Item(Item::Defined(..)) => false,
                        NamespacedObject::Import(Located(
                            _,
                            ImportObj {
                                is_builtin: false, ..
                            },
                        )) => false,
                        _ => true,
                    };

                    let mut path = path.clone();
                    path.push(symbol.clone());
                    Ok(ImportObj {
                        path,
                        import_type: ImportType::Symbol,
                        is_builtin,
                    })
                } else {
                    Err(Error::SymbolNotFound(symbol.clone()).core())
                }
            }
            None => Ok(ImportObj {
                path: path.clone(),
                import_type: ImportType::Module,
                is_builtin: path.starts_with(&["sh".to_string()]),
            }),
        },
        Tree::Node(package) => match symbol {
            Some(symbol) => match package.get(symbol) {
                Some(Tree::Leaf(..)) => {
                    let mut path = path.clone();
                    path.push(symbol.clone());
                    let is_builtin = path.starts_with(&["sh".to_string()]);
                    Ok(ImportObj {
                        path: path,
                        import_type: ImportType::Module,
                        is_builtin,
                    })
                }
                Some(Tree::Node(..)) => {
                    let mut path = path.clone();
                    path.push(symbol.clone());
                    let is_builtin = path.starts_with(&["sh".to_string()]);
                    Ok(ImportObj {
                        path: path,
                        import_type: ImportType::Package,
                        is_builtin,
                    })
                }
                None => Err(Error::SymbolNotFound(symbol.clone()).core()),
            },
            None => Ok(ImportObj {
                path: path.clone(),
                import_type: ImportType::Package,
                is_builtin: path.starts_with(&["sh".to_string()]),
            }),
        },
        _ => panic!(),
    }
}

/// Builds the namespace of every module.
pub fn namespace(registry: pre::ModuleRegistry) -> CResult<NamespaceOutput> {
    registry.try_into()
}
