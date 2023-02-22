//! Python builtin types.

use crate::{
    core::compile::{ast::*, build::*, builtin::*},
    match1,
};
use prelude::{Namespace, NamespacedObject};
use quote::quote;
use std::collections::BTreeMap;

#[derive(Clone, Debug, PartialEq)]
pub enum Python {
    // Types
    None,
    List,
    Tuple,
    Int,
    Bool,
    Str,
    // Meta types
    Iter,
    AsLen,
    // Functions
    Abs,
    Print,
    Min,
    Max,
    Round,
    Range,
    Len,
    Enumerate,
    Filter,
    Map,
    Zip,
    Sorted,
    Sum,
    ListConstructor,
}

/// Create the Python builtins namespace.
pub fn namespace() -> Namespace {
    let data = [
        ("None", Python::None),
        ("List", Python::List),
        ("Tuple", Python::Tuple),
        ("int", Python::Int),
        ("bool", Python::Bool),
        ("str", Python::Str),
        ("abs", Python::Abs),
        ("print", Python::Print),
        ("min", Python::Min),
        ("max", Python::Max),
        ("round", Python::Round),
        ("range", Python::Range),
        ("len", Python::Len),
        ("enumerate", Python::Enumerate),
        ("filter", Python::Filter),
        ("map", Python::Map),
        ("zip", Python::Zip),
        ("sorted", Python::Sorted),
        ("sum", Python::Sum),
        ("list", Python::ListConstructor),
    ];

    let mut namespace = BTreeMap::new();
    for (name, obj) in data.into_iter() {
        namespace.insert(
            name.to_string(),
            NamespacedObject::Automatic(Builtin::Python(obj)),
        );
    }

    return namespace;
}

impl BuiltinSource for Python {
    fn name(&self) -> String {
        match self {
            Self::None => "None",
            Self::List => "List",
            Self::Tuple => "Tuple",
            Self::Int => "int",
            Self::Bool => "bool",
            Self::Str => "str",
            Self::Iter => "<Iter>",
            Self::AsLen => "<Len>",
            Self::Abs => "abs",
            Self::Print => "print",
            Self::Min => "min",
            Self::Max => "max",
            Self::Round => "round",
            Self::Range => "range",
            Self::Len => "len",
            Self::Enumerate => "enumerate",
            Self::Filter => "filter",
            Self::Map => "map",
            Self::Zip => "zip",
            Self::Sorted => "sorted",
            Self::Sum => "sum",
            Self::ListConstructor => "list",
        }
        .to_string()
    }

    fn ty(&self) -> Ty {
        match self {
            Self::Str => Ty::Type(
                TyName::Builtin(Builtin::Python(self.clone())),
                Some(Ty::new_function(
                    vec![("", Ty::Any, ParamType::Required)],
                    Ty::Transformed(
                        Ty::python(Self::Str, vec![]).into(),
                        Transformation::new(|mut expr| {
                            let s = match1!(expr.obj, ExpressionObj::Call { args, .. } => args.into_iter().next().unwrap());

                            expr.obj = if expr.ty.is_display() {
                                ExpressionObj::Rendered(quote! {
                                    format!("{}", #s)
                                })
                            } else {
                                ExpressionObj::Rendered(quote! {
                                    format!("{:?}", #s)
                                })
                            };

                            Ok(Transformed::Expression(expr))
                        })
                    )
                ).into())
            ),
            // abs(T) -> T
            Self::Abs => Ty::new_function(
                vec![("x", Ty::Anonymous(0), ParamType::Required)],
                Ty::Transformed(
                    Ty::Anonymous(0).into(),
                    Transformation::new(|mut expr| {
                        let x = match1!(expr.obj, ExpressionObj::Call { args, .. } => args.into_iter().next().unwrap());

                        match &expr.ty {
                            Ty::Generic(TyName::Builtin(Builtin::Prelude(Prelude::RustInt(true, _) | Prelude::RustFloat)), _) => {},
                            _ => {
                                return Err(CoreError::make_raw(
                                    "cannot take the absolute value of an unsigned integer",
                                    ""
                                ))
                            }
                        }

                        expr.obj = ExpressionObj::Rendered(quote! {
                            #x.abs()
                        });

                        Ok(Transformed::Expression(expr))
                    })
                )
            ),
            // print(...any) -> None
            Self::Print => Ty::new_function(
                vec![("", Ty::Any.into(), ParamType::Variadic)],
                Ty::Transformed(
                    Ty::python(Self::None, vec![]).into(),
                    Transformation::new(|expr| {
                        let args = match1!(expr.obj, ExpressionObj::Call { mut args, .. } => args.remove(0));
                        let args = match1!(args.obj, ExpressionObj::Vec(args) => args);

                        let mut format = String::new();
                        for arg in args.iter() {
                            if format.len() > 0 {
                                format.push_str(" ");
                            }

                            format.push_str(if arg.ty.is_display() { "{}" } else { "{:?}" });
                        }

                        Ok(Transformed::Expression(
                            ExpressionObj::Rendered(quote! {
                                solana_program::msg!(#format, #(#args),*)
                            })
                            .into(),
                        ))
                    }),
                ),
            ),
            // min(...T) -> T
            Self::Min => Ty::new_function(
                vec![("", Ty::Anonymous(0), ParamType::Variadic)],
                Ty::Transformed(
                    Ty::Anonymous(0).into(),
                    Transformation::new(|expr| {
                        let args = match1!(expr.obj, ExpressionObj::Call { args, .. } => args.into_iter().next().unwrap());
                        let mut parts =
                            match1!(args.obj, ExpressionObj::Vec(parts) => parts.into_iter());
                        if parts.len() < 2 {
                            return Err(CoreError::make_raw(
                                "min() requires at least 1 argument",
                                "",
                            ));
                        }

                        let mut accum = parts.next().unwrap();
                        for part in parts {
                            accum.obj = accum.obj.with_call("min", vec![part]);
                        }

                        Ok(Transformed::Expression(accum))
                    }),
                ),
            ),
            // max(...T) -> T
            Self::Max => Ty::new_function(
                vec![("", Ty::Anonymous(0), ParamType::Variadic)],
                Ty::Transformed(
                    Ty::Anonymous(0).into(),
                    Transformation::new(|expr| {
                        let args = match1!(expr.obj, ExpressionObj::Call { args, .. } => args.into_iter().next().unwrap());
                        let mut parts =
                            match1!(args.obj, ExpressionObj::Vec(parts) => parts.into_iter());
                        if parts.len() < 2 {
                            return Err(CoreError::make_raw(
                                "max() requires at least 1 argument",
                                "",
                            ));
                        }

                        let mut accum = parts.next().unwrap();
                        for part in parts {
                            accum.obj = accum.obj.with_call("max", vec![part]);
                        }

                        Ok(Transformed::Expression(accum))
                    }),
                ),
            ),
            // round(f64) -> i128
            Self::Round => Ty::new_function(
                vec![(
                    "x",
                    Ty::prelude(Prelude::RustFloat, vec![]),
                    ParamType::Required,
                )],
                Ty::Transformed(
                    Ty::prelude(Prelude::RustInt(true, 128), vec![]).into(),
                    Transformation::new(|mut expr| {
                        let args = match1!(expr.obj, ExpressionObj::Call { args, .. } => args);
                        let x = args.into_iter().next().unwrap();

                        expr.obj = ExpressionObj::As {
                            value: ExpressionObj::Rendered(quote! { #x.round() }).into(),
                            ty: TyExpr::new_specific(vec!["i128"], Mutability::Immutable)
                        };

                        Ok(Transformed::Expression(expr))
                    }),
                ),
            ),
            // range(T, T?, T?) -> <Iter>[T]
            Self::Range => Ty::new_function(
                vec![
                    ("start", Ty::Anonymous(0), ParamType::Required),
                    ("stop", Ty::Anonymous(0), ParamType::Optional),
                    ("step", Ty::Anonymous(0), ParamType::Optional)
                ],
                Ty::Transformed(
                    Ty::python(Self::Iter, vec![Ty::Anonymous(0)]).into(),
                    Transformation::new(|mut expr| {
                        let mut args =
                            match1!(expr.obj, ExpressionObj::Call { args, .. } => args.into_iter());
                        let start = args.next().unwrap();
                        let stop = args.next().unwrap();
                        let step = args.next().unwrap();

                        let range = match stop.obj {
                            ExpressionObj::Placeholder => ExpressionObj::Rendered(quote! {
                                0 .. #start
                            }),
                            stop => ExpressionObj::Rendered(quote! {
                                #start .. #stop
                            }),
                        };

                        let range = match step.obj {
                            ExpressionObj::Placeholder => range,
                            step => ExpressionObj::Rendered(quote! {
                                (#range).step_by(#step.try_into().unwrap())
                            })
                        };

                        expr.obj = range;

                        Ok(Transformed::Expression(expr))
                    }),
                ),
            ),
            // len(Cast(<Len>)) -> u64
            Self::Len => Ty::new_function(
                vec![
                    ("iterable", Ty::Cast(Ty::python(Self::AsLen, vec![]).into()), ParamType::Required)
                ],
                Ty::Transformed(
                    Ty::prelude(Prelude::RustInt(false, 64), vec![]).into(),
                    Transformation::new(|mut expr| {
                        let len = match1!(expr.obj, ExpressionObj::Call { args, .. } => args.into_iter().next().unwrap());

                        expr.obj = len.obj;

                        Ok(Transformed::Expression(expr))
                    })
                )
            ),
            // enumerate(Cast(<Iter>[T])) -> <Iter>[(u64, T)]
            Self::Enumerate => Ty::new_function(
                vec![(
                    "iterable",
                    Ty::Cast(Ty::python(Self::Iter, vec![Ty::Anonymous(0)]).into()),
                    ParamType::Required,
                )],
                Ty::Transformed(
                    Ty::python(
                        Self::Iter,
                        vec![Ty::python(
                            Self::Tuple,
                            vec![
                                Ty::prelude(Prelude::RustInt(false, 64), vec![]),
                                Ty::Anonymous(0),
                            ],
                        )],
                    )
                    .into(),
                    Transformation::new(|mut expr| {
                        let iterable = match1!(expr.obj, ExpressionObj::Call { args, .. } => args.into_iter().next().unwrap());

                        expr.obj = ExpressionObj::Rendered(quote! {
                            (#iterable).enumerate().map(|(i, x)| (i as u64, x))
                        });

                        Ok(Transformed::Expression(expr))
                    }),
                ),
            ),
            // filter((T) -> bool, Cast(<Iter>[T])) -> <Iter>[T]
            Self::Filter => Ty::new_function(
                vec![
                    (
                        "function",
                        Ty::new_function(
                            vec![
                                ("x", Ty::Anonymous(0), ParamType::Required)
                            ],
                            Ty::python(Self::Bool, vec![])
                        ),
                        ParamType::Required
                    ),
                    (
                        "iterable",
                        Ty::Cast(Ty::python(Self::Iter, vec![Ty::Anonymous(0)]).into()),
                        ParamType::Required,
                    )
                ],
                Ty::Transformed(
                    Ty::python(Self::Iter, vec![Ty::Anonymous(0)]).into(),
                    Transformation::new(|mut expr| {
                        let mut args = match1!(expr.obj, ExpressionObj::Call { args, .. } => args.into_iter());
                        let function = args.next().unwrap();
                        let iterable = args.next().unwrap();

                        let return_ty = match1!(&function.ty, Ty::Function(_, returns) => *returns.clone());

                        let elem = ExpressionObj::Rendered(quote! { elem.clone() });
                        let call = ExpressionObj::Call {
                            function: function.into(),
                            args: vec![elem.into()]
                        }.into();

                        let elem = match return_ty {
                            Ty::Transformed(_, transformation) => match (transformation.function)(call, &vec![].into())? {
                                Transformed::Expression(expression) => expression,
                                _ => {
                                    return Err(CoreError::make_raw(
                                        "can not filter using a special function",
                                        "Hint: this function causes an effect that needs compiler magic to work, and can't be called from a filter."
                                    ));
                                }
                            }
                            _ => call
                        };

                        expr.obj = ExpressionObj::Rendered(quote! {
                            #iterable.filter(|elem| #elem)
                        });

                        Ok(Transformed::Expression(expr))
                    })
                )
            ),
            // map((T) -> U, Cast(<Iter>[T])) -> <Iter>[U]
            Self::Map => Ty::new_function(
                vec![
                    (
                        "function",
                        Ty::new_function(
                            vec![
                                ("x", Ty::Anonymous(0), ParamType::Required)
                            ],
                            Ty::Anonymous(1)
                        ),
                        ParamType::Required
                    ),
                    (
                        "iterable",
                        Ty::Cast(Ty::python(Self::Iter, vec![Ty::Anonymous(0)]).into()),
                        ParamType::Required,
                    )
                ],
                Ty::Transformed(
                    Ty::python(Self::Iter, vec![Ty::Anonymous(1)]).into(),
                    Transformation::new(|mut expr| {
                        let mut args = match1!(expr.obj, ExpressionObj::Call { args, .. } => args.into_iter());
                        let function = args.next().unwrap();
                        let iterable = args.next().unwrap();

                        let return_ty = match1!(&function.ty, Ty::Function(_, returns) => *returns.clone());

                        let elem = ExpressionObj::Rendered(quote! { elem.clone() });
                        let call = ExpressionObj::Call {
                            function: function.into(),
                            args: vec![elem.into()]
                        }.into();

                        let elem = match return_ty {
                            Ty::Transformed(_, transformation) => match (transformation.function)(call, &vec![].into())? {
                                Transformed::Expression(expression) => expression,
                                _ => {
                                    return Err(CoreError::make_raw(
                                        "can not map using a special function",
                                        "Hint: this function causes an effect that needs compiler magic to work, and can't be called from a map."
                                    ));
                                }
                            }
                            _ => call
                        };

                        expr.obj = ExpressionObj::Rendered(quote! {
                            #iterable.map(|elem| #elem)
                        });

                        Ok(Transformed::Expression(expr))
                    })
                )
            ),
            // zip(Cast(<Iter>[T]), Cast(<Iter>[U])) -> <Iter>[(T, U)]
            // Unfortunately the type system doesn't allow for `zip`ing with variadic iterators,
            // like Python does
            // TODO could either make specialized prelude functions (zip3, zip4, etc.) to solve this
            // or do more compiler magic like in the array constructors
            Self::Zip => Ty::new_function(
                vec![
                    (
                        "iterable1",
                        Ty::Cast(Ty::python(Self::Iter, vec![Ty::Anonymous(0)]).into()),
                        ParamType::Required,
                    ),
                    (
                        "iterable2",
                        Ty::Cast(Ty::python(Self::Iter, vec![Ty::Anonymous(1)]).into()),
                        ParamType::Required,
                    )
                ],
                Ty::Transformed(
                    Ty::python(Self::Iter, vec![
                        Ty::python(Self::Tuple, vec![Ty::Anonymous(0), Ty::Anonymous(1)])
                    ]).into(),
                    Transformation::new(|mut expr| {
                        let mut args = match1!(expr.obj, ExpressionObj::Call { args, .. } => args.into_iter());
                        let iterable1 = args.next().unwrap();
                        let iterable2 = args.next().unwrap();

                        expr.obj = ExpressionObj::Rendered(quote! {
                            #iterable1.zip(#iterable2)
                        });

                        Ok(Transformed::Expression(expr))
                    })
                )
            ),
            // sorted(Cast(<Iter>[T])) -> List[T]
            // TODO support key, reverse
            Self::Sorted => Ty::new_function(
                vec![(
                    "iterable",
                    Ty::Cast(Ty::python(Self::Iter, vec![Ty::Anonymous(0)]).into()),
                    ParamType::Required,
                )],
                Ty::Transformed(
                    Ty::python(
                        Self::List,
                        vec![Ty::Anonymous(0)],
                    ).into(),
                    Transformation::new(|mut expr| {
                        let iterable = match1!(expr.obj, ExpressionObj::Call { args, .. } => args.into_iter().next().unwrap());

                        expr.obj = ExpressionObj::Rendered(quote! {
                            Mutable::new({
                                let mut temp = #iterable.collect::<Vec<_>>();
                                temp.sort();
                                temp
                            })
                        });

                        Ok(Transformed::Expression(expr))
                    }),
                ),
            ),
            // sum(Cast(<Iter>[T])) -> T
            Self::Sum => Ty::new_function(
                vec![(
                    "iterable",
                    Ty::Cast(Ty::python(Self::Iter, vec![Ty::Anonymous(0)]).into()),
                    ParamType::Required,
                )],
                Ty::Transformed(
                    Ty::Anonymous(0).into(),
                    Transformation::new(|mut expr| {
                        let iterable = match1!(expr.obj, ExpressionObj::Call { args, .. } => args.into_iter().next().unwrap());

                        let init = match &expr.ty {
                            Ty::Generic(TyName::Builtin(Builtin::Prelude(Prelude::RustInt(..))), _) => quote! { 0 },
                            Ty::IntParam(..) => quote! { 0 },
                            Ty::Generic(TyName::Builtin(Builtin::Prelude(Prelude::RustFloat)), _) => quote! { 0.0 },
                            ty => {
                                return Err(CoreError::make_raw(
                                    format!("cannot perform sum of type {}", ty),
                                    "Hint: sums can be performed on numeric types only."
                                ))
                            }
                        };

                        expr.obj = ExpressionObj::Rendered(quote! {
                            // Rolling my own sum implementation here because Rust's `Iterator.sum`
                            // requires type info which is annoying to obtain
                            #iterable.fold(#init, |accum, elem| accum + elem)
                        });

                        Ok(Transformed::Expression(expr))
                    })
                )
            ),
            // list(Cast(<Iter>[T])) -> List[T]
            Self::ListConstructor => Ty::new_function(
                vec![(
                    "iterable",
                    Ty::Cast(Ty::python(Self::Iter, vec![Ty::Anonymous(0)]).into()),
                    ParamType::Required,
                )],
                Ty::Transformed(
                    Ty::python(
                        Self::List,
                        vec![Ty::Anonymous(0)],
                    ).into(),
                    Transformation::new(|mut expr| {
                        let iterable = match1!(expr.obj, ExpressionObj::Call { args, .. } => args.into_iter().next().unwrap());

                        expr.obj = ExpressionObj::Rendered(quote! {
                            Mutable::new((#iterable).collect::<Vec<_>>())
                        });

                        Ok(Transformed::Expression(expr))
                    }),
                ),
            ),
            name => Ty::Type(TyName::Builtin(Builtin::Python(name.clone())), None),
        }
    }

    fn as_instance(&self, params: &Vec<Ty>) -> CResult<()> {
        match self {
            Self::List if params.len() == 1 => Ok(()),
            Self::Tuple => Ok(()),
            Self::None | Self::Int if params.len() == 0 => Ok(()),
            _ => Err(CoreError::make_raw("invalid type", "")),
        }
    }

    fn attr(&self, attr: &String) -> Option<(Ty, Ty)> {
        match (self, attr.as_str()) {
            // List[T].append(T) -> None
            (Self::List, "append") => Some((
                Ty::python(self.clone(), vec![Ty::Anonymous(0)]),
                Ty::new_function(
                    vec![("x", Ty::Anonymous(0), ParamType::Required)],
                    Ty::Transformed(
                        Ty::python(Self::Tuple, vec![]).into(),
                        Transformation::new(|mut expr| {
                            let (function, args) = match1!(expr.obj, ExpressionObj::Call { function, args } => (*function, args));
                            let value = match1!(function.obj, ExpressionObj::Attribute { value, .. } => *value);

                            expr.obj = ExpressionObj::Rendered(quote! {
                                #value.borrow_mut().push(#(#args),*)
                            });

                            Ok(Transformed::Expression(expr))
                        }),
                    ),
                ),
            )),
            // List[T].pop() -> T
            (Self::List, "pop") => Some((
                Ty::python(self.clone(), vec![Ty::Anonymous(0)]),
                Ty::new_function(
                    vec![],
                    Ty::Transformed(
                        Ty::Anonymous(0).into(),
                        Transformation::new(|mut expr| {
                            let function = match1!(expr.obj, ExpressionObj::Call { function, .. } => *function);
                            let value = match1!(function.obj, ExpressionObj::Attribute { value, .. } => *value);

                            expr.obj = ExpressionObj::Rendered(quote! {
                                #value.borrow_mut().pop().unwrap()
                            });

                            Ok(Transformed::Expression(expr))
                        }),
                    ),
                ),
            )),
            _ => None,
        }
    }

    fn index(&self) -> Option<(Ty, Ty)> {
        match self {
            // List[T].__index__(i128) -> T
            Self::List => Some((
                Ty::python(self.clone(), vec![Ty::Anonymous(0)]),
                Ty::new_function(
                    vec![
                        ("", Ty::Cast(Ty::prelude(Prelude::RustInt(true, 128), vec![]).into()), ParamType::Required)
                    ],
                    Ty::Transformed(
                        Ty::Anonymous(0).into(),
                        Transformation::new(|mut expr| {
                            let (value, index) = match1!(expr.obj, ExpressionObj::Index { value, index } => (*value, *index));
                            
                            if let ExpressionObj::BorrowMut(..) = &value.obj {
                                expr.obj = ExpressionObj::Rendered(quote! {
                                    (*#value.index_wrapped_mut(#index.into()))
                                });
                            } else {
                                expr.obj = ExpressionObj::Rendered(quote! {
                                    (*#value.index_wrapped(#index.into()))
                                });
                            }

                            Ok(Transformed::Expression(expr))
                        })
                    ).into()
                )
            )),
            _ => None
        }
    }

    fn static_attr(&self, attr: &String) -> Option<Ty> {
        None
    }

    fn casted(&self, ty: &Ty) -> Option<(Ty, Ty)> {
        let builtin = if let Ty::Generic(TyName::Builtin(builtin), _) = ty {
            builtin
        } else {
            return None;
        };

        match self {
            Self::List => match builtin {
                Builtin::Python(Self::Iter) => Some((
                    Ty::python(Self::List, vec![Ty::Anonymous(0)]),
                    Ty::Transformed(
                        Ty::python(Self::Iter, vec![Ty::Anonymous(0)]).into(),
                        Transformation::new(|mut expr| {
                            let list = expr.obj;

                            expr.obj = ExpressionObj::Rendered(quote! {
                                #list.borrow().iter().map(|elem| elem.clone())
                            });

                            Ok(Transformed::Expression(expr))
                        }),
                    ),
                )),
                Builtin::Python(Self::AsLen) => Some((
                    Ty::python(self.clone(), vec![Ty::Anonymous(0)]),
                    Ty::Transformed(
                        Ty::python(Self::AsLen, vec![]).into(),
                        Transformation::new(|mut expr| {
                            let list = expr.obj;

                            expr.obj = ExpressionObj::Rendered(quote! {
                                (#list.borrow().len() as u64)
                            });

                            Ok(Transformed::Expression(expr))
                        }),
                    ),
                )),
                Builtin::Prelude(Prelude::Seed) => Some((
                    Ty::python(
                        self.clone(),
                        vec![Ty::prelude(Prelude::RustInt(false, 8), vec![])],
                    ),
                    Ty::Transformed(
                        Ty::prelude(Prelude::Seed, vec![]).into(),
                        Transformation::new(|mut expr| {
                            let err = CoreError::make_raw(
                                "to use a list of bytes as a seed, the list must be known at compile-time",
                                ""
                            );

                            let list = match expr.obj {
                                ExpressionObj::Mutable(obj) => match obj.obj {
                                    ExpressionObj::Vec(list) => {
                                        list.into_iter().map(|element| element.without_borrows())
                                    }
                                    _ => {
                                        return Err(err);
                                    }
                                },
                                _ => {
                                    return Err(err);
                                }
                            };

                            expr.obj = ExpressionObj::Rendered(quote! {
                                [#(#list),*].as_ref()
                            });

                            Ok(Transformed::Expression(expr))
                        }),
                    ),
                )),
                _ => None,
            },
            Self::Str => match builtin {
                Builtin::Prelude(Prelude::Seed) => Some((
                    Ty::python(Python::Str, vec![]),
                    Ty::Transformed(
                        Ty::prelude(Prelude::Seed, vec![]).into(),
                        Transformation::new(|mut expr| {
                            // TODO maybe limit to only string literals?
                            let obj = expr.obj.without_borrows();
                            expr.obj = ExpressionObj::Rendered(quote! {
                                #obj.as_bytes().as_ref()
                            });

                            Ok(Transformed::Expression(expr))
                        }),
                    ),
                )),
                Builtin::Python(Self::AsLen) => Some((
                    Ty::python(self.clone(), vec![]),
                    Ty::Transformed(
                        Ty::python(Self::AsLen, vec![]).into(),
                        Transformation::new(|mut expr| {
                            let string = expr.obj;

                            expr.obj = ExpressionObj::Rendered(quote! {
                                (#string.chars().count() as u64)
                            });

                            Ok(Transformed::Expression(expr))
                        }),
                    ),
                )),
                _ => None,
            },
            Self::Iter => match builtin {
                Builtin::Python(Self::Iter) => Some((
                    Ty::python(Self::Iter, vec![Ty::Anonymous(0)]),
                    Ty::python(Self::Iter, vec![Ty::Anonymous(0)]),
                )),
                Builtin::Python(Self::AsLen) => Some((
                    Ty::python(self.clone(), vec![Ty::Anonymous(0)]),
                    Ty::Transformed(
                        Ty::python(Self::AsLen, vec![]).into(),
                        Transformation::new(|mut expr| {
                            let iterable = expr.obj;

                            expr.obj = ExpressionObj::Rendered(quote! {
                                (#iterable.count() as u64)
                            });

                            Ok(Transformed::Expression(expr))
                        }),
                    ),
                )),
                _ => None,
            },
            _ => None,
        }
    }
}
