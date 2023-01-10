pub mod ast;

use std::{
    convert::{Infallible, TryInto},
    rc::Rc,
};

use crate::core::{parse::ast as py, CoreError, Located, Location};
use ast::*;

enum Error {
    ImportAlias,
    ClassDefWithKeywords,
    InvalidConstant,
    NonconstantConstant,
    Async,
    ArbitraryTopLevelStatementObj,
    ArbitraryClassDefStatement,
    ArbitraryTyExpression,
    ArbitraryParams,
    ParamWithoutType,
    CompareChain,
    BigInteger,
    ExpressionComplexNumber,
    ComprehensionNotList,
    ExpressionYield,
    ExpressionDict,
    ExpressionSet,
    ExpressionStarred,
    ExpressionSlice,
    ExpressionBytes,
    ExpressionLambda,
    ExpressionNamed,
    ExpressionEllipsis,
    FStrWithSpec,
    WhileWithOrelse,
    ForWithOrelse,
    StatementImport,
    StatementDelete,
    StatementGlobal,
    StatementNonlocal,
    StatementWith,
    StatementRaise,
    StatementTry,
    StatementClassDef,
    StatementFunctionDef,
    OperatorMatMult,
}

impl Error {
    fn core(self, loc: Location) -> CoreError {
        self.partial().located(loc)
    }

    fn partial(self) -> CoreError {
        match self {
            Self::ImportAlias => CoreError::make_raw("imports aliases are currently not supported", ""),
            Self::ClassDefWithKeywords => CoreError::make_raw("class definition with keywords", ""),
            Self::InvalidConstant => CoreError::make_raw(
                "invalid constant",
                concat!(
                    "Hint: constants may be defined like this:\n\n",
                    "\tMY_CONST = ..."
                )
            ),
            Self::NonconstantConstant => CoreError::make_raw(
                "potentiall non-constant constant",
                concat!(
                    "Help: constants may only be made of simple expressions that are guaranteed to be constant.\n",
                    "This excludes anything that calls a function (including constructors) or creates a mutable data type."
                )
            ),
            Self::Async => CoreError::make_raw("functions may not be async", ""),
            Self::ArbitraryTopLevelStatementObj => CoreError::make_raw(
                "arbitrary top-level statement",
                "Help: top-level statements may only be imports, class defs, and function defs.",
            ),
            Self::ArbitraryClassDefStatement => CoreError::make_raw(
                "arbitrary class def statement",
                "Help: class def statements may only be type-annotated fields.",
            ),
            Self::ArbitraryTyExpression => CoreError::make_raw(
                "arbitrary type expression",
                "Help: type expressions may only be identifiers, namespaced identifiers, generics, or positive constant integers."
            ),
            Self::ArbitraryParams => CoreError::make_raw(
                "arbitrary params",
                "Help: function params may only be type-annotated positional arguments."
            ),
            Self::ParamWithoutType => CoreError::make_raw(
                "param without type",
                "Help: all function params must have a type annotation."
            ),
            Self::CompareChain => CoreError::make_raw(
                "comparison chains are unsupported",
                "Help: you can convert this chain into several individual comparisons that are `and`'d together."
            ),
            Self::BigInteger => CoreError::make_raw(
                "integer too large",
                "Help: for compilation purposes, integers need to be able to fit into a Rust i128 (roughly the range [-1.7e38, 1.7e38])"
            ),
            Self::ExpressionComplexNumber => CoreError::make_raw(
                "complex numbers are not supported",
                ""
            ),
            Self::ComprehensionNotList => CoreError::make_raw(
                "non-list comprehensions are not supported",
                ""
            ),
            Self::ExpressionYield => CoreError::make_raw(
                "yield expressions are not supported",
                ""
            ),
            Self::ExpressionDict => CoreError::make_raw(
                "dicts are not supported",
                ""
            ),
            Self::ExpressionSet => CoreError::make_raw(
                "sets are not supported",
                ""
            ),
            Self::ExpressionStarred => CoreError::make_raw(
                "unpack expressions are not supported",
                ""
            ),
            Self::ExpressionSlice => CoreError::make_raw(
                "slices are not supported",
                ""
            ),
            Self::ExpressionBytes => CoreError::make_raw(
                "byte-strings are not supported",
                ""
            ),
            Self::ExpressionLambda => CoreError::make_raw(
                "lambda functions are not supported",
                ""
            ),
            Self::ExpressionNamed => CoreError::make_raw(
                "named expressions are not supported",
                ""
            ),
            Self::ExpressionEllipsis => CoreError::make_raw(
                "ellipsis expressions are not supported",
                ""
            ),
            Self::FStrWithSpec => CoreError::make_raw(
                "f-string formatting options are not supported",
                ""
            ),
            Self::WhileWithOrelse => CoreError::make_raw(
                "`while` statements with else blocks are not supported",
                ""
            ),
            Self::ForWithOrelse => CoreError::make_raw(
                "`for` statements with else blocks are not supported",
                ""
            ),
            Self::StatementImport => CoreError::make_raw(
                "import statements must be top-level",
                "Help: try moving this definition to outside of this function."
            ),
            Self::StatementDelete => CoreError::make_raw(
                "`del` statements are unsupported",
                ""
            ),
            Self::StatementGlobal => CoreError::make_raw(
                "`global` statements are unsupported",
                ""
            ),
            Self::StatementNonlocal => CoreError::make_raw(
                "`nonlocal` statements are unsupported",
                ""
            ),
            Self::StatementWith => CoreError::make_raw(
                "`with` statements are unsupported",
                ""
            ),
            Self::StatementRaise => CoreError::make_raw(
                "`raise` statements are unsupported",
                ""
            ),
            Self::StatementTry => CoreError::make_raw(
                "`try` statements are unsupported",
                ""
            ),
            Self::StatementClassDef => CoreError::make_raw(
                "class def statements must be top-level",
                "Help: try moving this definition to outside of this function."
            ),
            Self::StatementFunctionDef => CoreError::make_raw(
                "function def statements must be top-level",
                "Help: try moving this definition to outside of this function."
            ),
            Self::OperatorMatMult => CoreError::make_raw(
                "matrix multiplication is unsupported",
                ""
            ),
        }
    }
}

struct WithSrc<T> {
    src: Rc<String>,
    obj: T,
}

impl<T> WithSrc<T> {
    fn new(src: &Rc<String>, obj: T) -> Self {
        Self {
            src: src.clone(),
            obj,
        }
    }
}

impl TryInto<Module> for WithSrc<py::Program> {
    type Error = CoreError;

    fn try_into(self) -> Result<Module, Self::Error> {
        let WithSrc { src, obj } = self;

        let statements = obj
            .statements
            .into_iter()
            .map(|statement| WithSrc::new(&src, statement).try_into())
            .collect::<Result<Vec<TopLevelStatement>, CoreError>>()?;
        Ok(Module { statements })
    }
}

impl TryInto<TopLevelStatement> for WithSrc<py::Statement> {
    type Error = CoreError;

    fn try_into(self) -> Result<TopLevelStatement, Self::Error> {
        let WithSrc {
            src,
            obj: py::Located { location, node },
        } = self;
        let location = Location::new(&src, location);

        match node {
            // TODO importing something nested (like import seahorse.prelude) should get changed
            // into something else syntactically
            py::StatementType::Import { names } => Ok(TopLevelStatementObj::Import {
                symbols: names
                    .into_iter()
                    .map(|py::ImportSymbol { symbol, alias }| match alias {
                        None => Ok(ImportSymbol {
                            symbol,
                            alias: None,
                        }),
                        _ => Err(Error::ImportAlias.core(location.clone())),
                    })
                    .collect::<Result<_, _>>()?,
            }),
            py::StatementType::ImportFrom {
                level,
                module,
                names,
            } => Ok(TopLevelStatementObj::ImportFrom {
                level,
                path: match module {
                    None => vec![],
                    Some(path) => path.split('.').map(String::from).collect(),
                },
                symbols: names
                    .into_iter()
                    .map(|py::ImportSymbol { symbol, alias }| match alias {
                        None => Ok(ImportSymbol {
                            symbol,
                            alias: None,
                        }),
                        _ => Err(Error::ImportAlias.core(location.clone())),
                    })
                    .collect::<Result<_, _>>()?,
            }),
            py::StatementType::Assign { targets, value } => {
                if targets.len() != 1 {
                    Err(Error::InvalidConstant)
                } else {
                    let target =
                        WithSrc::new(&src, targets.into_iter().next().unwrap()).try_into()?;

                    if !validate_constant(&value) {
                        Err(Error::NonconstantConstant)
                    } else if let Located(_, ExpressionObj::Id(name)) = target {
                        Ok(TopLevelStatementObj::Constant {
                            name,
                            value: WithSrc::new(&src, value).try_into()?,
                        })
                    } else {
                        Err(Error::InvalidConstant)
                    }
                }
            }
            py::StatementType::AnnAssign { .. } => Err(Error::InvalidConstant),
            py::StatementType::ClassDef {
                name,
                body,
                bases,
                keywords,
                decorator_list,
            } => {
                if keywords.len() > 0 {
                    Err(Error::ClassDefWithKeywords)
                } else {
                    Ok(TopLevelStatementObj::ClassDef {
                        name,
                        body: body
                            .into_iter()
                            .map(|statement| WithSrc::new(&src, statement).try_into())
                            .collect::<Result<Vec<_>, CoreError>>()?,
                        bases: bases
                            .into_iter()
                            .map(|base| WithSrc::new(&src, base).try_into())
                            .collect::<Result<Vec<_>, CoreError>>()?,
                        decorator_list: decorator_list
                            .into_iter()
                            .map(|dec| WithSrc::new(&src, dec).try_into())
                            .collect::<Result<Vec<_>, CoreError>>()?,
                    })
                }
            }
            py::StatementType::FunctionDef {
                is_async,
                name,
                args,
                body,
                decorator_list,
                returns,
            } => {
                if is_async {
                    Err(Error::Async)
                } else {
                    Ok(TopLevelStatementObj::FunctionDef(FunctionDef {
                        name,
                        params: WithSrc::new(&src, *args)
                            .try_into()
                            .map_err(|err: CoreError| err.updated(&location))?,
                        body: body
                            .into_iter()
                            .map(|statement| WithSrc::new(&src, statement).try_into())
                            .collect::<Result<Vec<_>, CoreError>>()?,
                        decorator_list: decorator_list
                            .into_iter()
                            .map(|decorator| WithSrc::new(&src, decorator).try_into())
                            .collect::<Result<Vec<_>, CoreError>>()?,
                        returns: returns
                            .map(|returns| WithSrc::new(&src, returns).try_into())
                            .transpose()?,
                    }))
                }
            }
            py::StatementType::Expression { expression } => Ok(TopLevelStatementObj::Expression(
                WithSrc::new(&src, expression).try_into()?,
            )),
            _ => Err(Error::ArbitraryTopLevelStatementObj),
        }
        .map(|ok| Located(location.clone(), ok))
        .map_err(|err| err.core(location))
    }
}

fn validate_constant(constant: &py::Expression) -> bool {
    match &constant.node {
        py::ExpressionType::Number { .. }
        | py::ExpressionType::String { .. }
        | py::ExpressionType::Bytes { .. } // Doesn't exist yet, just future-proofing
        | py::ExpressionType::False
        | py::ExpressionType::True
        | py::ExpressionType::None
        | py::ExpressionType::Identifier { .. } => true,
        py::ExpressionType::Attribute { value, .. } => validate_constant(&*value),
        py::ExpressionType::Subscript { a, b } => (
            validate_constant(&*a) && validate_constant(&*b)
        ),
        py::ExpressionType::Binop { a, b, .. } => (
            validate_constant(&*a) && validate_constant(&*b)
        ),
        py::ExpressionType::Unop { a, .. } => validate_constant(&*a),
        py::ExpressionType::Compare { vals, .. } => vals.iter().all(|val| validate_constant(&*val)),
        py::ExpressionType::Tuple { elements } => elements.iter().all(|element| validate_constant(&*element)),
        _ => false
    }
}

impl TryInto<ClassDefStatement> for WithSrc<py::Statement> {
    type Error = CoreError;

    fn try_into(self) -> Result<ClassDefStatement, Self::Error> {
        let WithSrc {
            src,
            obj: py::Located { location, node },
        } = self;
        let location = Location::new(&src, location);

        match node {
            py::StatementType::Assign { mut targets, value } => {
                if targets.len() == 1 {
                    let Located(_, target) =
                        WithSrc::new(&src, targets.pop().unwrap()).try_into()?;

                    match target {
                        ExpressionObj::Id(name) => Ok(ClassDefStatementObj::FieldDef {
                            name,
                            ty: None,
                            value: Some(WithSrc::new(&src, value).try_into()?),
                        }),
                        _ => Err(Error::ArbitraryClassDefStatement),
                    }
                } else {
                    Err(Error::ArbitraryClassDefStatement)
                }
            }
            py::StatementType::AnnAssign {
                target,
                annotation,
                value,
            } => match target.node {
                py::ExpressionType::Identifier { name } => Ok(ClassDefStatementObj::FieldDef {
                    name,
                    ty: Some(WithSrc::new(&src, *annotation).try_into()?),
                    value: value
                        .map(|value| WithSrc::new(&src, value).try_into())
                        .transpose()?,
                }),
                _ => Err(Error::ArbitraryClassDefStatement),
            },
            py::StatementType::FunctionDef {
                is_async,
                name,
                args,
                body,
                decorator_list,
                returns,
            } => {
                if is_async {
                    Err(Error::Async)
                } else {
                    Ok(ClassDefStatementObj::MethodDef(FunctionDef {
                        name,
                        params: WithSrc::new(&src, *args)
                            .try_into()
                            .map_err(|err: CoreError| err.updated(&location))?,
                        body: body
                            .into_iter()
                            .map(|statement| WithSrc::new(&src, statement).try_into())
                            .collect::<Result<Vec<_>, CoreError>>()?,
                        decorator_list: decorator_list
                            .into_iter()
                            .map(|decorator| WithSrc::new(&src, decorator).try_into())
                            .collect::<Result<Vec<_>, CoreError>>()?,
                        returns: returns
                            .map(|returns| WithSrc::new(&src, returns).try_into())
                            .transpose()?,
                    }))
                }
            }
            _ => Err(Error::ArbitraryClassDefStatement),
        }
        .map(|ok| Located(location.clone(), ok))
        .map_err(|err| err.core(location))
    }
}

impl TryInto<Params> for WithSrc<py::Parameters> {
    type Error = CoreError;

    fn try_into(self) -> Result<Params, Self::Error> {
        let WithSrc { src, obj } = self;

        if obj.posonlyargs_count > 0
            || obj.kwonlyargs.len() > 0
            || obj.vararg != py::Varargs::None
            || obj.kwarg != py::Varargs::None
            || obj.defaults.len() > 0
            || obj.kw_defaults.len() > 0
        {
            Err(Error::ArbitraryParams.partial())
        } else {
            let mut is_instance_method = false;
            if let Some(arg) = obj.args.get(0) {
                if arg.annotation == None && &arg.arg == "self" {
                    is_instance_method = true;
                }
            }

            Ok(Params {
                is_instance_method,
                params: obj
                    .args
                    .into_iter()
                    .skip(if is_instance_method { 1 } else { 0 })
                    .map(|arg| WithSrc::new(&src, arg).try_into())
                    .collect::<Result<_, CoreError>>()?,
            })
        }
    }
}

impl TryInto<Param> for WithSrc<py::Parameter> {
    type Error = CoreError;

    fn try_into(self) -> Result<Param, Self::Error> {
        let WithSrc { src, obj } = self;
        let location = Location::new(&src, obj.location);

        if let Some(annotation) = obj.annotation {
            Ok(Located(
                location,
                ParamObj {
                    arg: obj.arg,
                    annotation: WithSrc::new(&src, *annotation).try_into()?,
                },
            ))
        } else {
            Err(Error::ParamWithoutType.core(location))
        }
    }
}

impl TryInto<Expression> for WithSrc<py::Expression> {
    type Error = CoreError;

    fn try_into(self) -> Result<Expression, Self::Error> {
        let WithSrc {
            src,
            obj: py::Located { location, node },
        } = self;
        let location = Location::new(&src, location);

        match node {
            py::ExpressionType::BoolOp { op, values } => {
                // Converts a boolean chain into a binop tree
                let op: Operator = op.try_into().unwrap();

                let mut values = values.into_iter();
                let first: Expression = WithSrc::new(&src, values.next().unwrap()).try_into()?;
                let mut res = Located(
                    first.0.clone(),
                    ExpressionObj::BinOp {
                        left: Box::new(first),
                        op: op.clone(),
                        right: Box::new(WithSrc::new(&src, values.next().unwrap()).try_into()?),
                    },
                );

                for value in values {
                    let value: Expression = WithSrc::new(&src, value).try_into()?;
                    res = Located(
                        value.0.clone(),
                        ExpressionObj::BinOp {
                            left: Box::new(res),
                            op: op.clone(),
                            right: Box::new(value),
                        },
                    );
                }

                Ok(res.1)
            }
            py::ExpressionType::Binop { a, op, b } => Ok(ExpressionObj::BinOp {
                left: Box::new(WithSrc::new(&src, *a).try_into()?),
                op: op
                    .try_into()
                    .map_err(|err: CoreError| err.updated(&location))?,
                right: Box::new(WithSrc::new(&src, *b).try_into()?),
            }),
            py::ExpressionType::Subscript { a, b } => Ok(ExpressionObj::Index {
                value: Box::new(WithSrc::new(&src, *a).try_into()?),
                index: Box::new(WithSrc::new(&src, *b).try_into()?),
            }),
            py::ExpressionType::Unop { op, a } => Ok(ExpressionObj::UnOp {
                op: op.try_into().unwrap(),
                value: Box::new(WithSrc::new(&src, *a).try_into()?),
            }),
            py::ExpressionType::Compare { vals, ops } => {
                if ops.len() == 1 {
                    let mut vals = vals.into_iter();
                    let mut ops = ops.into_iter();
                    Ok(ExpressionObj::BinOp {
                        left: Box::new(WithSrc::new(&src, vals.next().unwrap()).try_into()?),
                        op: ops.next().unwrap().try_into().unwrap(),
                        right: Box::new(WithSrc::new(&src, vals.next().unwrap()).try_into()?),
                    })
                } else {
                    Err(Error::CompareChain)
                }
            }
            py::ExpressionType::Attribute { value, name } => Ok(ExpressionObj::Attribute {
                value: Box::new(WithSrc::new(&src, *value).try_into()?),
                name,
            }),
            py::ExpressionType::Call {
                function,
                args,
                keywords,
            } => Ok(ExpressionObj::Call {
                function: Box::new(WithSrc::new(&src, *function).try_into()?),
                args: WithSrc::new(&src, (args, keywords)).try_into()?,
            }),
            py::ExpressionType::Number { value } => match value {
                py::Number::Integer { value } => value
                    .to_str_radix(10)
                    .parse()
                    .map(ExpressionObj::Int)
                    .map_err(|_| Error::BigInteger),
                py::Number::Float { value } => Ok(ExpressionObj::Float(value)),
                py::Number::Complex { .. } => Err(Error::ExpressionComplexNumber),
            },
            py::ExpressionType::List { elements } => Ok(ExpressionObj::List(
                elements
                    .into_iter()
                    .map(|element| WithSrc::new(&src, element).try_into())
                    .collect::<Result<Vec<_>, CoreError>>()?,
            )),
            py::ExpressionType::Tuple { elements } => Ok(ExpressionObj::Tuple(
                elements
                    .into_iter()
                    .map(|element| WithSrc::new(&src, element).try_into())
                    .collect::<Result<Vec<_>, CoreError>>()?,
            )),
            py::ExpressionType::Comprehension { kind, generators } => match *kind {
                py::ComprehensionKind::List { element } => Ok(ExpressionObj::Comprehension {
                    element: Box::new(WithSrc::new(&src, element).try_into()?),
                    parts: {
                        let mut parts = vec![];

                        for generator in generators.into_iter() {
                            let py::Comprehension {
                                location,
                                target,
                                iter,
                                ifs,
                                is_async,
                            } = generator;
                            let location = Location::new(&src, location);

                            if is_async {
                                return Err(Error::Async.core(location));
                            }

                            parts.push(ComprehensionPart::For {
                                target: WithSrc::new(&src, target).try_into()?,
                                iter: {
                                    let iter: Expression = WithSrc::new(&src, iter).try_into()?;
                                    Located(
                                        iter.0.clone(),
                                        ExpressionObj::Iter { value: iter.into() },
                                    )
                                },
                            });

                            parts.append(
                                &mut ifs
                                    .into_iter()
                                    .map(|cond| {
                                        Ok(ComprehensionPart::If {
                                            cond: WithSrc::new(&src, cond).try_into()?,
                                        })
                                    })
                                    .collect::<Result<Vec<_>, CoreError>>()?,
                            );
                        }

                        parts
                    },
                }),
                _ => Err(Error::ComprehensionNotList),
            },
            py::ExpressionType::String { value } => match value {
                py::StringGroup::Constant { value } => Ok(ExpressionObj::Str(value)),
                py::StringGroup::Joined { values } => Ok(ExpressionObj::FStr {
                    parts: values
                        .into_iter()
                        .map(|part| WithSrc::new(&src, part).try_into())
                        .collect::<Result<Vec<_>, CoreError>>()?,
                }),
                _ => panic!("Encountered an unexpected syntax element"),
            },
            py::ExpressionType::Identifier { name } => Ok(ExpressionObj::Id(name)),
            py::ExpressionType::True => Ok(ExpressionObj::Bool(true)),
            py::ExpressionType::False => Ok(ExpressionObj::Bool(false)),
            py::ExpressionType::None => Ok(ExpressionObj::None),
            py::ExpressionType::Await { .. } => Err(Error::Async),
            py::ExpressionType::Yield { .. } => Err(Error::ExpressionYield),
            py::ExpressionType::YieldFrom { .. } => Err(Error::ExpressionYield),
            py::ExpressionType::Dict { .. } => Err(Error::ExpressionDict),
            py::ExpressionType::Set { .. } => Err(Error::ExpressionSet),
            py::ExpressionType::Starred { .. } => Err(Error::ExpressionStarred),
            py::ExpressionType::Slice { .. } => Err(Error::ExpressionSlice),
            py::ExpressionType::Bytes { .. } => Err(Error::ExpressionBytes),
            py::ExpressionType::Lambda { .. } => Err(Error::ExpressionLambda),
            py::ExpressionType::IfExpression { test, body, orelse } => Ok(ExpressionObj::Ternary {
                test: Box::new(WithSrc::new(&src, *test).try_into()?),
                body: Box::new(WithSrc::new(&src, *body).try_into()?),
                orelse: Box::new(WithSrc::new(&src, *orelse).try_into()?),
            }),
            py::ExpressionType::NamedExpression { .. } => Err(Error::ExpressionNamed),
            py::ExpressionType::Ellipsis => Err(Error::ExpressionEllipsis),
        }
        .map(|ok| Located(location.clone(), ok))
        .map_err(|err| err.core(location))
    }
}

impl TryInto<Args> for WithSrc<(Vec<py::Expression>, Vec<py::Keyword>)> {
    type Error = CoreError;

    fn try_into(self) -> Result<Args, Self::Error> {
        let WithSrc {
            src,
            obj: (pos, kw),
        } = self;

        Ok(Args {
            pos: pos
                .into_iter()
                .map(|arg| WithSrc::new(&src, arg).try_into())
                .collect::<Result<Vec<_>, CoreError>>()?,
            kw: kw
                .into_iter()
                .map(
                    |py::Keyword { name, value }| match WithSrc::new(&src, value).try_into() {
                        Ok(value) => Ok((name.unwrap(), value)),
                        Err(err) => Err(err),
                    },
                )
                .collect::<Result<Vec<_>, CoreError>>()?,
        })
    }
}

impl TryInto<FStrPart> for WithSrc<py::StringGroup> {
    type Error = CoreError;

    fn try_into(self) -> Result<FStrPart, Self::Error> {
        let WithSrc { src, obj } = self;

        match obj {
            py::StringGroup::Constant { value } => Ok(FStrPart::Str(value)),
            py::StringGroup::FormattedValue {
                value,
                conversion,
                spec,
            } => {
                let location = Location::new(&src, value.location);
                if conversion.is_some() || spec.is_some() {
                    Err(Error::FStrWithSpec.core(location))
                } else {
                    Ok(FStrPart::ExpressionObj(
                        WithSrc::new(&src, *value).try_into()?,
                    ))
                }
            }
            _ => panic!("Encountered an unexpected syntax element"),
        }
    }
}

impl TryInto<TyExpression> for WithSrc<py::Expression> {
    type Error = CoreError;

    fn try_into(self) -> Result<TyExpression, Self::Error> {
        let WithSrc {
            src,
            obj: py::Located { location, node },
        } = self;
        let location = Location::new(&src, location);

        match node {
            py::ExpressionType::Subscript { a, b } => {
                let Located(_, base) = WithSrc::new(&src, *a).try_into()?;
                let base = match base {
                    TyExpressionObj::Generic { base, params } if params.len() == 0 => base,
                    _ => {
                        return Err(Error::ArbitraryTyExpression.core(location));
                    }
                };

                Ok(TyExpressionObj::Generic {
                    base,
                    params: match *b {
                        py::Located {
                            node: py::ExpressionType::Tuple { elements },
                            ..
                        } => elements,
                        expression => vec![expression],
                    }
                    .into_iter()
                    .map(|param| WithSrc::new(&src, param).try_into())
                    .collect::<Result<_, CoreError>>()?,
                })
            }
            py::ExpressionType::Attribute { value, name } => {
                let Located(_, base) = WithSrc::new(&src, *value).try_into()?;
                match base {
                    TyExpressionObj::Generic { mut base, params } => {
                        base.push(name);
                        Ok(TyExpressionObj::Generic { base, params })
                    }
                    _ => Err(Error::ArbitraryTyExpression),
                }
            }
            py::ExpressionType::Identifier { name } => Ok(TyExpressionObj::Generic {
                base: vec![name],
                params: vec![],
            }),
            py::ExpressionType::Number {
                value: py::Number::Integer { value },
            } => Ok(TyExpressionObj::Const(
                value
                    .to_str_radix(10)
                    .parse()
                    .map_err(|_| Error::ArbitraryTyExpression.core(location.clone()))?,
            )),
            _ => Err(Error::ArbitraryTyExpression),
        }
        .map(|ok| Located(location.clone(), ok))
        .map_err(|err| err.core(location))
    }
}

impl TryInto<Statement> for WithSrc<py::Statement> {
    type Error = CoreError;

    fn try_into(self) -> Result<Statement, Self::Error> {
        let WithSrc {
            src,
            obj: py::Located { location, node },
        } = self;
        let location = Location::new(&src, location);

        match node {
            py::StatementType::Break => Ok(StatementObj::Break),
            py::StatementType::Continue => Ok(StatementObj::Continue),
            py::StatementType::Return { value } => Ok(StatementObj::Return {
                value: value
                    .map(|value| WithSrc::new(&src, value).try_into())
                    .transpose()?,
            }),
            py::StatementType::Pass => Ok(StatementObj::Pass),
            py::StatementType::Assert { test, msg } => Ok(StatementObj::Assert {
                test: WithSrc::new(&src, test).try_into()?,
                msg: msg
                    .map(|msg| WithSrc::new(&src, msg).try_into())
                    .transpose()?,
            }),
            py::StatementType::Assign { mut targets, value } => Ok(StatementObj::Assign {
                target: if targets.len() == 1 {
                    WithSrc::new(&src, targets.pop().unwrap()).try_into()?
                } else {
                    Located(
                        location.clone(),
                        ExpressionObj::Tuple(
                            targets
                                .into_iter()
                                .map(|target| WithSrc::new(&src, target).try_into())
                                .collect::<Result<Vec<_>, CoreError>>()?,
                        ),
                    )
                },
                value: WithSrc::new(&src, value).try_into()?,
            }),
            py::StatementType::AugAssign { target, op, value } => Ok(StatementObj::OpAssign {
                target: WithSrc::new(&src, *target).try_into()?,
                op: op
                    .try_into()
                    .map_err(|err: CoreError| err.updated(&location))?,
                value: WithSrc::new(&src, *value).try_into()?,
            }),
            py::StatementType::AnnAssign {
                target,
                annotation,
                value,
            } => Ok(StatementObj::TyAssign {
                target: WithSrc::new(&src, *target).try_into()?,
                ty: WithSrc::new(&src, *annotation).try_into()?,
                value: value
                    .map(|value| WithSrc::new(&src, value).try_into())
                    .transpose()?,
            }),
            py::StatementType::Expression { expression } => Ok(StatementObj::ExpressionObj {
                expression: WithSrc::new(&src, expression).try_into()?,
            }),
            py::StatementType::If { test, body, orelse } => Ok(StatementObj::If {
                test: WithSrc::new(&src, test).try_into()?,
                body: body
                    .into_iter()
                    .map(|statement| WithSrc::new(&src, statement).try_into())
                    .collect::<Result<_, CoreError>>()?,
                orelse: orelse
                    .map(|orelse| {
                        orelse
                            .into_iter()
                            .map(|statement| WithSrc::new(&src, statement).try_into())
                            .collect::<Result<_, CoreError>>()
                    })
                    .transpose()?,
            }),
            py::StatementType::While { test, body, orelse } => match orelse {
                None => Ok(StatementObj::While {
                    test: WithSrc::new(&src, test).try_into()?,
                    body: body
                        .into_iter()
                        .map(|statement| WithSrc::new(&src, statement).try_into())
                        .collect::<Result<_, CoreError>>()?,
                }),
                _ => Err(Error::WhileWithOrelse),
            },
            py::StatementType::For {
                is_async,
                target,
                iter,
                body,
                orelse,
            } => {
                if is_async {
                    Err(Error::Async)
                } else if orelse.is_some() {
                    Err(Error::ForWithOrelse)
                } else {
                    Ok(StatementObj::For {
                        target: WithSrc::new(&src, *target).try_into()?,
                        iter: {
                            let iter: Expression = WithSrc::new(&src, *iter).try_into()?;
                            Located(iter.0.clone(), ExpressionObj::Iter { value: iter.into() })
                        },
                        body: body
                            .into_iter()
                            .map(|statement| WithSrc::new(&src, statement).try_into())
                            .collect::<Result<_, CoreError>>()?,
                    })
                }
            }
            py::StatementType::Import { .. } | py::StatementType::ImportFrom { .. } => {
                Err(Error::StatementImport)
            }
            py::StatementType::Delete { .. } => Err(Error::StatementDelete),
            py::StatementType::Global { .. } => Err(Error::StatementGlobal),
            py::StatementType::Nonlocal { .. } => Err(Error::StatementNonlocal),
            py::StatementType::With { .. } => Err(Error::StatementWith),
            py::StatementType::Raise { .. } => Err(Error::StatementRaise),
            py::StatementType::Try { .. } => Err(Error::StatementTry),
            py::StatementType::ClassDef { .. } => Err(Error::StatementClassDef),
            py::StatementType::FunctionDef { .. } => Err(Error::StatementFunctionDef),
        }
        .map(|ok| Located(location.clone(), ok))
        .map_err(|err| err.core(location))
    }
}

impl TryInto<Operator> for py::Operator {
    type Error = CoreError;

    fn try_into(self) -> Result<Operator, Self::Error> {
        Ok(match self {
            Self::Add => Operator::Add,
            Self::Sub => Operator::Sub,
            Self::Mult => Operator::Mul,
            Self::Div => Operator::Div,
            Self::Mod => Operator::Mod,
            Self::Pow => Operator::Pow,
            Self::LShift => Operator::LShift,
            Self::RShift => Operator::RShift,
            Self::BitOr => Operator::BitOr,
            Self::BitXor => Operator::BitXor,
            Self::BitAnd => Operator::BitAnd,
            Self::FloorDiv => Operator::FloorDiv,
            Self::MatMult => return Err(Error::OperatorMatMult.partial()),
        })
    }
}

impl TryInto<Operator> for py::BooleanOperator {
    type Error = Infallible;

    fn try_into(self) -> Result<Operator, Self::Error> {
        Ok(match self {
            Self::And => Operator::And,
            Self::Or => Operator::Or,
        })
    }
}

impl TryInto<Operator> for py::Comparison {
    type Error = Infallible;

    fn try_into(self) -> Result<Operator, Self::Error> {
        Ok(match self {
            Self::Equal => Operator::Eq,
            Self::NotEqual => Operator::NotEq,
            Self::Less => Operator::Lt,
            Self::LessOrEqual => Operator::Lte,
            Self::Greater => Operator::Gt,
            Self::GreaterOrEqual => Operator::Gte,
            Self::In => Operator::In,
            Self::NotIn => Operator::NotIn,
            Self::Is => Operator::Eq,
            Self::IsNot => Operator::NotEq,
        })
    }
}

impl TryInto<UnaryOperator> for py::UnaryOperator {
    type Error = Infallible;

    fn try_into(self) -> Result<UnaryOperator, Self::Error> {
        Ok(match self {
            Self::Pos => UnaryOperator::Pos,
            Self::Neg => UnaryOperator::Neg,
            Self::Not => UnaryOperator::Not,
            Self::Inv => UnaryOperator::Inv,
        })
    }
}

pub fn clean(program: py::Program, source: String) -> Result<Module, CoreError> {
    WithSrc::new(&Rc::new(source), program).try_into()
}
