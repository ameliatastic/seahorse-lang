pub mod ast;

use std::convert::TryInto;

use crate::core::{parse::ast as py, CoreError, Located};
use ast::*;

enum Error {
    ClassDefWithKeywords,
    ClassDefWithDecorators,
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
    ExpressionIf,
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
    fn core(self) -> CoreError {
        match self {
            Self::ClassDefWithKeywords => CoreError::make_raw("class definition with keywords", ""),
            Self::ClassDefWithDecorators => CoreError::make_raw("class definition with decorators", ""),
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
            Self::ExpressionIf => CoreError::make_raw(
                "ternaries are not supported",
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

impl TryInto<Module> for py::Program {
    type Error = CoreError;

    fn try_into(self) -> Result<Module, Self::Error> {
        let statements = self
            .statements
            .into_iter()
            .map(|statement| statement.try_into())
            .collect::<Result<Vec<TopLevelStatement>, CoreError>>()?;
        Ok(Module { statements })
    }
}

impl TryInto<TopLevelStatement> for py::Statement {
    type Error = CoreError;

    fn try_into(self) -> Result<TopLevelStatement, Self::Error> {
        let location = self.location;
        match self.node {
            // TODO importing something nested (like import seahorse.prelude) should get changed
            // into something else syntactically
            py::StatementType::Import { names } => Ok(TopLevelStatementObj::Import {
                symbols: names
                    .into_iter()
                    .map(|py::ImportSymbol { symbol, alias }| ImportSymbol { symbol, alias })
                    .collect(),
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
                    .map(|py::ImportSymbol { symbol, alias }| ImportSymbol { symbol, alias })
                    .collect(),
            }),
            py::StatementType::ClassDef {
                name,
                body,
                bases,
                keywords,
                decorator_list,
            } => {
                if keywords.len() > 0 {
                    Err(Error::ClassDefWithKeywords)
                } else if decorator_list.len() > 0 {
                    Err(Error::ClassDefWithDecorators)
                } else {
                    Ok(TopLevelStatementObj::ClassDef {
                        name,
                        body: body
                            .into_iter()
                            .map(|statement| statement.try_into())
                            .collect::<Result<Vec<_>, CoreError>>()?,
                        bases: bases
                            .into_iter()
                            .map(|base| base.try_into())
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
                        params: (*args)
                            .try_into()
                            .map_err(|err: CoreError| err.located(location))?,
                        body: body
                            .into_iter()
                            .map(|statement| statement.try_into())
                            .collect::<Result<Vec<_>, CoreError>>()?,
                        decorator_list: decorator_list
                            .into_iter()
                            .map(|decorator| decorator.try_into())
                            .collect::<Result<Vec<_>, CoreError>>()?,
                        returns: returns.map(|returns| returns.try_into()).transpose()?,
                    }))
                }
            }
            py::StatementType::Expression { expression } => {
                Ok(TopLevelStatementObj::Expression(expression.try_into()?))
            }
            _ => Err(Error::ArbitraryTopLevelStatementObj),
        }
        .map(|ok| Located(location, ok))
        .map_err(|err| err.core().located(location))
    }
}

impl TryInto<ClassDefStatement> for py::Statement {
    type Error = CoreError;

    fn try_into(self) -> Result<ClassDefStatement, Self::Error> {
        let location = self.location;
        match self.node {
            py::StatementType::Assign { mut targets, value } => {
                if targets.len() == 1 {
                    let Located(_, target) = targets.pop().unwrap().try_into()?;

                    match target {
                        ExpressionObj::Id(name) => Ok(ClassDefStatementObj::FieldDef {
                            name,
                            ty: None,
                            value: Some(value.try_into()?),
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
                    ty: Some((*annotation).try_into()?),
                    value: value.map(|value| value.try_into()).transpose()?,
                }),
                _ => Err(Error::ArbitraryClassDefStatement),
            },
            _ => Err(Error::ArbitraryClassDefStatement),
        }
        .map(|ok| Located(location, ok))
        .map_err(|err| err.core().located(location))
    }
}

impl TryInto<Params> for py::Parameters {
    type Error = CoreError;

    fn try_into(self) -> Result<Params, Self::Error> {
        if self.posonlyargs_count > 0
            || self.kwonlyargs.len() > 0
            || self.vararg != py::Varargs::None
            || self.kwarg != py::Varargs::None
            || self.defaults.len() > 0
            || self.kw_defaults.len() > 0
        {
            Err(Error::ArbitraryParams.core())
        } else {
            Ok(Params {
                params: self
                    .args
                    .into_iter()
                    .map(|arg| arg.try_into())
                    .collect::<Result<_, CoreError>>()?,
            })
        }
    }
}

impl TryInto<Param> for py::Parameter {
    type Error = CoreError;

    fn try_into(self) -> Result<Param, Self::Error> {
        if let Some(annotation) = self.annotation {
            Ok(Located(
                self.location,
                ParamObj {
                    arg: self.arg,
                    annotation: (*annotation).try_into()?,
                },
            ))
        } else {
            Err(Error::ParamWithoutType.core().located(self.location))
        }
    }
}

impl TryInto<Expression> for py::Expression {
    type Error = CoreError;

    fn try_into(self) -> Result<Expression, Self::Error> {
        let location = self.location;
        match self.node {
            py::ExpressionType::BoolOp { op, values } => {
                // Converts a boolean chain into a binop tree
                let op: Operator = op.try_into()?;

                let mut values = values.into_iter();
                let first: Expression = values.next().unwrap().try_into()?;
                let mut res = Located(
                    first.0.clone(),
                    ExpressionObj::BinOp {
                        left: Box::new(first),
                        op: op.clone(),
                        right: Box::new(values.next().unwrap().try_into()?),
                    },
                );

                for value in values {
                    let value: Expression = value.try_into()?;
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
                left: Box::new((*a).try_into()?),
                op: op.try_into()?,
                right: Box::new((*b).try_into()?),
            }),
            py::ExpressionType::Subscript { a, b } => Ok(ExpressionObj::Index {
                value: Box::new((*a).try_into()?),
                index: Box::new((*b).try_into()?),
            }),
            py::ExpressionType::Unop { op, a } => Ok(ExpressionObj::UnOp {
                op: op.try_into()?,
                value: Box::new((*a).try_into()?),
            }),
            py::ExpressionType::Compare { vals, ops } => {
                if ops.len() == 1 {
                    let mut vals = vals.into_iter();
                    let mut ops = ops.into_iter();
                    Ok(ExpressionObj::BinOp {
                        left: Box::new(vals.next().unwrap().try_into()?),
                        op: ops.next().unwrap().try_into()?,
                        right: Box::new(vals.next().unwrap().try_into()?),
                    })
                } else {
                    Err(Error::CompareChain)
                }
            }
            py::ExpressionType::Attribute { value, name } => Ok(ExpressionObj::Attribute {
                value: Box::new((*value).try_into()?),
                name,
            }),
            py::ExpressionType::Call {
                function,
                args,
                keywords,
            } => Ok(ExpressionObj::Call {
                function: Box::new((*function).try_into()?),
                args: (args, keywords).try_into()?,
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
                    .map(|element| element.try_into())
                    .collect::<Result<Vec<_>, CoreError>>()?,
            )),
            py::ExpressionType::Tuple { elements } => Ok(ExpressionObj::Tuple(
                elements
                    .into_iter()
                    .map(|element| element.try_into())
                    .collect::<Result<Vec<_>, CoreError>>()?,
            )),
            py::ExpressionType::Comprehension { kind, generators } => match *kind {
                py::ComprehensionKind::List { element } => Ok(ExpressionObj::Comprehension {
                    element: Box::new(element.try_into()?),
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

                            if is_async {
                                return Err(Error::Async.core().located(location));
                            }

                            parts.push(ComprehensionPart::For {
                                target: target.try_into()?,
                                iter: {
                                    let iter: Expression = iter.try_into()?;
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
                                            cond: cond.try_into()?,
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
                        .map(|part| part.try_into())
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
                test: Box::new((*test).try_into()?),
                body: Box::new((*body).try_into()?),
                orelse: Box::new((*orelse).try_into()?),
            }),
            py::ExpressionType::NamedExpression { .. } => Err(Error::ExpressionNamed),
            py::ExpressionType::Ellipsis => Err(Error::ExpressionEllipsis),
        }
        .map(|ok| Located(location, ok))
        .map_err(|err| err.core().located(location))
    }
}

impl TryInto<Args> for (Vec<py::Expression>, Vec<py::Keyword>) {
    type Error = CoreError;

    fn try_into(self) -> Result<Args, Self::Error> {
        let (pos, kw) = self;
        Ok(Args {
            pos: pos
                .into_iter()
                .map(|arg| arg.try_into())
                .collect::<Result<Vec<_>, CoreError>>()?,
            kw: kw
                .into_iter()
                .map(|py::Keyword { name, value }| match value.try_into() {
                    Ok(value) => Ok((name.unwrap(), value)),
                    Err(err) => Err(err),
                })
                .collect::<Result<Vec<_>, CoreError>>()?,
        })
    }
}

impl TryInto<FStrPart> for py::StringGroup {
    type Error = CoreError;

    fn try_into(self) -> Result<FStrPart, Self::Error> {
        match self {
            py::StringGroup::Constant { value } => Ok(FStrPart::Str(value)),
            py::StringGroup::FormattedValue {
                value,
                conversion,
                spec,
            } => {
                let location = value.location;
                if conversion.is_some() || spec.is_some() {
                    Err(Error::FStrWithSpec.core().located(location))
                } else {
                    Ok(FStrPart::ExpressionObj((*value).try_into()?))
                }
            }
            _ => panic!("Encountered an unexpected syntax element"),
        }
    }
}

impl TryInto<TyExpression> for py::Expression {
    type Error = CoreError;

    fn try_into(self) -> Result<TyExpression, Self::Error> {
        let location = self.location;
        match self.node {
            py::ExpressionType::Subscript { a, b } => {
                let Located(_, base) = (*a).try_into()?;
                let base = match base {
                    TyExpressionObj::Generic { base, params } if params.len() == 0 => base,
                    _ => {
                        return Err(Error::ArbitraryTyExpression.core().located(location));
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
                    .map(|param| param.try_into())
                    .collect::<Result<_, CoreError>>()?,
                })
            }
            py::ExpressionType::Attribute { value, name } => {
                let Located(_, base) = (*value).try_into()?;
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
                    .map_err(|_| Error::ArbitraryTyExpression.core().located(location))?,
            )),
            _ => Err(Error::ArbitraryTyExpression),
        }
        .map(|ok| Located(location, ok))
        .map_err(|err| err.core().located(location))
    }
}

impl TryInto<Statement> for py::Statement {
    type Error = CoreError;

    fn try_into(self) -> Result<Statement, Self::Error> {
        let location = self.location;
        match self.node {
            py::StatementType::Break => Ok(StatementObj::Break),
            py::StatementType::Continue => Ok(StatementObj::Continue),
            py::StatementType::Return { value } => Ok(StatementObj::Return {
                value: value.map(|value| value.try_into()).transpose()?,
            }),
            py::StatementType::Pass => Ok(StatementObj::Pass),
            py::StatementType::Assert { test, msg } => Ok(StatementObj::Assert {
                test: test.try_into()?,
                msg: msg.map(|msg| msg.try_into()).transpose()?,
            }),
            py::StatementType::Assign { mut targets, value } => Ok(StatementObj::Assign {
                target: if targets.len() == 1 {
                    targets.pop().unwrap().try_into()?
                } else {
                    Located(
                        location.clone(),
                        ExpressionObj::Tuple(
                            targets
                                .into_iter()
                                .map(|target| target.try_into())
                                .collect::<Result<Vec<_>, CoreError>>()?,
                        ),
                    )
                },
                value: value.try_into()?,
            }),
            py::StatementType::AugAssign { target, op, value } => Ok(StatementObj::OpAssign {
                target: (*target).try_into()?,
                op: op.try_into()?,
                value: (*value).try_into()?,
            }),
            py::StatementType::AnnAssign {
                target,
                annotation,
                value,
            } => Ok(StatementObj::TyAssign {
                target: (*target).try_into()?,
                ty: (*annotation).try_into()?,
                value: value.map(|value| value.try_into()).transpose()?,
            }),
            py::StatementType::Expression { expression } => Ok(StatementObj::ExpressionObj {
                expression: expression.try_into()?,
            }),
            py::StatementType::If { test, body, orelse } => Ok(StatementObj::If {
                test: test.try_into()?,
                body: body
                    .into_iter()
                    .map(|statement| statement.try_into())
                    .collect::<Result<_, CoreError>>()?,
                orelse: orelse
                    .map(|orelse| {
                        orelse
                            .into_iter()
                            .map(|statement| statement.try_into())
                            .collect::<Result<_, CoreError>>()
                    })
                    .transpose()?,
            }),
            py::StatementType::While { test, body, orelse } => match orelse {
                None => Ok(StatementObj::While {
                    test: test.try_into()?,
                    body: body
                        .into_iter()
                        .map(|statement| statement.try_into())
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
                        target: (*target).try_into()?,
                        iter: {
                            let iter: Expression = (*iter).try_into()?;
                            Located(iter.0.clone(), ExpressionObj::Iter { value: iter.into() })
                        },
                        body: body
                            .into_iter()
                            .map(|statement| statement.try_into())
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
        .map(|ok| Located(location, ok))
        .map_err(|err| err.core().located(location))
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
            Self::MatMult => return Err(Error::OperatorMatMult.core()),
        })
    }
}

impl TryInto<Operator> for py::BooleanOperator {
    type Error = CoreError;

    fn try_into(self) -> Result<Operator, Self::Error> {
        Ok(match self {
            Self::And => Operator::And,
            Self::Or => Operator::Or,
        })
    }
}

impl TryInto<Operator> for py::Comparison {
    type Error = CoreError;

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
    type Error = CoreError;

    fn try_into(self) -> Result<UnaryOperator, Self::Error> {
        Ok(match self {
            Self::Pos => UnaryOperator::Pos,
            Self::Neg => UnaryOperator::Neg,
            Self::Not => UnaryOperator::Not,
            Self::Inv => UnaryOperator::Inv,
        })
    }
}

pub fn clean(program: py::Program) -> Result<Module, CoreError> {
    program.try_into()
}
