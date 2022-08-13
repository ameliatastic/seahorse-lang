use crate::core::{python_ast as py, seahorse_ast::*, CoreError};
use std::mem::replace;

pub enum UnsupportedError {
    ParamsWithKwargs,
    ParamsWithVarargs,
    ParamsWithDefaults,
    ParamsWithoutType,
    Import,
    AssertWithoutString,
    DeleteStatement,
    AssignToMultiple,
    GlobalStatement,
    NonlocalStatement,
    WhileWithElse,
    WithStatement,
    AsyncForIn,
    ForInWithElse,
    RaiseStatement,
    TryStatement,
    ClassDefStatement,
    FunctionDefStatement,
    TypeNotRecognized,
    ListType,
    ArrayTypeNotRecognized,
    TraitNotRecognized(String),
    TraitNotIdentifier,
    Await,
    Yield,
    YieldFrom,
    CompareChain,
    BigIntegers,
    ComplexNumbers,
    DictExpression,
    SetExpression,
    ComprehensionNotList,
    Starred,
    Slice,
    FStringWithSpec,
    ByteString,
    UnderscoreIdentifier,
    Lambda,
    Ternary,
    NamedExpression,
    NoneExpression,
    Ellipsis,
    MatMultOp,
    InOp,
    NotInOp,
    InvalidPattern,
    ClassDefWithKeywords,
    ClassDefWithDecorators,
    EnumAccount,
    EnumWithNonAssign,
    ClassWithNonAssign,
    FunctionAsync,
    InstructionWithReturn,
    InvalidDeclareIdArgs,
    TopLevelExpression,
    TopLevelStatement,
}

impl From<UnsupportedError> for CoreError {
    fn from(error: UnsupportedError) -> Self {
        match error {
            UnsupportedError::ParamsWithKwargs => Self::make_raw(
                "keyword args in functions are not implemented yet",
                ""
            ),
            UnsupportedError::ParamsWithVarargs => Self::make_raw(
                "variadic args in functions are not implemented yet",
                ""
            ),
            UnsupportedError::ParamsWithDefaults => Self::make_raw(
                "default values for function arguments are not implemented yet",
                ""
            ),
            UnsupportedError::ParamsWithoutType => Self::make_raw(
                "all function params must have a type annotation",
                ""
            ),
            UnsupportedError::Import => Self::make_raw(
                "import statements are not implemented yet",
                ""
            ),
            UnsupportedError::AssertWithoutString => Self::make_raw(
                "invalid 'assert' statement",
                "Help: Seahorse 'assert' statements must have the following form:\n\n    assert condition, 'string literal'",
            ),
            UnsupportedError::DeleteStatement => Self::make_raw(
                "'del' statements are not implemented yet",
                ""
            ),
            UnsupportedError::AssignToMultiple => Self::make_raw(
                "multiple assignment is not implemented yet",
                ""
            ),
            UnsupportedError::GlobalStatement => Self::make_raw(
                "'global' statements are unsupported",
                ""
            ),
            UnsupportedError::NonlocalStatement => Self::make_raw(
                "'nonlocal' statements are unsupported",
                ""
            ),
            UnsupportedError::WhileWithElse => Self::make_raw(
                "'while-else' statements are not implemented yet",
                "Hint: remove the 'else' block after the while loop."
            ),
            UnsupportedError::WithStatement => Self::make_raw(
                "'with' statements are not implemented yet",
                ""
            ),
            UnsupportedError::AsyncForIn => Self::make_raw(
                "async 'for-in' statements are unsupported",
                ""
            ),
            UnsupportedError::ForInWithElse => Self::make_raw(
                "'for-in-else' statements are not implemented yet",
                "Hint: remove the 'else' block after the for loop."
            ),
            UnsupportedError::RaiseStatement => Self::make_raw(
                "'raise' statements are not implemented yet",
                "Hint: if you want to throw an error, try using an assert."
            ),
            UnsupportedError::TryStatement => Self::make_raw(
                "'try' statements are not implemented yet",
                ""
            ),
            UnsupportedError::ClassDefStatement => Self::make_raw(
                "class defs must be at the top level",
                "Hint: try moving this class definition outside of this function."
            ),
            UnsupportedError::FunctionDefStatement => Self::make_raw(
                "function defs must be at the top level",
                "Hint: try moving this function definition outside of this function."
            ),
            UnsupportedError::TypeNotRecognized => Self::make_raw(
                "unsupported type hint",
                ""
            ),
            UnsupportedError::ListType => Self::make_raw(
                "list type is not implemented yet",
                "Help: Lists (and other mutable types) will be supported when Seahorse is out of beta. For now, you can try using an Array."
            ),
            UnsupportedError::ArrayTypeNotRecognized => Self::make_raw(
                "unsupported array type hint",
                "Hint: array types have the following form: Array[T, N] where T is a type and N is an integer literal"
            ),
            UnsupportedError::TraitNotIdentifier => Self::make_raw(
                "unsupported base class",
                ""
            ),
            UnsupportedError::TraitNotRecognized(name) => Self::make_raw(
                "unsupported base class",
                format!("No base class named \"{}\"", name)
            ),
            UnsupportedError::Await => Self::make_raw(
                "await is unsupported",
                ""
            ),
            UnsupportedError::Yield => Self::make_raw(
                "yield is not implemented yet",
                ""
            ),
            UnsupportedError::YieldFrom => Self::make_raw(
                "yield is not implemented yet",
                ""
            ),
            UnsupportedError::CompareChain => Self::make_raw(
                "comparison chains are not implemented yet (this will change in a future release)",
                ""
            ),
            UnsupportedError::BigIntegers => Self::make_raw(
                "big integers are not implemented yet",
                "Help: all integer values must fit into a Rust i64, which covers the range [-9223372036854775808, 9223372036854775807]."
            ),
            UnsupportedError::ComplexNumbers => Self::make_raw(
                "complex numbers are unsupported",
                ""
            ),
            UnsupportedError::DictExpression => Self::make_raw(
                "dicts are not implemented yet",
                ""
            ),
            UnsupportedError::SetExpression => Self::make_raw(
                "sets are not implemented yet",
                ""
            ),
            UnsupportedError::ComprehensionNotList => Self::make_raw(
                "non-list comprehensions are not implemented yet",
                ""
            ),
            UnsupportedError::Starred => Self::make_raw(
                "starred expressions are not implemented yet",
                ""
            ),
            UnsupportedError::Slice => Self::make_raw(
                "slicing is not implemented yet",
                ""
            ),
            UnsupportedError::FStringWithSpec => Self::make_raw(
                "f-strings with format specifiers are not implemented yet",
                "Help: f-strings are only supported in a basic way, try using no format specifier:\n\n    f'This is an {element}'"
            ),
            UnsupportedError::ByteString => Self::make_raw(
                "bytestrings are not implemented yet",
                ""
            ),
            UnsupportedError::UnderscoreIdentifier => Self::make_raw(
                "\"_\" is not allowed as a variable name",
                "Help: in Rust, single-underscore (_) is not a valid identifier."
            ),
            UnsupportedError::Lambda => Self::make_raw(
                "lambda functions are not implemented yet",
                "Help: try moving this lambda into a new top-level function."
            ),
            UnsupportedError::Ternary => Self::make_raw(
                "ternary expressions are not implemented yet",
                ""
            ), // TODO support
            UnsupportedError::NamedExpression => Self::make_raw(
                "the walrus operator is not implemented yet",
                ""
            ),
            UnsupportedError::NoneExpression => Self::make_raw(
                "the None keyword is not implemented yet",
                ""
            ),
            UnsupportedError::Ellipsis => Self::make_raw(
                "ellipsis are not implemented yet",
                ""
            ),
            UnsupportedError::MatMultOp => Self::make_raw(
                "matrix multiplication is unsupported",
                ""
            ),
            UnsupportedError::InOp => Self::make_raw(
                "the 'in' operator is not implemented yet",
                ""
            ),
            UnsupportedError::NotInOp => Self::make_raw(
                "the 'not in' operator is not implemented yet",
                ""
            ),
            UnsupportedError::InvalidPattern => Self::make_raw(
                "for-in targets must be single variables",
                "(This will change in a future release.)"
            ),
            UnsupportedError::ClassDefWithKeywords => Self::make_raw(
                "class defs may not have keywords",
                ""
            ),
            UnsupportedError::ClassDefWithDecorators => Self::make_raw(
                "class defs may not have decorators",
                ""
            ),
            UnsupportedError::EnumAccount => Self::make_raw(
                "accounts may not be be enums",
                "Help: if you want enumerated data in an account, you can make a helper enum class and put that in the account."
            ),
            UnsupportedError::EnumWithNonAssign => Self::make_raw(
                "enums must only include variant definitions",
                "Help: each enum variant looks like:\n\n    variant_name = 0 # (or any other unique number)"
            ),
            UnsupportedError::ClassWithNonAssign => Self::make_raw(
                "classes must only have field type definitions",
                "Help: each field type definition looks like this:\n\n    field_name: FieldType"
            ),
            UnsupportedError::FunctionAsync => Self::make_raw(
                "async functions are not supported",
                "Hint: your Solana instruction will run synchronously, so if you're using async, you might be using the wrong programming model."
            ),
            UnsupportedError::InstructionWithReturn => Self::make_raw(
                "instructions must not have a return type",
                ""
            ),
            UnsupportedError::InvalidDeclareIdArgs => Self::make_raw(
                "invalid declare_id arguments",
                "Help: a valid declare_id statement looks like this:\n\n    declare_id('copy-pasted id from your IDL .json file')"
            ),
            UnsupportedError::TopLevelExpression => Self::make_raw(
                "arbitrary top-level expressions are not supported",
                ""
            ),
            UnsupportedError::TopLevelStatement => Self::make_raw(
                "arbitrary top-level statements are not supported",
                "Hint: if you're trying to write the body of your smart contract, you need to put it inside of an instruction:\n\n    @instruction\n    def instruction_name(arg: Type, account: AccountType, ...):\n        ..."
            ),
        }
    }
}

impl UnsupportedError {
    // Helper function to keep conversion code cleaner.
    pub fn into_core(self) -> CoreError {
        self.into()
    }
}

/// Helper function to repeatedly try_into() with the contents of a Vec.
fn vec_try_into<X, Y>(vec: Vec<X>) -> Result<Vec<Y>, <X as TryInto<Y>>::Error>
where
    Y: TryFrom<X>,
{
    let mut vec_ = Vec::new();
    for val in vec.into_iter() {
        vec_.push(val.try_into()?);
    }
    return Ok(vec_);
}

/// Helper function to try_into() with the contents of a Box.
fn box_try_into<X, Y>(obj: Box<X>) -> Result<Box<Y>, <X as TryInto<Y>>::Error>
where
    Y: TryFrom<X>,
{
    Ok(Box::new((*obj).try_into()?))
}

/// Helper function to try_into() with the contents of an Option.
fn option_try_into<X, Y>(opt: Option<X>) -> Result<Option<Y>, <X as TryInto<Y>>::Error>
where
    Y: TryFrom<X>,
{
    Ok(match opt {
        Some(val) => Some(val.try_into()?),
        None => None,
    })
}

fn try_into_params(
    params: py::Parameters,
    location: py::Location,
) -> Result<Vec<Param>, CoreError> {
    if params.kwonlyargs.len() > 0 {
        return Err(UnsupportedError::ParamsWithKwargs
            .into_core()
            .located(location));
    }
    if params.vararg != py::Varargs::None || params.kwarg != py::Varargs::None {
        return Err(UnsupportedError::ParamsWithVarargs
            .into_core()
            .located(location));
    }
    if params.defaults.len() > 0 || params.kw_defaults.len() > 0 {
        return Err(UnsupportedError::ParamsWithDefaults
            .into_core()
            .located(location));
    }

    let mut params_ = Vec::new();
    for param in params.args.into_iter() {
        params_.push(Param {
            name: param.arg,
            ty: match param.annotation {
                Some(annotation) => (*annotation).try_into()?,
                None => {
                    return Err(UnsupportedError::ParamsWithoutType
                        .into_core()
                        .located(location));
                }
            },
            required: true,
        });
    }

    return Ok(params_);
}

impl TryFrom<py::Statement> for Statement {
    type Error = CoreError;

    fn try_from(statement: py::Statement) -> Result<Self, Self::Error> {
        match statement.node {
            py::StatementType::Break => Ok(Statement::Break),
            py::StatementType::Continue => Ok(Statement::Continue),
            py::StatementType::Return { value } => Ok(Statement::Return {
                value: option_try_into(value)?,
            }),
            py::StatementType::Import { .. } => Err(UnsupportedError::Import),
            py::StatementType::ImportFrom { .. } => Err(UnsupportedError::Import),
            py::StatementType::Pass => Ok(Statement::Noop),
            py::StatementType::Assert {
                test,
                msg: Some(msg),
            } => match (Expression::try_from(msg)?).as_string() {
                Some(msg) => Ok(Statement::RawAssert {
                    cond: test.try_into()?,
                    msg,
                    location: statement.location,
                }),
                None => Err(UnsupportedError::AssertWithoutString),
            },
            py::StatementType::Assert { .. } => Err(UnsupportedError::AssertWithoutString),
            py::StatementType::Delete { .. } => Err(UnsupportedError::DeleteStatement),
            py::StatementType::Assign { mut targets, value } => {
                if targets.len() == 1 {
                    Ok(Statement::RawAssign {
                        left: targets.remove(0).try_into()?,
                        ty: None,
                        right: Some(value.try_into()?),
                        location: statement.location,
                    })
                } else {
                    // TODO can maybe support once we have tuples
                    Err(UnsupportedError::AssignToMultiple)
                }
            }
            py::StatementType::AugAssign { target, op, value } => Ok(Statement::OpAssign {
                left: (*target).try_into()?,
                op: op.try_into()?,
                right: (*value).try_into()?,
            }),
            py::StatementType::AnnAssign {
                target,
                annotation,
                value,
            } => Ok(Statement::RawAssign {
                left: (*target).try_into()?,
                ty: Some((*annotation).try_into()?),
                right: option_try_into(value)?,
                location: statement.location,
            }),
            py::StatementType::Expression { expression } => Ok(Statement::RawExpression {
                value: expression.try_into()?,
                location: statement.location,
            }),
            py::StatementType::Global { .. } => Err(UnsupportedError::GlobalStatement),
            py::StatementType::Nonlocal { .. } => Err(UnsupportedError::NonlocalStatement),
            py::StatementType::If { test, body, orelse } => {
                let or_else = match orelse {
                    Some(block) => Some(vec_try_into(block)?),
                    None => None,
                };

                Ok(Statement::If {
                    cond: test.try_into()?,
                    body: vec_try_into(body)?,
                    or_else,
                })
            }
            py::StatementType::While { test, body, orelse } => {
                if orelse.is_some() {
                    Err(UnsupportedError::WhileWithElse)
                } else {
                    Ok(Statement::While {
                        cond: test.try_into()?,
                        body: vec_try_into(body)?,
                    })
                }
            }
            py::StatementType::With { .. } => Err(UnsupportedError::WithStatement),
            py::StatementType::For {
                is_async,
                target,
                iter,
                body,
                orelse,
            } => {
                if is_async {
                    Err(UnsupportedError::AsyncForIn)
                } else if orelse.is_some() {
                    Err(UnsupportedError::ForInWithElse)
                } else {
                    Ok(Statement::ForIn {
                        target: (*target).try_into()?,
                        iter: (*iter).try_into()?,
                        body: vec_try_into(body)?,
                    })
                }
            }
            py::StatementType::Raise { .. } => Err(UnsupportedError::RaiseStatement),
            py::StatementType::Try { .. } => Err(UnsupportedError::TryStatement),
            py::StatementType::ClassDef { .. } => Err(UnsupportedError::ClassDefStatement),
            py::StatementType::FunctionDef { .. } => Err(UnsupportedError::FunctionDefStatement),
        }
        .map_err(|err| err.into_core().located(statement.location))
    }
}

impl TryFrom<py::Expression> for Ty {
    type Error = CoreError;

    fn try_from(expression: py::Expression) -> Result<Self, Self::Error> {
        let location = expression.location.clone();
        let expression: Expression = expression.try_into()?;

        expression
            .try_into()
            .map_err(|err: CoreError| err.located(location))
    }
}

impl TryFrom<Expression> for Ty {
    type Error = CoreError;

    fn try_from(expression: Expression) -> Result<Self, Self::Error> {
        match expression {
            Expression::Id(name) => Ok(match name.as_str() {
                // Supported types
                "u8" => Ty::U8,
                "u64" => Ty::U64,
                "i64" => Ty::I64,
                "f64" => Ty::F64,
                "bool" => Ty::Bool,
                "String" => Ty::String,
                "Pubkey" => Ty::Pubkey,
                "Signer" => Ty::Signer,
                "TokenAccount" => Ty::TokenAccount,
                "TokenMint" => Ty::TokenMint,
                "AssociatedTokenAccount" => Ty::AssociatedTokenAccount,
                // User types
                name => Ty::Defined(name.to_string()),
            }),
            Expression::Index { value, index } => match *value {
                Expression::Id(name) => match name.as_str() {
                    "Empty" => Ok(Ty::Empty(box_try_into(index)?)),
                    // TODO disabling lists for now
                    // "List" => Ok(Ty::List(box_try_into(index)?)),
                    "List" => Err(UnsupportedError::ListType),
                    "Array" => match index.as_tuple2() {
                        Some((expr, Expression::Int(len))) => Ok(Self::Array(
                            Box::new(expr.try_into()?),
                            TyParam::Exact(len as usize),
                        )),
                        _ => Err(UnsupportedError::ArrayTypeNotRecognized),
                    },
                    _ => Err(UnsupportedError::TypeNotRecognized),
                },
                _ => Err(UnsupportedError::TypeNotRecognized),
            },
            _ => Err(UnsupportedError::TypeNotRecognized),
        }
        .map_err(|err| err.into_core())
    }
}

impl TryFrom<py::Expression> for TraitName {
    type Error = CoreError;

    fn try_from(expression: py::Expression) -> Result<Self, Self::Error> {
        match expression.node {
            py::ExpressionType::Identifier { name } => match name.as_str() {
                "Enum" => Ok(TraitName::Enum),
                "Account" => Ok(TraitName::Account),
                // Decorators
                "instruction" => Ok(TraitName::Instruction), // TODO separate from trait types
                name => Err(UnsupportedError::TraitNotRecognized(name.to_string())),
            },
            _ => Err(UnsupportedError::TraitNotIdentifier),
        }
        .map_err(|err| err.into_core().located(expression.location))
    }
}

impl TryFrom<py::Expression> for Expression {
    type Error = CoreError;

    fn try_from(expression: py::Expression) -> Result<Self, Self::Error> {
        let location = expression.location;

        match expression.node {
            py::ExpressionType::BoolOp { op, values } => {
                // Converts a boolean chain into a binop tree
                let op = op.try_into()?;

                let mut values = values.into_iter();
                let mut expr = Expression::BinOp {
                    left: Box::new(values.next().unwrap().try_into()?),
                    op,
                    right: Box::new(values.next().unwrap().try_into()?),
                };

                for value in values {
                    expr = Expression::BinOp {
                        left: Box::new(expr),
                        op,
                        right: Box::new(value.try_into()?),
                    }
                }

                Ok(expr)
            }
            py::ExpressionType::Binop { a, op, b } => Ok(Expression::BinOp {
                left: box_try_into(a)?,
                op: op.try_into()?,
                right: box_try_into(b)?,
            }),
            py::ExpressionType::Subscript { a, b } => Ok(Expression::Index {
                value: box_try_into(a)?,
                index: box_try_into(b)?,
            }),
            py::ExpressionType::Unop { op, a } => Ok(Expression::UnOp {
                op: op.try_into()?,
                value: box_try_into(a)?,
            }),
            py::ExpressionType::Await { .. } => Err(UnsupportedError::Await),
            py::ExpressionType::Yield { .. } => Err(UnsupportedError::Yield),
            py::ExpressionType::YieldFrom { .. } => Err(UnsupportedError::YieldFrom),
            py::ExpressionType::Compare { vals, ops } => {
                if ops.len() == 1 {
                    let mut vals = vals.into_iter();
                    let mut ops = ops.into_iter();

                    Ok(Expression::BinOp {
                        left: Box::new(vals.next().unwrap().try_into()?),
                        op: ops.next().unwrap().try_into()?,
                        right: Box::new(vals.next().unwrap().try_into()?),
                    })
                } else {
                    Err(UnsupportedError::CompareChain)
                }
            }
            py::ExpressionType::Attribute { value, name } => Ok(Expression::Attribute {
                value: box_try_into(value)?,
                attr: name,
            }),
            py::ExpressionType::Call {
                function,
                args,
                keywords,
            } => {
                let mut kwargs = Vec::new();
                for kw in keywords.into_iter() {
                    kwargs.push((kw.name.unwrap(), kw.value.try_into()?));
                }

                Ok(Expression::RawCall {
                    func: box_try_into(function)?,
                    args: CallArgs {
                        pos: vec_try_into(args)?,
                        kw: kwargs,
                    },
                    location: expression.location,
                })
            }
            py::ExpressionType::Number { value } => match value {
                py::Number::Integer { value } => match value.to_str_radix(10).parse() {
                    Ok(value) => Ok(Expression::Int(value)),
                    _ => Err(UnsupportedError::BigIntegers),
                },
                py::Number::Float { value } => Ok(Expression::Float(value)),
                py::Number::Complex { .. } => Err(UnsupportedError::ComplexNumbers),
            },
            py::ExpressionType::List { elements } => Ok(Expression::List(vec_try_into(elements)?)),
            py::ExpressionType::Tuple { elements } => {
                Ok(Expression::Tuple(vec_try_into(elements)?))
            }
            py::ExpressionType::Dict { .. } => Err(UnsupportedError::DictExpression),
            py::ExpressionType::Set { .. } => Err(UnsupportedError::SetExpression),
            py::ExpressionType::Comprehension { kind, generators } => match *kind {
                py::ComprehensionKind::List { element } => {
                    let mut parts = Vec::new();
                    for mut comprehension in generators.into_iter() {
                        if comprehension.is_async {
                            return Err(UnsupportedError::AsyncForIn
                                .into_core()
                                .located(comprehension.location));
                        }

                        let ifs = replace(&mut comprehension.ifs, Vec::new());
                        parts.push(comprehension.try_into()?);
                        for cond in ifs.into_iter() {
                            parts.push(ComprehensionPart::If {
                                cond: cond.try_into()?,
                            });
                        }
                    }

                    Ok(Expression::Comprehension {
                        element: Box::new(element.try_into()?),
                        parts,
                    })
                }
                _ => Err(UnsupportedError::ComprehensionNotList),
            },
            py::ExpressionType::Starred { .. } => Err(UnsupportedError::Starred),
            py::ExpressionType::Slice { .. } => Err(UnsupportedError::Slice),
            py::ExpressionType::String { value } => match value {
                py::StringGroup::Constant { value } => Ok(Expression::String(value)),
                py::StringGroup::Joined { values } => {
                    let mut values_: Vec<Expression> = Vec::new();
                    for (i, value) in values.into_iter().enumerate() {
                        match value {
                            py::StringGroup::Constant { value } => {
                                values_.push(Expression::String(value))
                            }
                            py::StringGroup::FormattedValue {
                                value,
                                conversion,
                                spec,
                            } => {
                                if conversion.is_some() || spec.is_some() {
                                    return Err(UnsupportedError::FStringWithSpec
                                        .into_core()
                                        .located(location));
                                }

                                values_.push((*value).try_into()?);
                            }
                            py::StringGroup::Joined { .. } => panic!("Unexpected f-string?"),
                        }
                    }

                    Ok(Expression::RawFString { values: values_ })
                }
                py::StringGroup::FormattedValue { .. } => panic!("Unexpected FormattedValue?"),
            },
            py::ExpressionType::Bytes { .. } => Err(UnsupportedError::ByteString),
            py::ExpressionType::Identifier { name } => match name.as_str() {
                "_" => Err(UnsupportedError::UnderscoreIdentifier),
                _ => Ok(Expression::Id(name)),
            },
            py::ExpressionType::Lambda { .. } => Err(UnsupportedError::Lambda),
            py::ExpressionType::IfExpression { .. } => Err(UnsupportedError::Ternary),
            py::ExpressionType::NamedExpression { .. } => Err(UnsupportedError::NamedExpression),
            py::ExpressionType::True => Ok(Expression::Bool(true)),
            py::ExpressionType::False => Ok(Expression::Bool(true)),
            py::ExpressionType::None => Err(UnsupportedError::NoneExpression),
            py::ExpressionType::Ellipsis => Err(UnsupportedError::Ellipsis),
        }
        .map_err(|err| err.into_core().located(location))
    }
}

impl TryFrom<py::Operator> for Operator {
    type Error = CoreError;

    fn try_from(op: py::Operator) -> Result<Self, Self::Error> {
        match op {
            py::Operator::Add => Ok(Operator::Add),
            py::Operator::Sub => Ok(Operator::Sub),
            py::Operator::Mult => Ok(Operator::Mul),
            py::Operator::MatMult => Err(UnsupportedError::MatMultOp),
            py::Operator::Div => Ok(Operator::Div),
            py::Operator::Mod => Ok(Operator::Mod),
            py::Operator::Pow => Ok(Operator::Pow),
            py::Operator::LShift => Ok(Operator::LShift),
            py::Operator::RShift => Ok(Operator::RShift),
            py::Operator::BitOr => Ok(Operator::BitOr),
            py::Operator::BitXor => Ok(Operator::BitXor),
            py::Operator::BitAnd => Ok(Operator::BitAnd),
            py::Operator::FloorDiv => Ok(Operator::FloorDiv),
        }
        .map_err(|err| err.into_core())
    }
}

impl TryFrom<py::BooleanOperator> for Operator {
    type Error = CoreError;

    fn try_from(op: py::BooleanOperator) -> Result<Self, Self::Error> {
        match op {
            py::BooleanOperator::And => Ok(Operator::And),
            py::BooleanOperator::Or => Ok(Operator::Or),
        }
    }
}

impl TryFrom<py::Comparison> for Operator {
    type Error = CoreError;

    fn try_from(op: py::Comparison) -> Result<Self, Self::Error> {
        match op {
            py::Comparison::Equal => Ok(Operator::Eq),
            py::Comparison::NotEqual => Ok(Operator::NotEq),
            py::Comparison::Less => Ok(Operator::Lt),
            py::Comparison::LessOrEqual => Ok(Operator::Lte),
            py::Comparison::Greater => Ok(Operator::Gt),
            py::Comparison::GreaterOrEqual => Ok(Operator::Gte),
            py::Comparison::In => Err(UnsupportedError::InOp),
            py::Comparison::NotIn => Err(UnsupportedError::NotInOp),
            py::Comparison::Is => Ok(Operator::Eq),
            py::Comparison::IsNot => Ok(Operator::NotEq),
        }
        .map_err(|err| err.into_core())
    }
}

impl TryFrom<py::UnaryOperator> for UnaryOperator {
    type Error = CoreError;

    fn try_from(op: py::UnaryOperator) -> Result<Self, Self::Error> {
        match op {
            py::UnaryOperator::Pos => Ok(UnaryOperator::Pos),
            py::UnaryOperator::Neg => Ok(UnaryOperator::Neg),
            py::UnaryOperator::Not => Ok(UnaryOperator::Not),
            py::UnaryOperator::Inv => Ok(UnaryOperator::Inv),
        }
    }
}

impl TryFrom<py::Comprehension> for ComprehensionPart {
    type Error = CoreError;

    fn try_from(comprehension: py::Comprehension) -> Result<Self, Self::Error> {
        let py::Comprehension {
            target,
            iter,
            is_async,
            location,
            ..
        } = comprehension;

        if is_async {
            return Err(UnsupportedError::AsyncForIn.into_core().located(location));
        }

        Ok(ComprehensionPart::For {
            target: target.try_into()?,
            iter: iter.try_into()?,
        })
    }
}

impl TryFrom<py::Expression> for Pattern {
    type Error = CoreError;

    fn try_from(expression: py::Expression) -> Result<Self, Self::Error> {
        let location = expression.location.clone();
        let expression: Expression = expression.try_into()?;

        match expression {
            Expression::Id(name) => Ok(Pattern::Single(name)),
            _ => Err(UnsupportedError::InvalidPattern),
        }
        .map_err(|err| err.into_core().located(location))
    }
}

impl TryFrom<py::Statement> for Def {
    type Error = CoreError;

    fn try_from(statement: py::Statement) -> Result<Def, Self::Error> {
        let location = statement.location;
        match statement.node {
            py::StatementType::Import { .. } => Ok(Def::RawImport),
            py::StatementType::ImportFrom { .. } => Ok(Def::RawImport),
            py::StatementType::ClassDef {
                name,
                body,
                bases,
                keywords,
                decorator_list,
            } => {
                if keywords.len() > 0 {
                    Err(UnsupportedError::ClassDefWithKeywords)
                } else if decorator_list.len() > 0 {
                    Err(UnsupportedError::ClassDefWithDecorators)
                } else {
                    let bases = vec_try_into(bases)?;
                    let is_enum = bases.contains(&TraitName::Enum);
                    let is_account = bases.contains(&TraitName::Account);

                    let body = vec_try_into(body)?;

                    if is_enum && is_account {
                        Err(UnsupportedError::EnumAccount)
                    } else if is_enum {
                        let mut options = Vec::new();
                        for statement in body.into_iter() {
                            options.push(match statement {
                                Statement::RawAssign {
                                    left: Expression::Id(name),
                                    ty: None,
                                    ..
                                } => name,
                                _ => {
                                    return Err(UnsupportedError::EnumWithNonAssign
                                        .into_core()
                                        .located(location));
                                }
                            });
                        }

                        Ok(Def::TyDef(TyDef::Enum { name, options }))
                    } else {
                        let mut fields = Vec::new();
                        for statement in body.into_iter() {
                            fields.push(match statement {
                                Statement::RawAssign {
                                    left: Expression::Id(name),
                                    ty: Some(ty),
                                    right: None,
                                    ..
                                } => Field { name, ty },
                                _ => {
                                    return Err(UnsupportedError::ClassWithNonAssign
                                        .into_core()
                                        .located(location));
                                }
                            });
                        }

                        let type_def = TyDef::Struct {
                            name,
                            fields,
                            traits: bases,
                        };
                        Ok(Def::TyDef(type_def))
                    }
                }
            }
            py::StatementType::FunctionDef { is_async: true, .. } => {
                Err(UnsupportedError::FunctionAsync)
            }
            py::StatementType::FunctionDef {
                name,
                args,
                body,
                decorator_list,
                returns,
                ..
            } => {
                let params = try_into_params(*args, location.clone())?;

                let decorator_list = vec_try_into(decorator_list)?;
                let mut body = vec_try_into(body)?;

                let mut def = FunctionDef {
                    name,
                    params,
                    returns: option_try_into(returns)?.unwrap_or(Ty::Unit),
                    body,
                };

                if decorator_list.contains(&TraitName::Instruction) {
                    if &def.returns != &Ty::Unit {
                        Err(UnsupportedError::InstructionWithReturn)
                    } else {
                        def.body.push(Statement::ReturnOk);
                        def.returns = Ty::ProgramResult;
                        Ok(Def::RawInstructionDef(def))
                    }
                } else {
                    Ok(Def::FunctionDef(def))
                }
            }
            py::StatementType::Expression { expression } => match expression.try_into()? {
                Expression::RawCall { func, mut args, .. } => match func.as_id() {
                    Some(id) if id.as_str() == "declare_id" => {
                        let id = args.remove_pos_or_kw("id").and_then(|id| id.as_string());

                        if id.is_some() {
                            Ok(Def::RawDeclareId(id.unwrap()))
                        } else {
                            Err(UnsupportedError::InvalidDeclareIdArgs)
                        }
                    }
                    _ => Err(UnsupportedError::TopLevelExpression),
                },
                _ => Err(UnsupportedError::TopLevelExpression),
            },
            _ => Err(UnsupportedError::TopLevelStatement),
        }
        .map_err(|err| err.into_core().located(statement.location))
    }
}

impl TryFrom<py::Program> for Program {
    type Error = CoreError;

    fn try_from(program: py::Program) -> Result<Program, Self::Error> {
        let defs = vec_try_into(program.statements)?;
        let program = Program {
            id: "".to_string(),
            defs,
            instructions: Vec::new(),
            errors: Vec::new(),
        };

        return Ok(program);
    }
}
