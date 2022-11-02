// TODO just throwing everything into mod.rs for now, don't want to deal with keeping things clean
// yet
use crate::{
    core::{
        clean::ast::{self, ComprehensionPart, Expression, ParamObj},
        compile::{ast::*, builtin::*, check::*},
        util::*,
    },
    match1,
};
use heck::ToPascalCase;
use quote::quote;
use std::{collections::VecDeque, rc::Rc};

use super::{check::*, namespace::*, sign::*};

enum Error {
    InvalidDecorator(Ty),
    MisplacedInit,
    TopLevelNonDirective,
    MisplacedDirective,
    MisplacedCpi,
}

impl Error {
    fn core(self, loc: &Location) -> CoreError {
        match self {
            Self::InvalidDecorator(dec) => {
                CoreError::make_raw(format!("\"{:?}\" is not a valid decorator", dec), "")
            }
            Self::MisplacedInit => {
                CoreError::make_raw("Empty.inits can only be at the top of an @instruction", "")
            }
            Self::TopLevelNonDirective => {
                CoreError::make_raw("arbitrary expression may not be at the top level of a module", "Hint: the only expressions that can be at the top level of a module are directives, like declare_id.")
            }
            Self::MisplacedDirective => {
                CoreError::make_raw("misplaced directive", "Hint: directives (like declare_id) are special expressions that can only be at the top level of a module.")
            }
            Self::MisplacedCpi => {
                CoreError::make_raw(
                    "misplaced CPI",
                    "Hint: CPI functions are currently only supported inside of @instructions. Try moving this call to the instruction that uses it instead.\n\n(This will change in a future release.)"
                )
            }
        }
        .located(loc.clone())
    }
}

/// Represents an AST -> (AST + extra info) conversion for builtin functions that can't be simply
/// translated to Rust.
///
/// For example, the `print` function doesn't exist in Rust, we need to turn it into a log! macro
/// invocation. So the transform for this function will take a `print(...)` statement in AST,
/// generate a format string based on the number and type of args, and return AST for a log!.
///
/// Some (or maybe just one) types of transformations are special - for example, the Empty.init()
/// call is magic in that it disappears from the function body altogether, instead getting collected
/// in the instruction context.
#[derive(Clone)]
pub struct Transformation(pub Rc<Box<dyn Fn(TypedExpression) -> Result<Transformed, CoreError>>>);

impl Transformation {
    pub fn new<F>(f: F) -> Self
    where
        F: Fn(TypedExpression) -> Result<Transformed, CoreError> + 'static,
    {
        Self(Rc::new(Box::new(f)))
    }
}

impl std::fmt::Debug for Transformation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Transformation")
    }
}

impl PartialEq for Transformation {
    fn eq(&self, _other: &Self) -> bool {
        true
    }

    fn ne(&self, _other: &Self) -> bool {
        false
    }
}

#[derive(Clone, Debug)]
pub enum Transformed {
    Expression(TypedExpression),
    Cpi {
        expr: TypedExpression,
        program: AccountTyExpr,
    },
    AccountInit {
        expr: TypedExpression,
        name: String,
        annotation: AccountAnnotation,
    },
    Directive(Directive),
}

/// Build context for a single function.
struct Context<'a> {
    check_output: &'a CheckOutput,
    abs: &'a Vec<String>,
    ix_context: Option<InstructionContext>,
    directives: Option<Vec<Directive>>,
    expr_order: VecDeque<Ty>,
    assign_order: VecDeque<Assign>,
}

/// Create a finalized `TyExpr` based on a given `TyExpression` (a type expression defined in the
/// Seahorse code) and its typecheked `Ty`.
fn make_ty_expr(ty_expr: ast::TyExpression, ty: Ty) -> TyExpr {
    let is_mut = ty.is_mut();
    match (ty_expr.1, ty) {
        (ast::TyExpressionObj::Generic { base, params }, Ty::Generic(name, ty_params)) => {
            let mut params = params
                .into_iter()
                .zip(ty_params.into_iter())
                .map(|(ty_expr, ty)| make_ty_expr(ty_expr, ty))
                .collect::<Vec<_>>();

            let mutability = if is_mut {
                Mutability::Mutable
            } else {
                Mutability::Immutable
            };

            match name {
                TyName::Builtin(builtin) => match builtin {
                    // Tuple[...] -> (...)
                    Builtin::Python(Python::Tuple) => TyExpr::Tuple(params),
                    // None -> ()
                    Builtin::Python(Python::None) => TyExpr::Tuple(vec![]),
                    // Array[T, N] -> [T; N]
                    Builtin::Prelude(Prelude::Array) => {
                        let element = params.remove(0).into();
                        let size = params.remove(0).into();
                        TyExpr::Array { element, size }
                    }
                    // List[T] -> Vec<T>
                    Builtin::Python(Python::List) => TyExpr::Generic {
                        mutability,
                        name: vec!["Vec".to_string()],
                        params,
                    },
                    // str -> String
                    Builtin::Python(Python::Str) => TyExpr::Generic {
                        mutability,
                        name: vec!["String".to_string()],
                        params,
                    },
                    // Empty[T] -> Empty<T>
                    Builtin::Prelude(Prelude::Empty) => TyExpr::Generic {
                        mutability: Mutability::Immutable,
                        name: vec!["Empty".to_string()],
                        params,
                    },
                    // Signer -> SeahorseSigner<'entrypoint, 'info>
                    Builtin::Prelude(Prelude::Signer) => TyExpr::Generic {
                        mutability: Mutability::Immutable,
                        name: vec!["SeahorseSigner".to_string()],
                        params: vec![TyExpr::InfoLifetime, TyExpr::AnonLifetime],
                    },
                    // TokenMint -> SeahorseAccount<'entrypoint, 'info, Mint>>
                    Builtin::Prelude(Prelude::TokenMint) => TyExpr::Generic {
                        mutability: Mutability::Immutable,
                        name: vec!["SeahorseAccount".to_string()],
                        params: vec![
                            TyExpr::InfoLifetime,
                            TyExpr::AnonLifetime,
                            TyExpr::new_specific(vec!["Mint"], Mutability::Immutable),
                        ],
                    },
                    // TokenAccount -> SeahorseAccount<'entrypoint, 'info, TokenAccount>>
                    Builtin::Prelude(Prelude::TokenAccount) => TyExpr::Generic {
                        mutability: Mutability::Immutable,
                        name: vec!["SeahorseAccount".to_string()],
                        params: vec![
                            TyExpr::InfoLifetime,
                            TyExpr::AnonLifetime,
                            TyExpr::new_specific(vec!["TokenAccount"], Mutability::Immutable),
                        ],
                    },
                    // Program, UncheckedAccount -> UncheckedAccount<'info>
                    Builtin::Prelude(Prelude::Program | Prelude::UncheckedAccount) => {
                        TyExpr::Generic {
                            mutability: Mutability::Immutable,
                            name: vec!["UncheckedAccount".to_string()],
                            params: vec![TyExpr::InfoLifetime],
                        }
                    }
                    // Clock -> Sysvar<'info, Clock>
                    Builtin::Prelude(Prelude::Clock) => TyExpr::Generic {
                        mutability: Mutability::Immutable,
                        name: vec!["Sysvar".to_string()],
                        params: vec![
                            TyExpr::InfoLifetime,
                            TyExpr::new_specific(vec!["Clock"], Mutability::Immutable),
                        ],
                    },
                    // Everything else
                    builtin => TyExpr::Generic {
                        mutability,
                        name: vec![builtin.name()],
                        params,
                    },
                },
                TyName::Defined(_, DefinedType::Struct | DefinedType::Enum) => TyExpr::Generic {
                    mutability,
                    name: base,
                    params,
                },
                TyName::Defined(_, DefinedType::Account) => TyExpr::Account(base),
            }
        }
        (ast::TyExpressionObj::Const(..), Ty::Const(size)) => TyExpr::Const(size as usize),
        _ => panic!(),
    }
}

fn make_account_ty_expr(ty: Ty) -> AccountTyExpr {
    match ty {
        Ty::Generic(name, params) => match name {
            TyName::Builtin(Builtin::Prelude(builtin)) => match builtin {
                Prelude::Empty => {
                    AccountTyExpr::Empty(make_account_ty_expr(params[0].clone()).into())
                }
                Prelude::Signer => AccountTyExpr::Signer,
                Prelude::Program => AccountTyExpr::UncheckedAccount,
                Prelude::TokenMint => AccountTyExpr::TokenMint,
                Prelude::TokenAccount => AccountTyExpr::TokenAccount,
                Prelude::UncheckedAccount => AccountTyExpr::UncheckedAccount,
                Prelude::Clock => AccountTyExpr::ClockSysvar,
                _ => panic!(),
            },
            TyName::Defined(name, DefinedType::Account) => AccountTyExpr::Defined(name),
            _ => panic!(),
        },
        _ => panic!(),
    }
}

/// Turn an expression into a valid lval. Involves:
///     1. removing any `clone`s
///     2. descending into the data of a mutable object (attributes, indices)
///     3. replacing the first `borrow()` with a `borrow_mut()`.
fn make_lval(mut expression: TypedExpression) -> TypedExpression {
    expression.obj = match expression.obj {
        ExpressionObj::Move(value) => make_lval(*value).obj,
        ExpressionObj::Attribute { value, name } => ExpressionObj::Attribute {
            value: make_borrow_mut(*value).into(),
            name,
        },
        ExpressionObj::Index { value, index } => ExpressionObj::Index {
            value: make_borrow_mut(*value).into(),
            index,
        },
        obj => obj,
    };

    return expression;
}

fn make_borrow_mut(mut expression: TypedExpression) -> TypedExpression {
    expression.obj = match expression.obj {
        ExpressionObj::BorrowImmut(value) => ExpressionObj::BorrowMut(value),
        obj => obj,
    };

    return expression;
}

impl<'a> Context<'a> {
    fn build_func(
        &mut self,
        func: ast::FunctionDef,
        signature: FunctionSignature,
    ) -> CResult<Function> {
        let ast::FunctionDef {
            name,
            body,
            decorator_list,
            params: def_params,
            returns: def_returns,
        } = func;

        let mut ix_context = None;
        for Located(loc, _) in decorator_list.iter() {
            let decorator = self.expr_order.pop_front().unwrap();
            match decorator {
                Ty::Type(TyName::Builtin(Builtin::Prelude(prelude::Prelude::Instruction)), _) => {
                    let name = format!("{}", name.to_pascal_case());

                    let params = signature
                        .params
                        .iter()
                        .zip(def_params.params.iter())
                        .filter_map(|((name, ty, _), Located(_, ParamObj { annotation, .. }))| {
                            if !ty.is_account() {
                                Some((name.clone(), make_ty_expr(annotation.clone(), ty.clone())))
                            } else {
                                None
                            }
                        })
                        .collect();

                    let accounts = signature
                        .params
                        .iter()
                        .zip(def_params.params.iter())
                        .filter_map(|((name, ty, _), Located(_, ParamObj { annotation, .. }))| {
                            if ty.is_account() {
                                Some((
                                    name.clone(),
                                    ContextAccount {
                                        account_ty: make_account_ty_expr(ty.clone()),
                                        annotation: Some(AccountAnnotation::new()),
                                        ty: Some(make_ty_expr(annotation.clone(), ty.clone())),
                                    },
                                ))
                            } else {
                                None
                            }
                        })
                        .collect();

                    ix_context = Some(InstructionContext {
                        name,
                        params,
                        accounts,
                        inferred_accounts: HashMap::new(),
                    });
                }
                dec => {
                    return Err(Error::InvalidDecorator(dec).core(loc));
                }
            }
        }
        self.ix_context = ix_context;

        let params = signature
            .params
            .into_iter()
            .zip(def_params.params.into_iter())
            .map(|((name, ty, _), Located(_, ParamObj { annotation, .. }))| {
                (name, make_ty_expr(annotation, ty))
            })
            .collect::<Vec<_>>();

        let info_lifetime = params.iter().any(|(_, param)| param.has_info_lifetime());

        let returns = make_ty_expr(
            def_returns.unwrap_or(Located(
                // Location doesn't matter here I think
                // (if it does then whoopsies)
                Location::new(
                    &Rc::new("".to_string()),
                    rustpython_parser::location::Location::default(),
                ),
                ast::TyExpressionObj::Generic {
                    base: vec!["None".to_string()],
                    params: vec![],
                },
            )),
            signature.returns,
        );

        let body = self.build_block(body)?;

        return Ok(Function {
            ix_context: self.ix_context.take(),
            info_lifetime,
            name,
            params,
            returns,
            body,
        });
    }

    fn build_block(&mut self, block: Vec<ast::Statement>) -> CResult<Block> {
        let body = block
            .into_iter()
            .map(|statement| self.build_statement(statement))
            .collect::<Result<Vec<_>, CoreError>>()?;

        return Ok(Block {
            body,
            implicit_return: None,
        });
    }

    fn build_statement(&mut self, statement: ast::Statement) -> CResult<Statement> {
        let statement = match statement.1 {
            ast::StatementObj::Break => Statement::Break,
            ast::StatementObj::Continue => Statement::Continue,
            ast::StatementObj::Return { value } => {
                let value = value
                    .map(|value| self.build_expression(value))
                    .transpose()?;

                Statement::Return(value)
            }
            ast::StatementObj::Pass => Statement::Noop,
            ast::StatementObj::Assert { test, msg } => Statement::AnchorRequire {
                cond: self.build_expression(test)?,
                msg: self.build_expression(msg.unwrap())?,
            },
            ast::StatementObj::Assign { target, value } => {
                let assign = self.assign_order.pop_front().unwrap();
                match assign {
                    Assign::Mutate => {
                        let receiver = make_lval(self.build_expression(target)?);
                        let rval = self.build_expression(value)?;

                        if let TypedExpression {
                            obj: ExpressionObj::Index { value, index },
                            ..
                        } = receiver
                        {
                            Statement::IndexAssign {
                                receiver: *value,
                                index: *index,
                                value: rval,
                            }
                        } else {
                            Statement::Assign {
                                receiver,
                                value: rval,
                            }
                        }
                    }
                    Assign::Declare { undeclared, target } => Statement::Let {
                        undeclared,
                        target: self.build_target(target),
                        value: self.build_expression(value)?,
                    },
                    _ => panic!(),
                }
            }
            ast::StatementObj::OpAssign { target, op, value } => {
                let receiver = self.build_expression(target)?;
                Statement::Assign {
                    receiver: make_lval(receiver.clone()),
                    value: TypedExpression {
                        ty: Ty::Never,
                        // obj: ExpressionObj::BinOp {
                        //     left: receiver.into(),
                        //     op: self.build_op(op),
                        //     right: self.build_expression(value)?.into(),
                        // },
                        obj: {
                            let right = self.build_expression(value)?;

                            self.build_op(receiver, op, right)
                        },
                    },
                }
            }
            ast::StatementObj::TyAssign { value, .. } => {
                let assign = self.assign_order.pop_front().unwrap();
                let (undeclared, target) =
                    match1!(assign, Assign::Declare { undeclared, target } => (undeclared, target));

                // TODO might want to do something with the type info lol
                match value {
                    Some(value) => Statement::Let {
                        // TODO
                        undeclared,
                        target: self.build_target(target),
                        value: self.build_expression(value)?,
                    },
                    None => panic!(),
                }
            }
            ast::StatementObj::ExpressionObj { expression } => {
                Statement::Expression(self.build_expression(expression)?)
            }
            ast::StatementObj::If { test, body, orelse } => Statement::If {
                cond: self.build_expression(test)?,
                body: self.build_block(body)?,
                orelse: orelse.map(|block| self.build_block(block)).transpose()?,
            },
            ast::StatementObj::While { test, body } => Statement::While {
                cond: self.build_expression(test)?,
                body: self.build_block(body)?,
            },
            ast::StatementObj::For {
                target: _,
                iter,
                body,
            } => {
                let assign = self.assign_order.pop_front().unwrap();
                match assign {
                    Assign::Declare { target, .. } => Statement::For {
                        target: self.build_target(target),
                        iter: self.build_expression(iter)?,
                        body: self.build_block(body)?,
                    },
                    _ => panic!(),
                }
            }
        };

        return Ok(statement);
    }

    fn build_expression(&mut self, expression: ast::Expression) -> CResult<TypedExpression> {
        let Located(loc, obj) = expression;
        let expr_ty = self.expr_order.pop_front().unwrap();

        let obj = match obj {
            ast::ExpressionObj::BinOp { left, op, right } => {
                let left = self.build_expression(*left)?;
                let right = self.build_expression(*right)?;

                self.build_op(left, op, right)
            }
            ast::ExpressionObj::Index { value, index } => ExpressionObj::Index {
                value: {
                    let mut value = self.build_expression(*value)?;

                    if value.ty.is_mut() {
                        value.obj = ExpressionObj::BorrowImmut(value.obj.into());
                    }

                    value
                }
                .into(),
                index: self.build_expression(*index)?.into(),
            },
            ast::ExpressionObj::UnOp { op, value } => ExpressionObj::UnOp {
                op: match op {
                    ast::UnaryOperator::Pos => UnaryOperator::Pos,
                    ast::UnaryOperator::Neg => UnaryOperator::Neg,
                    ast::UnaryOperator::Not => UnaryOperator::Not,
                    ast::UnaryOperator::Inv => UnaryOperator::Inv,
                },
                value: self.build_expression(*value)?.into(),
            },
            ast::ExpressionObj::Attribute { value, name } => {
                let mut value = self.build_expression(*value)?;

                match &value.ty {
                    Ty::Type(..) | Ty::Path(..) => ExpressionObj::StaticAttribute {
                        value: value.into(),
                        name,
                    },
                    ty => {
                        if ty.is_mut() {
                            // Methods of custom types are impl'd on `Mutable<T>`, not `T` - this
                            // means we should not borrow the value if the attribute leads us to a
                            // function.
                            if let Ty::Function(..) = &expr_ty {
                            } else {
                                value.obj = ExpressionObj::BorrowImmut(value.obj.into());
                            }
                        }

                        ExpressionObj::Attribute {
                            value: value.into(),
                            name,
                        }
                    }
                }
            }
            ast::ExpressionObj::Call { function, args } => {
                let array_constructor_params = (
                    vec![
                        (
                            "iterable".to_string(),
                            Ty::Cast(Ty::python(Python::Iter, vec![Ty::Anonymous(0)]).into()),
                            ParamType::Required,
                        ),
                        ("len".to_string(), Ty::Any, ParamType::Required),
                    ],
                    vec![(
                        "elements".to_string(),
                        Ty::Anonymous(0),
                        ParamType::Variadic,
                    )],
                );

                let function = self.build_expression(*function)?;
                let params = match &function.ty {
                    Ty::Function(params, ..) => params,
                    Ty::Type(_, Some(constructor)) => match &**constructor {
                        Ty::Function(params, _) => params,
                        Ty::ArrayConstructor1 => &array_constructor_params.0,
                        _ => panic!(),
                    },
                    Ty::ArrayConstructor2 => &array_constructor_params.1,
                    _ => panic!(),
                };

                let order = order_args(&args, params, &loc)?;
                let mut args = vec![];
                for arg in order.into_iter() {
                    match arg {
                        OrderedArg::Pos(pos) => {
                            args.push(self.build_expression(pos.clone())?);
                        }
                        OrderedArg::Var(var) => {
                            let variadic = var
                                .into_iter()
                                .map(|arg| self.build_expression(arg.clone()))
                                .collect::<Result<Vec<_>, CoreError>>()?;

                            args.push(ExpressionObj::Vec(variadic).into());
                        }
                        OrderedArg::Kw(Some(kw)) => {
                            args.push(self.build_expression(kw.clone())?);
                        }
                        OrderedArg::Kw(None) => args.push(ExpressionObj::Placeholder.into()),
                    }
                }

                ExpressionObj::Call {
                    function: function.into(),
                    args,
                }
            }
            ast::ExpressionObj::Ternary { test, body, orelse } => ExpressionObj::Ternary {
                cond: Box::new(self.build_expression(*test)?),
                body: Box::new(self.build_expression(*body)?),
                orelse: Box::new(self.build_expression(*orelse)?),
            },
            ast::ExpressionObj::Int(n) => ExpressionObj::Literal(Literal::Int(n)),
            ast::ExpressionObj::Float(n) => ExpressionObj::Literal(Literal::Float(n)),
            ast::ExpressionObj::List(list) => ExpressionObj::Mutable(
                ExpressionObj::Vec(
                    list.into_iter()
                        .map(|element| self.build_expression(element))
                        .collect::<Result<Vec<_>, CoreError>>()?,
                )
                .into(),
            ),
            ast::ExpressionObj::Tuple(tuple) => ExpressionObj::Tuple(
                tuple
                    .into_iter()
                    .map(|element| self.build_expression(element))
                    .collect::<Result<Vec<_>, CoreError>>()?,
            ),
            ast::ExpressionObj::Comprehension { element, parts } => {
                let parts = parts
                    .into_iter()
                    .map(|part| match part {
                        ComprehensionPart::For { target: _, iter } => {
                            let assign = self.assign_order.pop_front().unwrap();
                            Ok(match assign {
                                Assign::Declare { target, .. } => Statement::For {
                                    target: self.build_target(target),
                                    iter: self.build_expression(iter)?,
                                    body: Block {
                                        body: vec![],
                                        implicit_return: None,
                                    },
                                },
                                _ => panic!(),
                            })
                        }
                        ComprehensionPart::If { cond } => Ok(Statement::If {
                            cond: self.build_expression(cond)?,
                            body: Block {
                                body: vec![],
                                implicit_return: None,
                            },
                            orelse: None,
                        }),
                    })
                    .collect::<Result<Vec<_>, CoreError>>()?;

                let element = self.build_expression(*element)?;

                let temp: TypedExpression = ExpressionObj::Id("temp".to_string()).into();

                // temp.push(element);
                let mut comprehension = Statement::Expression(
                    ExpressionObj::Call {
                        function: ExpressionObj::Attribute {
                            value: temp.clone().into(),
                            name: "push".to_string(),
                        }
                        .into(),
                        args: vec![element],
                    }
                    .into(),
                );

                for mut part in parts.into_iter().rev() {
                    match &mut part {
                        Statement::For { body, .. } | Statement::If { body, .. } => {
                            body.body.push(comprehension);
                        }
                        _ => panic!(),
                    }

                    comprehension = part;
                }

                // {
                //     let temp = vec![];
                //     (do comprehension, adding elements to temp)
                //     temp
                // }
                let block = ExpressionObj::Block(Block {
                    body: vec![
                        Statement::Let {
                            undeclared: vec![],
                            target: LetTarget::Var {
                                name: "temp".to_string(),
                                is_mut: true,
                            },
                            value: ExpressionObj::Vec(vec![]).into(),
                        },
                        comprehension,
                    ],
                    implicit_return: Some(temp.clone().into()),
                });

                ExpressionObj::Rendered(quote! {
                    Mutable::new(#block)
                })
            }
            ast::ExpressionObj::Str(s) => ExpressionObj::Literal(Literal::Str(s)),
            ast::ExpressionObj::FStr { parts } => {
                let mut format = String::from("");
                let mut parts_ = vec![];

                for part in parts.into_iter() {
                    match part {
                        ast::FStrPart::ExpressionObj(expr) => {
                            let part = self.build_expression(expr)?;

                            if part.ty.is_display() {
                                format.push_str("{}");
                            } else {
                                format.push_str("{:?}");
                            }

                            parts_.push(part);
                        }
                        ast::FStrPart::Str(string) => {
                            format.push_str(&string);
                        }
                    }
                }

                ExpressionObj::Rendered(quote! {
                    format!(#format, #(#parts_),*)
                })
            }
            ast::ExpressionObj::Id(name) => ExpressionObj::Id(name),
            ast::ExpressionObj::Bool(p) => ExpressionObj::Literal(Literal::Bool(p)),
            ast::ExpressionObj::None => ExpressionObj::Literal(Literal::Unit),
            ast::ExpressionObj::Iter { value } => {
                let TypedExpression { obj, .. } = self.build_expression(*value)?;
                obj
            }
        };

        let expression = TypedExpression {
            ty: expr_ty.clone(),
            obj,
        };
        let expression = self.transform(expression, &loc)?;

        return Ok(expression);
    }

    /// Perform transformations on an expression. Can perform transformations with an explicit
    /// `Transformation`.
    fn transform(
        &mut self,
        mut expression: TypedExpression,
        loc: &Location,
    ) -> CResult<TypedExpression> {
        let transformation;
        (expression.ty, transformation) = match expression.ty {
            Ty::Transformed(ty, transformation) => (*ty, Some(transformation)),
            ty => (ty, None),
        };

        if let Some(transformation) = transformation {
            let transformed =
                transformation.0(expression).map_err(|err| err.located(loc.clone()))?;

            let expression = match transformed {
                Transformed::Expression(expression) => Ok(expression),
                Transformed::Cpi { expr, program } => {
                    if let Some(ix_context) = &mut self.ix_context {
                        let name = match &program {
                            AccountTyExpr::SystemProgram => "system_program",
                            AccountTyExpr::TokenProgram => "token_program",
                            _ => panic!(),
                        }
                        .to_string();

                        ix_context.inferred_accounts.insert(
                            name,
                            ContextAccount {
                                account_ty: program,
                                annotation: None,
                                ty: None,
                            },
                        );
                    } else {
                        return Err(Error::MisplacedCpi.core(loc));
                    }

                    Ok(expr)
                }
                Transformed::AccountInit {
                    expr: expression,
                    name,
                    annotation,
                } => {
                    if let Some(ix_context) = &mut self.ix_context {
                        let index = ix_context
                            .accounts
                            .iter()
                            .position(|(name_, ..)| &name == name_)
                            .unwrap();

                        let is_associated = annotation.is_associated;

                        let account = &mut ix_context.accounts.get_mut(index).unwrap().1;
                        account.annotation = Some(annotation);

                        ix_context.inferred_accounts.insert(
                            "system_program".to_string(),
                            ContextAccount {
                                account_ty: AccountTyExpr::SystemProgram,
                                annotation: None,
                                ty: None,
                            },
                        );
                        ix_context.inferred_accounts.insert(
                            "rent".to_string(),
                            ContextAccount {
                                account_ty: AccountTyExpr::RentSysvar,
                                annotation: None,
                                ty: None,
                            },
                        );

                        // Token program is needed to init its own accounts
                        if let AccountTyExpr::Empty(account_ty) = &account.account_ty {
                            if let AccountTyExpr::TokenMint | AccountTyExpr::TokenAccount =
                                &**account_ty
                            {
                                ix_context.inferred_accounts.insert(
                                    "token_program".to_string(),
                                    ContextAccount {
                                        account_ty: AccountTyExpr::TokenProgram,
                                        annotation: None,
                                        ty: None,
                                    },
                                );
                            }
                        }

                        // Might need the associated token program as well
                        if is_associated {
                            ix_context.inferred_accounts.insert(
                                "associated_token_program".to_string(),
                                ContextAccount {
                                    account_ty: AccountTyExpr::AssociatedTokenProgram,
                                    annotation: None,
                                    ty: None,
                                },
                            );
                        }

                        Ok(expression)
                    } else {
                        Err(Error::MisplacedInit.core(loc))
                    }
                }
                Transformed::Directive(directive) => {
                    if let Some(directives) = &mut self.directives {
                        directives.push(directive);
                        Ok(ExpressionObj::Placeholder.into())
                    } else {
                        Err(Error::MisplacedDirective.core(loc))
                    }
                }
            }?;

            // Might be multiple transformations
            self.transform(expression, loc)
        } else {
            Ok(expression)
        }
    }

    fn build_op(
        &self,
        left: TypedExpression,
        op: ast::Operator,
        right: TypedExpression,
    ) -> ExpressionObj {
        let op = match op {
            ast::Operator::Add => Operator::Add,
            ast::Operator::Sub => Operator::Sub,
            ast::Operator::Mul => Operator::Mul,
            ast::Operator::Div => Operator::Div,
            ast::Operator::Mod => Operator::Mod,
            ast::Operator::LShift => Operator::LShift,
            ast::Operator::RShift => Operator::RShift,
            ast::Operator::BitOr => Operator::BitOr,
            ast::Operator::BitXor => Operator::BitXor,
            ast::Operator::BitAnd => Operator::BitAnd,
            ast::Operator::FloorDiv => Operator::Div,
            ast::Operator::And => Operator::And,
            ast::Operator::Or => Operator::Or,
            ast::Operator::Eq => Operator::Eq,
            ast::Operator::NotEq => Operator::NotEq,
            ast::Operator::Lt => Operator::Lt,
            ast::Operator::Lte => Operator::Lte,
            ast::Operator::Gt => Operator::Gt,
            ast::Operator::Gte => Operator::Gte,
            ast::Operator::Pow => {
                return if left.ty == Ty::prelude(Prelude::RustFloat, vec![]) {
                    ExpressionObj::Rendered(quote! { #left.powf(#right) })
                } else {
                    ExpressionObj::Rendered(quote! { #left.pow(#right) })
                };
            }
            ast::Operator::In => todo!(),
            ast::Operator::NotIn => todo!(),
        };

        ExpressionObj::BinOp {
            left: left.into(),
            op,
            right: right.into(),
        }
    }

    fn build_target(&self, target: Target) -> LetTarget {
        match target {
            Target::Var(var) => LetTarget::Var {
                name: var,
                is_mut: true,
            },
            Target::Tuple(tuple) => LetTarget::Tuple(
                tuple
                    .into_iter()
                    .map(|target| self.build_target(target))
                    .collect(),
            ),
        }
    }
}

pub struct BuildOutput {
    pub tree: Tree<Artifact>,
    pub origin: Vec<String>,
}

impl TryFrom<CheckOutput> for BuildOutput {
    type Error = CoreError;

    fn try_from(check_output: CheckOutput) -> CResult<Self> {
        // Might be the ugliest expression I've ever written
        let tree = check_output.tree.clone().zip(
            check_output
                .sign_output
                .tree
                .clone()
                .zip(check_output.sign_output.namespace_output.tree.clone()),
        );

        let mut tree = tree
            .map_with_path(|(mut contexts, (mut signatures, namespace)), path| {
                let mut artifact = Artifact {
                    uses: vec![Use { rooted: true, tree: Tree::Node(HashMap::new()) }],
                    directives: vec![],
                    constants: vec![],
                    type_defs: vec![],
                    functions: vec![],
                };

                for (name, export) in namespace.into_iter() {
                    match export {
                        Export::Import(Located(_, ImportObj::SymbolPath(mut path) | ImportObj::ModulePath(mut path) | ImportObj::PackagePath(mut path))) => {
                            let mut node = match1!(artifact.uses.get_mut(0), Some(Use { tree: Tree::Node(node), .. }) => node);
                            let last = path.pop().unwrap();

                            for part in path.into_iter() {
                                match node.get_mut(&part) {
                                    Some(Tree::Node(..)) => {}
                                    None => {
                                        node.insert(part.clone(), Tree::Node(HashMap::new()));
                                    }
                                    Some(Tree::Leaf(alias)) => {
                                        let new_node = Tree::Node(HashMap::from([("self".to_string(), Tree::Leaf(alias.clone()))]));
                                        node.insert(part.clone(), new_node);
                                    }
                                }

                                node = match1!(node.get_mut(&part), Some(Tree::Node(node)) => node);
                            }

                            node.insert(last, Tree::Leaf(None));
                        }
                        Export::Item(item) => {
                            match item {
                                Item::Builtin(..) => {}
                                Item::Defined(defined) => {
                                    let mut context = contexts.remove(&name).unwrap();
                                    let mut signature = signatures.remove(&name).unwrap();

                                    match defined {
                                        Located(
                                            _,
                                            ast::TopLevelStatementObj::ClassDef {
                                                mut name,
                                                body,
                                                ..
                                            },
                                        ) => match signature {
                                            Signature::Class(ClassSignature::Struct(
                                                StructSignature {
                                                    is_account, fields: mut fields_map, methods: mut methods_map, ..
                                                },
                                            )) => {
                                                let mut fields = vec![];
                                                let mut methods = vec![];
                                                let mut constructor = None;

                                                for Located(_, statement) in body.into_iter() {
                                                    match statement {
                                                        ast::ClassDefStatementObj::FieldDef { name, ty: Some(ty_expr), .. } => {
                                                            let ty = fields_map.remove(&name).unwrap();
                                                            fields.push((
                                                                name,
                                                                make_ty_expr(ty_expr, ty.clone()),
                                                                ty
                                                            ));
                                                        },
                                                        ast::ClassDefStatementObj::MethodDef(func) => {
                                                            let typecheck = match1!(context, FinalContext::Class(ref mut typechecks) => typechecks.remove(&func.name).unwrap());

                                                            let mut context = Context {
                                                                check_output: &check_output,
                                                                abs: &path,
                                                                ix_context: None,
                                                                directives: None,
                                                                expr_order: typecheck.expr_order.into(),
                                                                assign_order: typecheck.assign_order.into(),
                                                            };
                                                            let (method_type, signature) = methods_map.remove(&func.name).unwrap();
                                                            let func = context.build_func(func, signature)?;

                                                            if &func.name == "__init__" {
                                                                constructor = Some(func);
                                                            } else {
                                                                methods.push((method_type, func));
                                                            }
                                                        },
                                                        _ => {}
                                                    }
                                                }

                                                let type_def = if is_account {
                                                    TypeDef::Account(Account {
                                                        name: name.clone(),
                                                        fields,
                                                        methods
                                                    })
                                                } else {
                                                    TypeDef::Struct(Struct {
                                                        name,
                                                        fields: fields.into_iter().map(|(name, ty, _)| (name, ty)).collect(),
                                                        methods,
                                                        constructor
                                                    })
                                                };

                                                artifact.type_defs.push(type_def);
                                            }
                                            Signature::Class(ClassSignature::Enum(
                                                EnumSignature { variants },
                                            )) => {
                                                artifact.type_defs.push(TypeDef::Enum(Enum {
                                                    name,
                                                    variants: variants
                                                        .into_iter()
                                                        .map(|(name, _)| (name, None))
                                                        .collect(),
                                                }));
                                            }
                                            _ => panic!(),
                                        },
                                        Located(
                                            _,
                                            ast::TopLevelStatementObj::FunctionDef(func),
                                        ) => {
                                            let typecheck = match1!(context, FinalContext::Function(typecheck) => typecheck);

                                            let mut context = Context {
                                                check_output: &check_output,
                                                abs: &path,
                                                ix_context: None,
                                                directives: None,
                                                expr_order: typecheck.expr_order.into(),
                                                assign_order: typecheck.assign_order.into(),
                                            };
                                            let signature = match signature {
                                                Signature::Function(signature) => signature,
                                                _ => panic!(),
                                            };
                                            let func = context.build_func(func, signature)?;

                                            artifact.functions.push(func);
                                        }
                                        _ => {}
                                    }
                                }
                            }
                        }
                    }
                }

                let directives = match1!(contexts.remove(""), Some(FinalContext::Directives(directives)) => directives);
                for (expression, typecheck) in directives.into_iter() {
                    let mut context = Context {
                        check_output: &check_output,
                        abs: &path,
                        ix_context: None,
                        directives: Some(vec![]),
                        expr_order: typecheck.expr_order.into(),
                        assign_order: typecheck.assign_order.into()
                    };

                    let loc = expression.0.clone();
                    context.build_expression(expression)?;

                    if let Some(mut directives) = context.directives.take() {
                        artifact.directives.append(&mut directives);
                    } else {
                        return Err(Error::TopLevelNonDirective.core(&loc));
                    }
                }

                artifact.uses[0].tree.remove(&vec!["sh".to_string()]);

                return Ok(artifact);
            })
            .transpose()?;

        if let Some(Tree::Node(node)) = tree.get_mut(&vec!["dot".to_string()]) {
            node.remove("seahorse");
        }

        return Ok(BuildOutput {
            tree,
            origin: check_output
                .sign_output
                .namespace_output
                .preprocessed
                .origin,
        });
    }
}

pub fn build(checked: CheckOutput) -> CResult<BuildOutput> {
    checked.try_into()
}
