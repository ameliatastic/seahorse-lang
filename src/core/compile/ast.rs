use proc_macro2::TokenStream;

use super::{
    super::generate::Feature,
    builtin::{
        prelude::MethodType,
        pyth::{ExprContext, ExprContextStack},
    },
    check::Ty,
};
use crate::core::Tree;
use std::collections::{BTreeMap, BTreeSet};

/// A compilation artifact. Does not handle lib.rs, which will be generated separately.
///
/// Collects and propagates features to make generation a bit easier.
#[derive(Clone, Debug)]
pub struct Artifact {
    pub features: BTreeSet<Feature>,
    pub uses: Vec<Use>,
    pub directives: Vec<Directive>,
    pub constants: Vec<Constant>,
    pub type_defs: Vec<TypeDef>,
    pub functions: Vec<Function>,
}

/// A `use` statement.
#[derive(Clone, Debug)]
pub struct Use {
    pub rooted: bool,
    pub tree: Tree<Option<String>>,
}

/// Special compiler control statements (e.g. declare_id).
#[derive(Clone, Debug)]
pub enum Directive {
    DeclareId(String),
}

/// A `const` declaration.
#[derive(Clone, Debug)]
pub struct Constant {
    pub name: String,
    pub value: TypedExpression,
}

/// A type definition.
#[derive(Clone, Debug)]
pub enum TypeDef {
    Struct(Struct),
    Account(Account),
    // Context(Context),
    Enum(Enum),
}

/// A `struct` definition.
#[derive(Clone, Debug)]
pub struct Struct {
    pub name: String,
    pub fields: Vec<(String, TyExpr, Ty)>,
    pub methods: Vec<(MethodType, Function)>,
    pub constructor: Option<Function>,
    pub is_event: bool,
    pub is_dataclass: bool,
}

/// An Anchor account definition.
#[derive(Clone, Debug)]
pub struct Account {
    pub name: String,
    // Save type info for generation later
    pub fields: Vec<(String, TyExpr, Ty)>,
    pub methods: Vec<(MethodType, Function)>,
}

/// An `enum` definition.
#[derive(Clone, Debug)]
pub struct Enum {
    pub name: String,
    // NOTE: saving some space for the future when enums variants might have data
    pub variants: Vec<(String, Option<TyExpr>)>,
}

/// An expression that resolves to a Rust type. `Generic` types include whether their Seahorse type
/// is mutable. `Array`s are always mutable, `Tuple`s are always immutable.
#[derive(Clone, Debug)]
pub enum TyExpr {
    Generic {
        mutability: Mutability,
        name: Vec<String>,
        params: Vec<TyExpr>,
        is_loadable: bool,
    },
    Array {
        element: Box<TyExpr>,
        size: Box<TyExpr>,
    },
    Tuple(Vec<TyExpr>),
    Account(Vec<String>), // Type expression explicitly for defined accounts
    Const(usize),
    InfoLifetime,
    AnonLifetime,
}

#[derive(Clone, Debug)]
pub enum Mutability {
    Mutable,
    Immutable,
}

impl TyExpr {
    /// Make a "specific" type (as opposed to a generic) - a type with no params.
    pub fn new_specific(name: Vec<&str>, mutability: Mutability) -> Self {
        Self::Generic {
            mutability,
            name: name.into_iter().map(|part| part.to_string()).collect(),
            params: vec![],
            is_loadable: false,
        }
    }

    pub fn has_info_lifetime(&self) -> bool {
        match self {
            Self::Generic { params, .. } => params.iter().any(|param| param.has_info_lifetime()),
            Self::Array { element, .. } => element.has_info_lifetime(),
            Self::Tuple(tuple) => tuple.iter().any(|part| part.has_info_lifetime()),
            Self::InfoLifetime { .. } => true,
            Self::Account(..) => true,
            _ => false,
        }
    }
}

/// An `fn` definition.
#[derive(Clone, Debug)]
pub struct Function {
    pub ix_context: Option<InstructionContext>,
    pub name: String,
    pub info_lifetime: bool,
    pub params: Vec<(String, TyExpr)>,
    pub returns: TyExpr,
    pub body: Block,
}

/// An Anchor instruction context definition.
#[derive(Clone, Debug)]
pub struct InstructionContext {
    pub name: String,
    pub params: Vec<(String, TyExpr)>,
    pub accounts: Vec<(String, ContextAccount)>,
    pub inferred_accounts: BTreeMap<String, ContextAccount>,
}

#[derive(Clone, Debug)]
pub struct ContextAccount {
    pub account_ty: AccountTyExpr,
    pub annotation: Option<AccountAnnotation>,
    pub ty: Option<TyExpr>,
}

/// A type expression specfically for accounts.
#[derive(Clone, Debug)]
pub enum AccountTyExpr {
    Empty(Box<AccountTyExpr>),
    Defined(Vec<String>),
    Signer,
    TokenMint,
    TokenAccount,
    UncheckedAccount,
    SystemProgram,
    TokenProgram,
    AssociatedTokenProgram,
    RentSysvar,
    ClockSysvar,
}

impl AccountTyExpr {
    pub fn is_program(&self) -> bool {
        match self {
            Self::SystemProgram | Self::TokenProgram | Self::AssociatedTokenProgram => true,
            _ => false,
        }
    }
}

/// Content of an Anchor account annotation (#[account(...)]).
#[derive(Clone, Debug)]
pub struct AccountAnnotation {
    pub is_mut: bool,
    pub is_associated: bool,
    pub init: bool,
    pub payer: Option<TypedExpression>,
    pub seeds: Option<Vec<TypedExpression>>,
    pub mint_decimals: Option<TypedExpression>,
    pub mint_authority: Option<TypedExpression>,
    pub token_mint: Option<TypedExpression>,
    pub token_authority: Option<TypedExpression>,
    pub space: Option<TypedExpression>,
    pub padding: Option<TypedExpression>,
}

impl AccountAnnotation {
    pub fn new() -> Self {
        Self {
            is_mut: true,
            is_associated: false,
            init: false,
            payer: None,
            seeds: None,
            mint_decimals: None,
            mint_authority: None,
            token_mint: None,
            token_authority: None,
            space: None,
            padding: None,
        }
    }
}

/// A block of code - multiple statements optionally followed by an implicit return.
#[derive(Clone, Debug)]
pub struct Block {
    pub body: Vec<Statement>,
    pub implicit_return: Option<Box<TypedExpression>>,
}

/// A single statement in a function.
#[derive(Clone, Debug)]
pub enum Statement {
    Let {
        undeclared: Vec<String>,
        target: LetTarget,
        value: TypedExpression,
    },
    Assign {
        receiver: TypedExpression,
        value: TypedExpression,
    },
    Expression(TypedExpression),
    Return(Option<TypedExpression>),
    Break,
    Continue,
    Noop,
    AnchorRequire {
        cond: TypedExpression,
        msg: TypedExpression,
    },
    If {
        cond: TypedExpression,
        body: Block,
        orelse: Option<Block>,
    },
    While {
        cond: TypedExpression,
        body: Block,
    },
    Loop {
        label: Option<String>,
        body: Block,
    },
    For {
        target: LetTarget,
        iter: TypedExpression,
        body: Block,
    },
}

/// Let-bindable target.
#[derive(Clone, Debug)]
pub enum LetTarget {
    Var { name: String, is_mut: bool },
    Tuple(Vec<LetTarget>),
}

impl LetTarget {
    pub fn as_immut(&self) -> Self {
        match self {
            Self::Var { name, .. } => Self::Var {
                name: name.clone(),
                is_mut: false,
            },
            Self::Tuple(tuple) => Self::Tuple(tuple.iter().map(|part| part.as_immut()).collect()),
        }
    }
}

/// Expression + type, types are saved so that transformations can occur when necessary.
#[derive(Clone, Debug)]
pub struct TypedExpression {
    pub ty: Ty,
    pub obj: ExpressionObj,
}

impl TypedExpression {
    /// Get the result of an optional argument - if it's a placeholder, return `None`.
    pub fn optional(self) -> Option<Self> {
        match &self.obj {
            ExpressionObj::Placeholder => None,
            _ => Some(self),
        }
    }

    /// Add a move to the expression, if appropriate to do so.
    pub fn moved(mut self, context_stack: &ExprContextStack) -> Self {
        if !context_stack.has_any(&[ExprContext::Directive, ExprContext::Seed]) {
            self.obj = ExpressionObj::Move(self.obj.into());
        }

        return self;
    }

    /// Remove the borrows from this expression.
    pub fn without_borrows(mut self) -> Self {
        self.obj = self.obj.without_borrows();

        return self;
    }
}

impl From<ExpressionObj> for TypedExpression {
    fn from(obj: ExpressionObj) -> Self {
        Self { ty: Ty::Never, obj }
    }
}

impl From<ExpressionObj> for Box<TypedExpression> {
    fn from(obj: ExpressionObj) -> Self {
        TypedExpression { ty: Ty::Never, obj }.into()
    }
}

/// A single expression.
#[derive(Clone, Debug)]
pub enum ExpressionObj {
    BinOp {
        left: Box<TypedExpression>,
        op: Operator,
        right: Box<TypedExpression>,
    },
    Index {
        value: Box<TypedExpression>,
        index: Box<TypedExpression>,
    },
    TupleIndex {
        tuple: Box<TypedExpression>,
        index: usize,
    },
    UnOp {
        op: UnaryOperator,
        value: Box<TypedExpression>,
    },
    Attribute {
        value: Box<TypedExpression>,
        name: String,
    },
    StaticAttribute {
        value: Box<TypedExpression>,
        name: String,
    },
    Call {
        function: Box<TypedExpression>,
        args: Vec<TypedExpression>,
    },
    Ternary {
        cond: Box<TypedExpression>,
        body: Box<TypedExpression>,
        orelse: Box<TypedExpression>,
    },
    As {
        value: Box<TypedExpression>,
        ty: TyExpr,
    },
    Vec(Vec<TypedExpression>),
    Array(Vec<TypedExpression>),
    Tuple(Vec<TypedExpression>),
    Id(String),
    Literal(Literal),
    Block(Block),
    Ref(Box<TypedExpression>),
    // Indicator that an expression is being moved and may need a clone
    Move(Box<TypedExpression>),
    // Indicator that an expression resolves to a mutable type that needs to be mutably
    // borrowed
    BorrowMut(Box<TypedExpression>),
    // Indicator that an expression resolves to a mutable type that needs to be immutably
    // borrowed
    BorrowImmut(Box<TypedExpression>),
    // Indicator that an expression is a "raw" (unshared) mutable type that needs to be wrapped
    // in an Rc<RefCell<_>>
    Mutable(Box<TypedExpression>),
    // Expression that is just directly rendered into tokens. Useful for making transformations
    // easier to write
    Rendered(TokenStream),
    // Expression that comes from an unused keyword arg
    Placeholder,
}

impl ExpressionObj {
    pub fn with_call(self, name: &str, args: Vec<TypedExpression>) -> Self {
        ExpressionObj::Call {
            function: ExpressionObj::Attribute {
                value: self.into(),
                name: name.into(),
            }
            .into(),
            args,
        }
    }

    /// Return whether this expression refers to some owned data. This is true
    /// when we're referring directly to a variable (`Id`), or some part of a
    /// value (attributes or indices).
    pub fn is_owned(&self) -> bool {
        match self {
            Self::Attribute { .. }
            | Self::Id(..)
            | Self::Index { .. }
            | Self::TupleIndex { .. } => true,
            _ => false,
        }
    }

    pub fn without_borrows(self) -> Self {
        match self {
            Self::BinOp { left, op, right } => Self::BinOp {
                left: left.without_borrows().into(),
                op,
                right: right.without_borrows().into(),
            },
            Self::Index { value, index } => Self::Index {
                value: value.without_borrows().into(),
                index: index.without_borrows().into(),
            },
            Self::TupleIndex { tuple, index } => Self::TupleIndex {
                tuple: tuple.without_borrows().into(),
                index,
            },
            Self::UnOp { op, value } => Self::UnOp {
                op,
                value: value.without_borrows().into(),
            },
            Self::Attribute { value, name } => Self::Attribute {
                value: value.without_borrows().into(),
                name,
            },
            Self::StaticAttribute { value, name } => Self::StaticAttribute {
                value: value.without_borrows().into(),
                name,
            },
            Self::Call { function, args } => Self::Call {
                function: function.without_borrows().into(),
                args: args.into_iter().map(|arg| arg.without_borrows()).collect(),
            },
            Self::Ternary { cond, body, orelse } => Self::Ternary {
                cond: cond.without_borrows().into(),
                body: body.without_borrows().into(),
                orelse: orelse.without_borrows().into(),
            },
            Self::As { value, ty } => Self::As {
                value: value.without_borrows().into(),
                ty,
            },
            Self::Vec(elements) => Self::Vec(
                elements
                    .into_iter()
                    .map(|element| element.without_borrows())
                    .collect(),
            ),
            Self::Array(elements) => Self::Array(
                elements
                    .into_iter()
                    .map(|element| element.without_borrows())
                    .collect(),
            ),
            Self::Tuple(elements) => Self::Tuple(
                elements
                    .into_iter()
                    .map(|element| element.without_borrows())
                    .collect(),
            ),
            Self::Ref(value) => Self::Ref(value.without_borrows().into()),
            Self::Move(value) => Self::Move(value.without_borrows().into()),
            Self::BorrowMut(value) | Self::BorrowImmut(value) => value.without_borrows().obj,
            Self::Mutable(value) => Self::Mutable(value.without_borrows().into()),
            // Deliberately skipping `Block` for now, there's no good way to map the body of a
            // block to a body with no borrows
            obj => obj,
        }
    }
}

/// A literal expression, separated so that expressions aren't cluttered.
#[derive(Clone, Debug)]
pub enum Literal {
    Int(i128),
    Float(f64),
    Str(String),
    Bool(bool),
    Unit,
}

/// A binary operator.
#[derive(Clone, Debug)]
pub enum Operator {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    // Pow,
    LShift,
    RShift,
    BitOr,
    BitXor,
    BitAnd,
    And,
    Or,
    Eq,
    NotEq,
    Lt,
    Lte,
    Gt,
    Gte,
}

/// A unary operator.
#[derive(Clone, Debug)]
pub enum UnaryOperator {
    Pos,
    Neg,
    Not,
    Inv,
}
