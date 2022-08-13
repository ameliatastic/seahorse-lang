//! Defines a syntax tree that we can actually convert to Rust + Anchor code.

use crate::core::python_ast::Location;
use std::fmt;

/// An entire Seahorse program.
#[derive(Debug)]
pub struct Program {
    pub id: String,
    // pub imports: Vec<()>, // using this as a placeholder, might want to actually save the data
    pub instructions: Vec<Instruction>,
    pub errors: Vec<(String, String)>,
    pub defs: Vec<Def>,
}

/// A top-level definition in a Seahorse program.
#[derive(Debug)]
pub enum Def {
    TyDef(TyDef),
    FunctionDef(FunctionDef),
    RawInstructionDef(FunctionDef),
    RawDeclareId(String),
    RawImport,
}

#[derive(Debug)]
pub struct Instruction {
    pub name: String,
    pub context_name: String,
    pub accounts_context: AccountsContext,
    pub params: Vec<Param>,
    pub handler_name: String,
}

#[derive(Clone, Debug)]
pub struct AccountsContext {
    pub accounts: Vec<Account>,
    pub params: Vec<(String, Ty)>,
}

/// A defined type.
#[derive(Clone, Debug)]
pub enum TyDef {
    Struct {
        name: String,
        fields: Vec<Field>,
        traits: Vec<TraitName>,
    },
    Enum {
        name: String,
        options: Vec<String>,
    },
}

/// A field of a struct.
#[derive(Clone, Debug, PartialEq)]
pub struct Field {
    pub name: String,
    pub ty: Ty,
}

/// A parameter for a function.
#[derive(Clone, Debug, PartialEq)]
pub struct Param {
    pub name: String,
    pub ty: Ty,
    pub required: bool,
}

/// A defined function.
#[derive(Debug)]
pub struct FunctionDef {
    pub name: String,
    pub params: Vec<Param>,
    pub returns: Ty,
    pub body: Vec<Statement>,
}

/// A context-usable account.
#[derive(Clone, Debug)]
pub struct Account {
    pub name: String,
    pub account_type: Ty,
    pub init: Option<AccountInit>,
}

#[derive(Clone, Debug)]
pub enum AccountInit {
    Program {
        account_type: Ty,
        payer: String,
        seeds: Vec<Expression>,
    },
    TokenAccount {
        payer: String,
        seeds: Vec<Expression>,
        mint: String,
        authority: String,
    },
    TokenMint {
        payer: String,
        seeds: Vec<Expression>,
        decimals: u8,
        authority: String,
    },
    AssociatedTokenAccount {
        payer: String,
        mint: String,
        authority: String,
    },
}

/// A statement that can be translated to Rust.
///
/// Specifically, these statements may live inside functions. Python defines
/// extra things as statements, like function defs/class defs - we can't have
/// that in Rust, so they don't belong in this enum.
#[derive(Clone, Debug, PartialEq)]
pub enum Statement {
    Break,
    Continue,
    Return {
        value: Option<Expression>,
    },
    ReturnOk,
    Assign {
        left: Expression,
        right: Expression,
    },
    OpAssign {
        left: Expression,
        op: Operator,
        right: Expression,
    },
    Expression {
        value: Expression,
    },
    If {
        cond: Expression,
        body: Vec<Statement>,
        or_else: Option<Vec<Statement>>,
    },
    While {
        cond: Expression,
        body: Vec<Statement>,
    },
    ForIn {
        target: Pattern,
        iter: Expression,
        body: Vec<Statement>,
    },
    Noop,
    Declare {
        name: String,
        ty: Option<Ty>,
        init: Expression,
    },
    DeclareFromContext {
        name: String,
        ty: Ty,
    },
    Require {
        cond: Expression,
        throw: String,
    },
    RawAssign {
        left: Expression,
        ty: Option<Ty>,
        right: Option<Expression>,
        location: Location,
    },
    RawExpression {
        value: Expression,
        location: Location,
    },
    RawAssert {
        cond: Expression,
        msg: String,
        location: Location,
    },
}

/// An enum that represents usable Rust/Anchor types and traits.
#[derive(Clone, Debug, PartialEq)]
pub enum Ty {
    // Prelude (builtin) types
    U8,
    _U32, // Not supported but needed for generation
    U64,
    I64,
    F64,
    Bool,
    String,
    Unit,
    Tuple(Vec<Ty>),
    List(Box<Ty>),
    Array(Box<Ty>, TyParam),
    Iter(Box<Ty>),
    Function {
        params: Vec<Param>,
        returns: Box<Ty>,
    },
    Union(Vec<Ty>),
    Pubkey,
    Signer,
    Empty(Box<Ty>),
    TokenAccount,
    TokenMint,
    AssociatedTokenAccount,
    SystemProgram,
    TokenProgram,
    AssociatedTokenProgram,
    Rent,
    ProgramResult,
    Context(String),
    // Other types
    Defined(String),
    ExactDefined {
        name: String,
        is_mut: bool,
        is_acc: bool,
    },
    DefinedName(String),
    Never,
    Any,
}

/// Numeric parameters for types.
#[derive(Clone, Debug, PartialEq)]
pub enum TyParam {
    Exact(usize),
    Any,
}

#[derive(Clone, Debug, PartialEq)]
pub enum TraitName {
    Enum,
    Account,
    Instruction,
}

/// An expression.
#[derive(Clone, Debug, PartialEq)]
pub enum Expression {
    BinOp {
        left: Box<Expression>,
        op: Operator,
        right: Box<Expression>,
    },
    Attribute {
        value: Box<Expression>,
        attr: String,
    },
    StaticAttribute {
        value: Box<Expression>,
        attr: String,
    },
    Index {
        value: Box<Expression>,
        index: Box<Expression>,
    },
    UnOp {
        op: UnaryOperator,
        value: Box<Expression>,
    },
    Call {
        func: Box<Expression>,
        args: Vec<Expression>,
    },
    Range {
        from: Box<Expression>,
        to: Box<Expression>,
    },
    Comprehension {
        element: Box<Expression>,
        parts: Vec<ComprehensionPart>,
    },
    As {
        value: Box<Expression>,
        as_type: Ty,
    },
    Coerce {
        value: Box<Expression>,
        as_type: Ty,
    },
    Block {
        body: Vec<Statement>,
        returns: Box<Expression>,
    },
    Log {
        format: String,
        args: Vec<Expression>,
    },
    // Special variant for results from Empty.init, since context accounts get special treatment
    Initialized {
        name: String,
    },
    SolTransfer {
        from: Box<Expression>,
        to: Box<Expression>,
        amount: Box<Expression>,
        pda: bool,
    },
    CpiCall {
        cpi: Box<Cpi>,
        signer: Option<Vec<Expression>>,
    },
    GetBump {
        name: String,
    },
    FString {
        format: String,
        args: Vec<Expression>,
    },
    Clone(Box<Expression>), // Indicates that a value is crossing a function border
    List(Vec<Expression>),
    Tuple(Vec<Expression>),
    Int(i64),
    Float(f64),
    String(String),
    Bool(bool),
    Id(String),
    RawCall {
        func: Box<Expression>,
        args: CallArgs,
        location: Location,
    },
    RawFString {
        values: Vec<Expression>,
    },
}

#[derive(Clone, Debug, PartialEq)]
pub struct CallArgs {
    pub pos: Vec<Expression>,
    pub kw: Vec<(String, Expression)>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Cpi {
    TokenTransfer {
        from: Expression,
        authority: Expression,
        to: Expression,
        amount: Expression,
    },
    Mint {
        mint: Expression,
        authority: Expression,
        to: Expression,
        amount: Expression,
    },
    Burn {
        mint: Expression,
        authority: Expression,
        // Should be called from, but that's a keyword in Python
        holder: Expression,
        amount: Expression,
    },
}

/// A (binary) operator in an expression.
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Operator {
    // Math
    Add,
    Sub,
    Mul,
    Div,
    FloorDiv,
    Mod,
    Pow,
    // Bit math
    LShift,
    RShift,
    BitOr,
    BitXor,
    BitAnd,
    // Boolean
    And,
    Or,
    // Comparison
    Lt,
    Gt,
    Lte,
    Gte,
    Eq,
    NotEq,
}

/// A unary operator in an expression.
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum UnaryOperator {
    Pos,
    Neg,
    Not,
    Inv,
}

/// A single component of a list comprehension.
#[derive(Clone, Debug, PartialEq)]
pub enum ComprehensionPart {
    For { target: Pattern, iter: Expression },
    If { cond: Expression },
}

/// An assignable pattern.
#[derive(Clone, Debug, PartialEq)]
pub enum Pattern {
    Single(String),
}

impl Ty {
    pub fn as_function(self) -> Option<(Vec<Param>, Self)> {
        match self {
            Self::Function { params, returns } => Some((params, *returns)),
            _ => None,
        }
    }

    pub fn as_iterated(self) -> Option<Self> {
        match self {
            Self::List(element) => Some(*element),
            Self::Array(element, ..) => Some(*element),
            Self::Iter(element) => Some(*element),
            _ => None,
        }
    }

    /// Convenience function to create a Function type out of just the return value (0 args).
    pub fn function0(returns: Self) -> Self {
        Self::Function {
            params: vec![],
            returns: Box::new(returns),
        }
    }

    /// Get the type of an attribute of this type.
    pub fn get_attr(&self, attr: &str) -> Option<Self> {
        Some(match (self, attr) {
            (Self::TokenAccount | Self::AssociatedTokenAccount, "amount") => Self::U64,
            (Self::TokenAccount | Self::AssociatedTokenAccount, "owner") => Self::Pubkey,
            (
                Self::Empty(..)
                | Self::Signer
                | Self::TokenAccount
                | Self::TokenMint
                | Self::AssociatedTokenAccount,
                "key",
            ) => Self::function0(Self::Pubkey),
            (Self::U8 | Self::U64 | Self::I64 | Self::F64, "abs") => Self::Function {
                params: vec![],
                returns: Box::new(self.clone()),
            },
            (Self::F64, "floor" | "round" | "ceil") => Self::function0(Self::F64),
            (Self::U8 | Self::U64 | Self::I64 | Self::F64, "min" | "max") => Self::Function {
                params: vec![Param::new("0", self.clone())],
                returns: Box::new(self.clone()),
            },
            (Self::U8 | Self::U64 | Self::I64 | Self::F64, "pow") => Self::Function {
                params: vec![Param::new("0", Ty::_U32)],
                returns: Box::new(self.clone()),
            },
            (Self::F64, "powf") => Self::Function {
                params: vec![Param::new("0", Ty::F64)],
                returns: Box::new(self.clone()),
            },
            (Self::List(..), "len") => Self::function0(Self::U64),
            (Self::Array(..), "sort") => Self::function0(Self::Unit),
            (Self::Array(ty, ..), "iter") => Self::function0(Self::Iter(ty.clone())),
            // (Self::Iter(ty), "sum") => Self::function0(*ty.clone()),
            _ => {
                return None;
            }
        })
    }

    /// Get the "strictness level" of a numeric type. Used to decide type coercion.
    fn numeric_strictness(&self) -> Option<u8> {
        match self {
            Self::F64 => Some(0),
            Self::I64 => Some(1),
            Self::U64 => Some(2),
            Self::U8 => Some(3),
            _ => None,
        }
    }

    /// Numeric type
    pub fn num() -> Self {
        Self::Union(vec![Self::U8, Self::U64, Self::I64, Self::F64])
    }

    /// Check whether this type fits as another. Basically only necessary because of Union types
    pub fn fits_as(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::List(self_), Self::List(other_)) => self_.fits_as(&*other_),
            (Self::Array(self_, self_len), Self::Array(other_, other_len))
                if self_len.fits_as(other_len) =>
            {
                self_.fits_as(&*other_)
            }
            (Self::AssociatedTokenAccount, Self::TokenAccount) => true,
            (self_, Self::Union(opts)) => opts.iter().any(|opt| self_.fits_as(opt)),
            (_, Self::Any) => true,
            _ => self == other,
        }
    }

    /// Return whether this type is (Rust) Display-able.
    pub fn is_display(&self) -> bool {
        match self {
            Self::U8
            | Self::U64
            | Self::I64
            | Self::F64
            | Self::Bool
            | Self::String
            | Self::Pubkey => true,
            _ => false,
        }
    }

    /// Return whether this type is (Python) mutable.
    pub fn is_mutable(&self) -> bool {
        match self {
            Self::ExactDefined { is_mut, .. } => *is_mut,
            Self::List(..)
            | Self::Array(..)
            | Self::Signer
            | Self::Empty(..)
            | Self::TokenAccount
            | Self::TokenMint
            | Self::AssociatedTokenAccount => true,
            _ => false,
        }
    }
}

impl TyParam {
    fn fits_as(&self, other: &TyParam) -> bool {
        match (self, other) {
            (Self::Exact(self_len), Self::Exact(other_len)) => self_len == other_len,
            (_, Self::Any) => true,
            _ => false,
        }
    }
}

impl fmt::Display for Ty {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Function { returns, .. } => write!(f, "Callable[..., {}]", returns),
            Self::Empty(account) => write!(f, "Empty[{}]", account),
            Self::Defined(name) => write!(f, "{}", name),
            // TODO more display
            _ => write!(f, "{:?}", self),
        }
    }
}

impl TraitName {
    /// Get the type of an attribute of this trait.
    fn get_attr(&self, attr: &str) -> Option<Ty> {
        match self {
            Self::Account => match attr {
                "key" => Some(Ty::function0(Ty::Pubkey)),
                _ => None,
            },
            Self::Enum | Self::Instruction => None,
        }
    }

    /// Get the type of a static attribute of this trait.
    fn get_static_attr(&self, _attr: &str) -> Option<Ty> {
        None
    }
}

impl Expression {
    pub fn as_list(self) -> Option<Vec<Expression>> {
        match self {
            Expression::List(value) => Some(value),
            _ => None,
        }
    }

    pub fn as_tuple2(self) -> Option<(Self, Self)> {
        match self {
            Expression::Tuple(mut value) if value.len() == 2 => {
                Some((value.remove(0), value.remove(0)))
            }
            _ => None,
        }
    }

    pub fn as_int(self) -> Option<i64> {
        match self {
            Expression::Int(value) => Some(value),
            _ => None,
        }
    }

    pub fn as_string(self) -> Option<String> {
        match self {
            Expression::String(value) => Some(value),
            _ => None,
        }
    }

    pub fn as_id(self) -> Option<String> {
        match self {
            Expression::Id(name) => Some(name),
            _ => None,
        }
    }

    pub fn with_call(self, name: &str, args: Vec<Expression>) -> Self {
        Expression::Call {
            func: Box::new(Expression::Attribute {
                value: Box::new(self),
                attr: name.to_string(),
            }),
            args,
        }
    }
}

impl CallArgs {
    /// Remove the next positional arg, or get the matching keyword arg
    pub fn remove_pos_or_kw(&mut self, name: &str) -> Option<Expression> {
        if self.pos.len() == 0 {
            let index = self.kw.iter().position(|(kw, _)| kw.as_str() == name);
            index.map(|index| self.kw.remove(index).1)
        } else {
            Some(self.pos.remove(0))
        }
    }
}

impl TyDef {
    /// Return whether this type def is an enum.
    pub fn is_enum(&self) -> bool {
        match self {
            TyDef::Enum { .. } => true,
            _ => false,
        }
    }

    /// Return whether this type def is an account.
    pub fn is_account(&self) -> bool {
        match self {
            TyDef::Struct { traits, .. } => traits.contains(&TraitName::Account),
            _ => false,
        }
    }

    /// Get the type of an attribute of this defined type.
    pub fn get_attr(&self, attr: &str) -> Option<Ty> {
        match self {
            Self::Struct { fields, traits, .. } => {
                for trait_name in traits.iter() {
                    if let Some(found) = trait_name.get_attr(attr) {
                        return Some(found);
                    }
                }

                for Field { name, ty, .. } in fields.iter() {
                    if name.as_str() == attr {
                        return Some(ty.clone());
                    }
                }

                None
            }
            Self::Enum { .. } => None,
        }
    }

    /// Get the type of a static attribute of this defined type.
    pub fn get_static_attr(&self, attr: &str) -> Option<Ty> {
        match self {
            Self::Struct { traits, .. } => {
                for trait_name in traits.iter() {
                    if let Some(found) = trait_name.get_static_attr(attr) {
                        return Some(found);
                    }
                }

                None
            }
            Self::Enum { name, options } => {
                for option in options.iter() {
                    if option.as_str() == attr {
                        return Some(Ty::ExactDefined {
                            name: name.clone(),
                            is_mut: false,
                            is_acc: false,
                        });
                    }
                }

                None
            }
        }
    }
}

impl Param {
    pub fn new(name: &str, ty: Ty) -> Self {
        Self {
            name: name.to_string(),
            ty,
            required: true,
        }
    }

    pub fn optional(self) -> Self {
        Self {
            required: false,
            ..self
        }
    }
}

impl Account {
    pub fn system_program() -> Self {
        Account {
            name: "system_program".to_string(),
            account_type: Ty::SystemProgram,
            init: None,
        }
    }

    pub fn token_program() -> Self {
        Account {
            name: "token_program".to_string(),
            account_type: Ty::TokenProgram,
            init: None,
        }
    }

    pub fn associated_token_program() -> Self {
        Account {
            name: "associated_token_program".to_string(),
            account_type: Ty::AssociatedTokenProgram,
            init: None,
        }
    }

    pub fn rent() -> Self {
        Account {
            name: "rent".to_string(),
            account_type: Ty::Rent,
            init: None,
        }
    }
}

impl Operator {
    /// Return whether this operator is a boolean-only operator.
    pub fn is_bool(&self) -> bool {
        match self {
            Operator::And | Operator::Or => true,
            _ => false,
        }
    }

    /// Return whether this operator is a comparison.
    pub fn is_comp(&self) -> bool {
        match self {
            Operator::Lt
            | Operator::Gt
            | Operator::Lte
            | Operator::Gte
            | Operator::Eq
            | Operator::NotEq => true,
            _ => false,
        }
    }

    /// Determine the types to coerce a binary operation to.
    ///
    /// Boolean operators obviously need two booleans.
    /// Division always coerces to f64, all other numeric operations (including comparisons) coerce
    /// to the less strict type - f64 < i64 < u64 < u8.
    pub fn coercion(&self, left: &Ty, right: &Ty) -> Option<(Ty, Ty)> {
        match self {
            Self::Add
            | Self::Sub
            | Self::Mul
            | Self::FloorDiv
            | Self::Mod
            | Self::Pow
            | Self::Lt
            | Self::Gt
            | Self::Lte
            | Self::Gte
            | Self::Eq
            | Self::NotEq => {
                // See if we can skip numeric coercion
                if (*self == Self::Eq || *self == Self::NotEq) && left == right {
                    return Some((left.clone(), right.clone()));
                }

                let left_strictness = left.numeric_strictness();
                let right_strictness = right.numeric_strictness();

                if left_strictness.is_none() || right_strictness.is_none() {
                    return None;
                }

                let loosest = if left_strictness.unwrap() < right_strictness.unwrap() {
                    left.clone()
                } else {
                    right.clone()
                };

                Some((loosest.clone(), loosest))
            }
            Self::Div => Some((Ty::F64, Ty::F64)),
            Self::And | Self::Or => {
                // TODO add coercion for "anything to boolean," since python allows it
                if left == &Ty::Bool && right == &Ty::Bool {
                    Some((Ty::Bool, Ty::Bool))
                } else {
                    None
                }
            }
            _ => None,
        }
    }
}

impl fmt::Display for Operator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let op = match self {
            Self::Add => "+",
            Self::Sub => "-",
            Self::Mul => "*",
            Self::Div => "/",
            Self::FloorDiv => "//",
            Self::Mod => "%",
            Self::Pow => "**",
            Self::LShift => "<<",
            Self::RShift => ">>",
            Self::BitOr => "|",
            Self::BitXor => "^",
            Self::BitAnd => "&",
            Self::And => "and",
            Self::Or => "or",
            Self::Lt => "<",
            Self::Gt => ">",
            Self::Lte => "<=",
            Self::Gte => ">=",
            Self::Eq => "==",
            Self::NotEq => "!=",
        };

        write!(f, "{}", op)
    }
}
