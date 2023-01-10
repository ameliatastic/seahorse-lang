//! An AST that closely mirrors the Python AST, but only including syntax elements that Seahorse
//! supports and offering some strictness for things that are supported + some name changes.
//!
//! The only "transformation" done is merging binary operators into one type - in the Python AST,
//! boolean ops (and/or) are separate from comparisons (==/</<=/...) which are separate from
//! mathematicaly operations (+/-/...).

use crate::core::Located;

/// A module - one translation unit, or one Python file.
#[derive(Clone, Debug)]
pub struct Module {
    pub statements: Vec<TopLevelStatement>,
}

/// A top-level statement in a module.
pub type TopLevelStatement = Located<TopLevelStatementObj>;
#[derive(Clone, Debug)]
pub enum TopLevelStatementObj {
    Import {
        symbols: Vec<ImportSymbol>,
    },
    ImportFrom {
        level: usize,
        path: Vec<String>,
        symbols: Vec<ImportSymbol>,
    },
    Constant {
        name: String,
        value: Expression,
    },
    ClassDef {
        name: String,
        body: Vec<ClassDefStatement>,
        bases: Vec<TyExpression>,
        decorator_list: Vec<Expression>,
    },
    FunctionDef(FunctionDef),
    Expression(Expression),
}

/// A symbol that can be imported from a module. Includes "*", which will never have an alias.
/// Why did I even bother making this type? It's the exact same thing in rustpython_parser.
#[derive(Clone, Debug)]
pub struct ImportSymbol {
    pub symbol: String,
    pub alias: Option<String>,
}

/// A statement in a class definition.
pub type ClassDefStatement = Located<ClassDefStatementObj>;
#[derive(Clone, Debug)]
pub enum ClassDefStatementObj {
    FieldDef {
        name: String,
        ty: Option<TyExpression>,
        value: Option<Expression>,
    },
    MethodDef(FunctionDef),
}

/// A function definition.
#[derive(Clone, Debug)]
pub struct FunctionDef {
    pub name: String,
    pub params: Params,
    pub body: Vec<Statement>,
    // TODO maybe tighten what qualifies as a decorator? user-defined decorators most likely will
    // not be a thing
    pub decorator_list: Vec<Expression>,
    pub returns: Option<TyExpression>,
}

/// Parameters for a function definition.
#[derive(Clone, Debug)]
pub struct Params {
    pub is_instance_method: bool,
    pub params: Vec<Param>,
}

/// A single parameter in a function. Requires each param to be type-annotated.
pub type Param = Located<ParamObj>;
#[derive(Clone, Debug)]
pub struct ParamObj {
    pub arg: String,
    pub annotation: TyExpression,
}

/// A computable value.
pub type Expression = Located<ExpressionObj>;
#[derive(Clone, Debug)]
pub enum ExpressionObj {
    BinOp {
        left: Box<Expression>,
        op: Operator,
        right: Box<Expression>,
    },
    Index {
        value: Box<Expression>,
        index: Box<Expression>,
    },
    UnOp {
        op: UnaryOperator,
        value: Box<Expression>,
    },
    Attribute {
        value: Box<Expression>,
        name: String,
    },
    Call {
        function: Box<Expression>,
        args: Args,
    },
    Ternary {
        test: Box<Expression>,
        body: Box<Expression>,
        orelse: Box<Expression>,
    },
    Int(i128),
    Float(f64),
    List(Vec<Expression>),
    Tuple(Vec<Expression>),
    Comprehension {
        element: Box<Expression>,
        parts: Vec<ComprehensionPart>,
    },
    Str(String),
    FStr {
        parts: Vec<FStrPart>,
    },
    Id(String),
    Bool(bool),
    None,
    // A meta expression used to help the type checker
    Iter {
        value: Box<Expression>,
    },
}

/// Arguments to a function call.
#[derive(Clone, Debug)]
pub struct Args {
    pub pos: Vec<Expression>,
    pub kw: Vec<(String, Expression)>,
}

/// A component of a list comprehension.
#[derive(Clone, Debug)]
pub enum ComprehensionPart {
    For {
        target: Expression,
        iter: Expression,
    },
    If {
        cond: Expression,
    },
}

/// A component of an f-string.
#[derive(Clone, Debug)]
pub enum FStrPart {
    Str(String),
    ExpressionObj(Expression),
}

/// An expression that resolves to a type.
pub type TyExpression = Located<TyExpressionObj>;
#[derive(Clone, Debug)]
pub enum TyExpressionObj {
    Generic {
        base: Vec<String>,
        params: Vec<TyExpression>,
    },
    Const(u64),
}

/// A line/block of code in a function.
pub type Statement = Located<StatementObj>;
#[derive(Clone, Debug)]
pub enum StatementObj {
    Break,
    Continue,
    Return {
        value: Option<Expression>,
    },
    Pass,
    Assert {
        test: Expression,
        // TODO what to allow here
        msg: Option<Expression>,
    },
    Assign {
        target: Expression,
        value: Expression,
    },
    OpAssign {
        target: Expression,
        op: Operator,
        value: Expression,
    },
    TyAssign {
        target: Expression,
        ty: TyExpression,
        value: Option<Expression>,
    },
    ExpressionObj {
        expression: Expression,
    },
    If {
        test: Expression,
        body: Vec<Statement>,
        orelse: Option<Vec<Statement>>,
    },
    While {
        test: Expression,
        body: Vec<Statement>,
    },
    For {
        target: Expression,
        iter: Expression,
        body: Vec<Statement>,
    },
}

/// An operator in a binary operation. Note that Python splits these into three categories
/// (normal/math operators, boolean operators, and comparisons).
#[derive(Clone, Debug)]
pub enum Operator {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Pow,
    LShift,
    RShift,
    BitOr,
    BitXor,
    BitAnd,
    FloorDiv,
    // BooleanOperator
    And,
    Or,
    // Comparison
    Eq,
    NotEq,
    Lt,
    Lte,
    Gt,
    Gte,
    In,
    NotIn,
}

/// An operator in a unary operation.
#[derive(Clone, Debug)]
pub enum UnaryOperator {
    Pos,
    Neg,
    Not,
    Inv,
}
