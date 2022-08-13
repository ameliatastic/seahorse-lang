use crate::core::{python_ast::Location, seahorse_ast::*, CoreError};
use heck::ToUpperCamelCase;
use std::{
    collections::{HashMap, HashSet},
    mem::replace,
};

pub enum Error {
    NameReused(String),
    UndefinedTyName(String),
    InitAfterStart,
    Uniterable,
    AccidentalShadow(Ty, Ty),
    ListTyUnsupported,
    OperatorTypeCoercion(Ty, Operator, Ty),
    SeedsUnsupportedType(Ty),
    InitializerRepeat,
    CannotInit(Ty),
    CallWithoutFunctionType(String),
    UnknownVariable(String),
    CallWithTypeMismatch(String, Ty, Ty),
    CallWithArgMissing(String),
    CallWithInvalidArgs(Vec<Param>),
}

impl From<Error> for CoreError {
    fn from(error: Error) -> Self {
        match error {
            Error::NameReused(name) => Self::make_raw(
                "name reused",
                format!("Help: an object with the name \"{}\" is already defined", name)
            ),
            Error::UndefinedTyName(name) => Self::make_raw(
                "type name unknown",
                format!("Help: a type named \"{}\" was not found", name)
            ),
            Error::InitAfterStart => Self::make_raw(
                ".init() after function start",
                "Help: all calls to Empty[...].init() must be at the start of the function. Try moving this statement a few lines up."
            ),
            Error::Uniterable => Self::make_raw(
                "iterating over non-iterable value",
                ""
            ),
            Error::AccidentalShadow(from, to) => Self::make_raw(
                "accidental shadowing",
                format!(
                    "Help: unlike Python, Rust is statically typed. Reassigning a variable to a new type in a deeper scope would cause the original variable name to be shadowed instead of reassigning the variable, which is probably not what you want.\n\nOriginal type: {}\nNew type: {}\n\nTry converting to the appropriate type here, or use a different variable.",
                    from,
                    to
                )
            ),
            Error::ListTyUnsupported => Self::make_raw(
                "lists are unsupported",
                "Help: for now, list types are unsupported, but you can try using arrays instead. If you tried to use a list comprehension, you can assign it to an array by explicitly coercing to an array type:\n\n    array: Array[u64, 4] = [u64(i) for i in range(4)]"
            ),
            Error::OperatorTypeCoercion(left, op, right) => Self::make_raw(
                "type coercion failure",
                format!(
                    "Hint: you probably have a type mismatch. Make sure each:\n  - and/or has booleans on the left and right\n  - Math operation has only numbers involved\n  - Equality comparison has two of the same type\n\nFound: {} {} {}",
                    left,
                    op,
                    right
                )
            ),
            Error::SeedsUnsupportedType(ty) => Self::make_raw(
                "unsupported type for seed",
                format!(
                    "Hint: initializer/signer seeds may be other accounts, strings, integers, and u8 arrays. Found: {}",
                    ty
                )
            ),
            Error::InitializerRepeat => Self::make_raw(
                "reused initializer",
                "Help: each initializer may only be used once, you may have accidentally used the wrong variable. If you're trying to initialize multiple accounts, provide multiple initializers."
            ),
            Error::CannotInit(ty) => Self::make_raw(
                "cannot initialize type",
                format!(
                    "Help: you can only initialize accounts that are created by your program, or a few SPL accounts supported by Seahorse. If you want a custom class to be an account type, make sure it derives from Account:\n\n    class {}(Account):\n        # ...",
                    ty
                )
            ),
            Error::CallWithoutFunctionType(func) => Self::make_raw(
                "function not found",
                format!("\"{}\" is not a function", func)
            ),
            Error::UnknownVariable(name) => Self::make_raw(
                format!("variable \"{}\" not found", name),
                ""
            ),
            Error::CallWithTypeMismatch(param, param_ty, ty) => Self::make_raw(
                "type mismatch in function call",
                format!(
                    "For arg {}, expected {} but got {}",
                    param,
                    param_ty,
                    ty
                )
            ),
            Error::CallWithArgMissing(param) => Self::make_raw(
                "missing arg in function call",
                format!("Needs arg: {}", param)
            ),
            Error::CallWithInvalidArgs(params) => {
                let mut required_params = String::new();
                let mut required_count = 0;
                let mut optional_params = String::new();
                let mut optional_count = 0;

                for Param { name, required, .. } in params.iter() {
                    if *required {
                        if required_params.len() > 0 {
                            required_params.push_str(", ");
                        }
                        required_params.push_str(name.as_str());
                        required_count += 1;
                    }
                    else {
                        if optional_params.len() > 0 {
                            optional_params.push_str(", ");
                        }
                        optional_params.push_str(name.as_str());
                        optional_count += 1;
                    }
                }

                Self::make_raw("invalid function args",
                    match (required_count, optional_count) {
                        (0, 0) => format!("Help: this function has no args"),
                        (0, oc) => format!("Help: this function has {} optional args ({})", oc, optional_params),
                        (rc, 0) => format!("Help: this function has {} required args ({})", rc, required_params),
                        (rc, oc) => format!("Help: this function has {} required args ({}) and {} optional args ({})", rc, required_params, oc, optional_params),
                    }
                )
            }
        }
    }
}

impl Error {
    // Helper function to keep conversion code cleaner.
    pub fn into_core(self) -> CoreError {
        self.into()
    }
}

/// Data for the second pass.
pub struct TransformPass {
    // Types defined in this compilation unit
    ty_defs: HashMap<String, TyDef>,
    // Name -> type mapping
    defined: HashMap<String, Ty>,
    // Map of variable name -> declared type
    scopes: Vec<HashMap<String, Ty>>,
    // Collected error codes + messages
    errors: Vec<(String, String)>,
    // Current instruction context
    context: CurrentContext,
}

impl TransformPass {
    pub fn new() -> Self {
        TransformPass {
            ty_defs: HashMap::new(),
            defined: HashMap::new(),
            scopes: vec![HashMap::new()],
            errors: Vec::new(),
            context: CurrentContext::new(),
        }
    }

    fn push_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn pop_scope(&mut self) {
        self.scopes.pop();
    }

    fn declare(&mut self, name: String, ty: Ty) {
        self.scopes.last_mut().unwrap().insert(name, ty);
    }

    fn is_account_type(&self, ty: &Ty) -> bool {
        match ty {
            Ty::Signer
            | Ty::TokenAccount
            | Ty::TokenMint
            | Ty::AssociatedTokenAccount
            | Ty::Empty(_)
            | Ty::SystemProgram
            | Ty::TokenProgram
            | Ty::AssociatedTokenProgram => true,
            Ty::ExactDefined { is_acc, .. } => *is_acc,
            _ => false,
        }
    }

    fn name_exists(&self, name: &String) -> bool {
        // Builtins
        match name.as_str() {
            "abs" | "pow" | "repr" | "sorted" | "str" | "sum" | "type" | "len" | "range"
            | "print" | "u8" | "u64" | "i64" | "f64" | "round" | "ceil" | "floor" | "min"
            | "max" => {
                return true;
            }
            _ => {}
        };

        // Defined types/functions
        if self.defined.contains_key(name) {
            return true;
        }

        // Declared variables
        if self.get_declared(name).is_some() {
            return true;
        }

        return false;
    }

    fn get_declared(&self, name: &String) -> Option<Ty> {
        for scope in self.scopes.iter().rev() {
            if scope.contains_key(name) {
                return Some(scope.get(name).unwrap().clone());
            }
        }

        return None;
    }

    fn infer_type(&mut self, expression: &Expression) -> Ty {
        let res = match expression {
            Expression::BinOp { left, op, right } => {
                if op.is_bool() || op.is_comp() {
                    Ty::Bool
                } else {
                    let left_type = self.infer_type(&*left);
                    let right_type = self.infer_type(&*right);
                    // TODO unsafe unwrap?
                    op.coercion(&left_type, &right_type).unwrap().0
                }
            }
            Expression::Attribute { value, attr } => {
                let attr = attr.as_str();

                match self.infer_type(&*value) {
                    Ty::DefinedName(name) => {
                        match self.ty_defs.get(&name).unwrap().get_static_attr(attr) {
                            Some(ty) => ty,
                            None => Ty::Any,
                        }
                    }
                    Ty::ExactDefined { name, .. } => match self.ty_defs.get(&name) {
                        Some(type_def) => match type_def.get_attr(attr) {
                            Some(ty) => ty,
                            None => Ty::Any,
                        },
                        None => Ty::Any,
                    },
                    ty => match ty.get_attr(attr) {
                        Some(ty) => ty,
                        None => Ty::Any,
                    },
                }
            }
            Expression::StaticAttribute { value, attr } => {
                // Attribute variant handles both instance and static attributes, but this might
                // show up after a transformation
                self.infer_type(&Expression::Attribute {
                    value: value.clone(),
                    attr: attr.clone(),
                })
            }
            Expression::Index { value, .. } => match self.infer_type(&*value) {
                // Ty::List(el_type) => *el_type.clone(),
                Ty::Array(el_type, ..) => *el_type.clone(),
                _ => Ty::Any,
            },
            Expression::UnOp { value, .. } => {
                // NOTE: good enough, no unary operator we use changes the type of its operand
                self.infer_type(&*value)
            }
            Expression::Call { func, .. } => {
                let func = self.infer_type(&*func);
                match func {
                    Ty::Function { returns, .. } => *returns,
                    _ => Ty::Any,
                }
            }
            Expression::Range { to, .. } => {
                // start (from) might be a raw int literal, stop (to) is always the right type
                Ty::Iter(Box::new(self.infer_type(to)))
            }
            Expression::Comprehension { element, parts } => {
                // Build a scope with the variables used in the comprehension, and use it to infer
                // the element type
                self.push_scope();

                for part in parts.iter() {
                    if let ComprehensionPart::For { target, iter } = part {
                        match target {
                            Pattern::Single(name) => {
                                let iterated = match self.infer_type(iter).as_iterated() {
                                    Some(iterated) => iterated,
                                    _ => Ty::Any,
                                };
                                self.declare(name.clone(), iterated);
                            }
                        }
                    }
                }
                let ty = self.infer_type(element);

                self.pop_scope();

                Ty::List(Box::new(ty))
            }
            Expression::As { as_type, .. } => as_type.clone(),
            Expression::Coerce { as_type, .. } => as_type.clone(),
            Expression::Block { returns, .. } => self.infer_type(returns),
            Expression::Log { .. } => Ty::Unit,
            Expression::Initialized { name } => {
                if let Ty::Empty(account) = self.infer_type(&Expression::Id(name.clone())) {
                    *account
                } else {
                    panic!("Encountered an unknown initializer?");
                }
            }
            Expression::SolTransfer { .. } => Ty::Unit,
            Expression::CpiCall { .. } => Ty::Unit,
            Expression::GetBump { .. } => Ty::U8,
            Expression::FString { .. } => Ty::String,
            Expression::List(value) => {
                if value.len() == 0 {
                    Ty::Array(Box::new(Ty::Any), TyParam::Any)
                } else {
                    Ty::Array(
                        Box::new(self.infer_type(value.get(0).unwrap())),
                        TyParam::Any,
                    )
                }
            }
            Expression::Clone(value) => self.infer_type(&**value),
            Expression::Tuple(..) => Ty::Never,
            Expression::Int(value) => {
                if *value >= 0 {
                    if *value < 256 {
                        Ty::U8
                    } else {
                        Ty::U64
                    }
                } else {
                    Ty::I64
                }
            }
            Expression::Float(..) => Ty::F64,
            Expression::String(..) => Ty::String,
            Expression::Bool(..) => Ty::Bool,
            Expression::Id(name) => {
                if self.ty_defs.contains_key(name) {
                    Ty::DefinedName(name.clone())
                } else if let Some(ty) = self.get_declared(name) {
                    ty.clone()
                } else {
                    Ty::Any
                }
            }
            Expression::RawCall { .. } | Expression::RawFString { .. } => Ty::Never,
        };

        return res;
    }

    pub fn transform(&mut self, program: Program) -> Result<Program, CoreError> {
        let Program {
            mut id,
            mut instructions,
            errors,
            defs,
        } = program;

        // 0. Information pass
        let defs = defs
            .into_iter()
            .filter_map(|def| match def {
                Def::RawImport => None,
                Def::RawDeclareId(id_) => {
                    id = id_;
                    None
                }
                def => Some(def),
            })
            .collect::<Vec<_>>();

        // 1. Collect type sigs
        for def in defs.iter() {
            match def {
                // TODO check for overlap with builtin type names as well
                Def::TyDef(TyDef::Struct { name, traits, .. }) => {
                    if self.defined.contains_key(name) {
                        return Err(Error::NameReused(name.clone()).into_core());
                    }
                    self.defined.insert(
                        name.clone(),
                        Ty::ExactDefined {
                            name: name.clone(),
                            is_mut: true,
                            is_acc: true,
                        },
                    );
                }
                Def::TyDef(TyDef::Enum { name, .. }) => {
                    if self.defined.contains_key(name) {
                        return Err(Error::NameReused(name.clone()).into_core());
                    }
                    self.defined.insert(
                        name.clone(),
                        Ty::ExactDefined {
                            name: name.clone(),
                            is_mut: false,
                            is_acc: false,
                        },
                    );
                }
                _ => {}
            }
        }

        // 2. Transform type defs
        let defs = defs
            .into_iter()
            .map(|def| match def {
                Def::TyDef(def) => {
                    let def = self.transform_ty_def(def)?;
                    match &def {
                        TyDef::Struct { name, .. } | TyDef::Enum { name, .. } => {
                            self.ty_defs.insert(name.clone(), def.clone());
                        }
                    }

                    Ok(Def::TyDef(def))
                }
                def => Ok(def),
            })
            .collect::<Result<Vec<_>, CoreError>>()?;

        // 3. Collect function sigs
        let defs = defs
            .into_iter()
            .map(|def| {
                Ok(match def {
                    Def::FunctionDef(def) => {
                        let def = self.transform_function_sig(def)?;

                        self.declare(
                            def.name.clone(),
                            Ty::Function {
                                params: def.params.clone(),
                                returns: Box::new(def.returns.clone()),
                            },
                        );

                        Def::FunctionDef(def)
                    }
                    Def::RawInstructionDef(def) => {
                        let (def, instruction) = self.transform_instruction_sig(def)?;
                        instructions.push(instruction);
                        Def::RawInstructionDef(def)
                    }
                    def => def,
                })
            })
            .collect::<Result<Vec<_>, CoreError>>()?;

        // 4. Transform function bodies
        let defs = defs
            .into_iter()
            .map(|def| {
                Ok(match def {
                    Def::FunctionDef(def) => {
                        let def = self.transform_function(def)?;
                        Def::FunctionDef(def)
                    }
                    Def::RawInstructionDef(def) => {
                        let mut context = CurrentContext::new();
                        context.in_instruction = true;

                        let index = instructions
                            .iter()
                            .position(|Instruction { handler_name, .. }| &def.name == handler_name)
                            .unwrap();
                        let instruction = instructions.get_mut(index).unwrap();
                        context.accounts =
                            replace(&mut instruction.accounts_context.accounts, Vec::new());

                        self.context = context;
                        let def = self.transform_function(def)?;

                        let context = replace(&mut self.context, CurrentContext::new());
                        instruction.accounts_context = context.as_accounts_context();

                        Def::FunctionDef(def)
                    }
                    def => def,
                })
            })
            .collect::<Result<Vec<_>, CoreError>>()?;

        return Ok(Program {
            id,
            instructions,
            errors: self.errors.clone(),
            defs: defs,
        });
    }

    /// Transform the parameters and return type of a function.
    fn transform_function_sig(&mut self, def: FunctionDef) -> Result<FunctionDef, CoreError> {
        let FunctionDef {
            name,
            params,
            returns,
            body,
        } = def;

        if self.defined.contains_key(&name) {
            return Err(Error::NameReused(name.clone()).into_core());
        }

        let params = params
            .into_iter()
            .map(|Param { name, ty, required }| {
                Ok(Param {
                    name: name,
                    ty: self.transform_ty(ty)?,
                    required,
                })
            })
            .collect::<Result<Vec<_>, CoreError>>()?;
        let returns = self.transform_ty(returns.clone())?;

        return Ok(FunctionDef {
            name,
            params,
            returns,
            body,
        });
    }

    /// Transform the parameters and return type of an instruction, and create the respective
    /// instruction object.
    fn transform_instruction_sig(
        &mut self,
        def: FunctionDef,
    ) -> Result<(FunctionDef, Instruction), CoreError> {
        let FunctionDef {
            name,
            params,
            returns,
            body,
        } = def;

        let handler_name = format!("{}_handler", name);
        let context_name = name.to_upper_camel_case();

        if self.defined.contains_key(&name) {
            return Err(Error::NameReused(name.clone()).into_core());
        }

        let params = params
            .into_iter()
            .map(|Param { name, ty, required }| {
                Ok(Param {
                    name: name,
                    ty: self.transform_ty(ty)?,
                    required,
                })
            })
            .collect::<Result<Vec<_>, CoreError>>()?;
        let returns = self.transform_ty(returns.clone())?;

        let mut accounts = vec![];
        let mut context_defs = vec![];
        let mut params = params
            .into_iter()
            .filter_map(|param| {
                if self.is_account_type(&param.ty) {
                    accounts.push(Account {
                        name: param.name.clone(),
                        account_type: param.ty.clone(),
                        init: None,
                    });

                    context_defs.push(Statement::DeclareFromContext {
                        name: param.name.clone(),
                        ty: param.ty.clone(),
                    });

                    None
                } else {
                    Some(param)
                }
            })
            .collect::<Vec<_>>();

        params.insert(0, Param::new("ctx", Ty::Context(context_name.clone())));
        context_defs.extend(body);
        let body = context_defs;

        let instruction = Instruction {
            name,
            context_name,
            accounts_context: AccountsContext {
                accounts,
                params: Vec::new(),
            },
            params: params.clone(),
            handler_name: handler_name.clone(),
        };

        let def = FunctionDef {
            name: handler_name,
            params,
            returns,
            body,
        };

        return Ok((def, instruction));
    }

    /// Transform a type, pretty much just here to remove RawDefined types
    fn transform_ty(&mut self, ty: Ty) -> Result<Ty, CoreError> {
        match ty {
            // TODO more matching yay
            Ty::Empty(ty) => Ok(Ty::Empty(Box::new(self.transform_ty(*ty)?))),
            Ty::Defined(name) => {
                if self.defined.contains_key(&name) {
                    Ok(self.defined.get(&name).unwrap().clone())
                } else {
                    Err(Error::UndefinedTyName(name.clone()).into_core())
                }
            }
            ty => Ok(ty),
        }
    }

    /// Transform a type def
    fn transform_ty_def(&mut self, def: TyDef) -> Result<TyDef, CoreError> {
        match def {
            TyDef::Struct {
                name,
                fields,
                traits,
            } => Ok(TyDef::Struct {
                name,
                fields: fields
                    .into_iter()
                    .map(|Field { name, ty }| match self.transform_ty(ty) {
                        Ok(ty) => Ok(Field { name, ty }),
                        Err(err) => Err(err),
                    })
                    .collect::<Result<_, CoreError>>()?,
                traits,
            }),
            enum_ => Ok(enum_),
        }
    }

    fn transform_function(&mut self, def: FunctionDef) -> Result<FunctionDef, CoreError> {
        let FunctionDef {
            name,
            params,
            returns,
            body,
        } = def;

        // Add the function params to the scope
        self.push_scope();

        for Param { name, ty, .. } in params.iter() {
            self.declare(name.clone(), ty.clone());
        }

        let def = FunctionDef {
            name,
            params,
            returns,
            body: self.transform_block(body, false)?,
        };

        self.pop_scope();

        return Ok(def);
    }

    fn transform_block(
        &mut self,
        block: Vec<Statement>,
        new_scope: bool,
    ) -> Result<Vec<Statement>, CoreError> {
        if new_scope {
            self.push_scope();
        }

        let mut block_ = Vec::new();
        for statement in block.into_iter() {
            let statement = self.transform_statement(statement)?;
            match &statement {
                Statement::Declare { name, init, .. } => {
                    let ty = self.infer_type(&init);
                    self.declare(name.clone(), ty);
                }
                Statement::DeclareFromContext { name, ty } => {
                    self.declare(name.clone(), ty.clone());
                }
                _ => {}
            }
            block_.push(statement);
        }

        if new_scope {
            self.pop_scope();
        }

        return Ok(block_);
    }

    fn transform_statement(&mut self, statement: Statement) -> Result<Statement, CoreError> {
        match statement {
            Statement::Return { value: Some(value) } => Ok(Statement::Return {
                value: Some(self.transform_expression(value)?),
            }),
            Statement::RawAssign {
                left,
                ty,
                right: Some(right),
                location,
            } => {
                // Reassignment rule: if a variable is reassigned to a new type, it becomes a
                // declaration if it occurs in the exact same scope and an error otherwise. This
                // prevents accidental shadowing while allowing as much type flexibility as
                // possible.

                let mut right = self
                    .transform_expression(right)
                    .map_err(|err| err.located(location))?;
                let right_type = self.infer_type(&right);

                if let Expression::Initialized { .. } = right {
                    if !self.context.allow_inits {
                        return Err(Error::InitAfterStart.into_core().located(location));
                    }
                }

                if let Expression::Id(name) = left {
                    let curr_type = self.get_declared(&name);

                    // Explicit type coercion: if a type is provided, coerce to that type
                    if ty.is_some() {
                        let ty = ty.unwrap();
                        right = Expression::Coerce {
                            value: Box::new(right),
                            as_type: ty.clone(),
                        };

                        Ok(Statement::Declare {
                            name,
                            ty: Some(ty),
                            init: right,
                        })
                    } else if right_type.fits_as(&Ty::List(Box::new(Ty::Any))) {
                        Err(Error::ListTyUnsupported.into_core().located(location))
                    } else if Some(right_type.clone()) != curr_type {
                        if self.scopes.last().unwrap().contains_key(&name) || curr_type.is_none() {
                            Ok(Statement::Declare {
                                name,
                                ty: None,
                                init: right,
                            })
                        } else {
                            Err(Error::AccidentalShadow(curr_type.unwrap(), right_type)
                                .into_core()
                                .located(location))
                        }
                    } else {
                        Ok(Statement::Assign {
                            left: Expression::Id(name),
                            right,
                        })
                    }
                } else {
                    Ok(Statement::Assign { left, right })
                }
            }
            Statement::RawExpression { value, location } => {
                let value = self
                    .transform_expression(value)
                    .map_err(|err| err.located(location))?;

                match &value {
                    // Remove ineffective expressions - these can result from calling special
                    // functions like Empty.init(), which resolve to a variable from context but
                    // are used to modify the program
                    Expression::Initialized { .. } => {
                        if !self.context.allow_inits {
                            Err(Error::InitAfterStart.into_core().located(location))
                        } else {
                            Ok(Statement::Noop)
                        }
                    }
                    _ => {
                        self.context.allow_inits = false;
                        Ok(Statement::Expression { value })
                    }
                }
            }
            Statement::RawAssert {
                cond,
                msg,
                location,
            } => {
                self.context.allow_inits = false;

                let cond = self
                    .transform_expression(cond)
                    .map_err(|err| err.located(location))?;

                let index = self.errors.iter().position(|(_, msg_)| &msg == msg_);
                if let Some(index) = index {
                    let throw = self.errors.get(index).unwrap().0.clone();

                    Ok(Statement::Require { cond, throw })
                } else {
                    let throw = format!("E{:03}", self.errors.len());
                    self.errors.push((throw.clone(), msg));

                    Ok(Statement::Require { cond, throw })
                }
            }
            // Composite statements
            Statement::If {
                cond,
                body,
                or_else,
            } => {
                self.context.allow_inits = false;

                Ok(Statement::If {
                    cond: self.transform_expression(cond)?,
                    body: self.transform_block(body, true)?,
                    or_else: match or_else {
                        None => None,
                        Some(block) => Some(self.transform_block(block, true)?),
                    },
                })
            }
            Statement::While { cond, body } => {
                self.context.allow_inits = false;

                Ok(Statement::While {
                    cond: self.transform_expression(cond)?,
                    body: self.transform_block(body, true)?,
                })
            }
            Statement::ForIn { target, iter, body } => {
                self.context.allow_inits = false;

                let iter = self.transform_expression(iter)?;
                let iterated_ty = self
                    .infer_type(&iter)
                    .as_iterated()
                    .ok_or(Error::Uniterable.into_core())?;

                self.push_scope();
                match &target {
                    Pattern::Single(name) => self.declare(name.clone(), iterated_ty),
                }
                let body = self.transform_block(body, true)?;
                self.pop_scope();

                Ok(Statement::ForIn { target, iter, body })
            }
            // Ignore DeclareFromContext statements - they're just there for the source code
            statement @ Statement::DeclareFromContext { .. } => Ok(statement),
            // Everything else
            statement => {
                self.context.allow_inits = false;

                Ok(statement)
            }
        }
    }

    fn transform_expression(&mut self, expression: Expression) -> Result<Expression, CoreError> {
        match expression {
            Expression::BinOp { left, op, right } => {
                let mut left = self.transform_expression(*left)?;
                let left_type = self.infer_type(&left);

                let mut right = self.transform_expression(*right)?;
                let right_type = self.infer_type(&right);

                let (left_type_as, right_type_as) = op.coercion(&left_type, &right_type).ok_or(
                    Error::OperatorTypeCoercion(left_type.clone(), op, right_type.clone())
                        .into_core(),
                )?;
                if left_type != left_type_as {
                    // TODO use Coerce instead?
                    left = Expression::As {
                        value: Box::new(left),
                        as_type: left_type_as,
                    };
                }
                if right_type != right_type_as {
                    right = Expression::As {
                        value: Box::new(right),
                        as_type: right_type_as,
                    };
                }

                Ok(Expression::BinOp {
                    left: Box::new(left),
                    op,
                    right: Box::new(right),
                })
            }
            Expression::Attribute { value, attr } => {
                let value = self.transform_expression(*value)?;

                Ok(match self.infer_type(&value) {
                    Ty::DefinedName(_) => Expression::StaticAttribute {
                        value: Box::new(value),
                        attr,
                    },
                    _ => Expression::Attribute {
                        value: Box::new(value),
                        attr,
                    },
                })
            }
            Expression::Index { value, index } => {
                let expression = Expression::Index {
                    value: Box::new(self.transform_expression(*value)?),
                    index: Box::new(self.transform_expression(*index)?),
                };

                let ty = self.infer_type(&expression);
                Ok(expression)
            }
            Expression::UnOp { op, value } => Ok(Expression::UnOp {
                op,
                value: Box::new(self.transform_expression(*value)?),
            }),
            Expression::Comprehension { element, parts } => {
                self.push_scope();
                let mut parts_ = Vec::new();
                for part in parts.into_iter() {
                    parts_.push(match part {
                        ComprehensionPart::For { target, iter } => {
                            let iter = self.transform_expression(iter)?;

                            match &target {
                                Pattern::Single(target_name) => {
                                    let ty = match self.infer_type(&iter).as_iterated() {
                                        Some(ty) => ty,
                                        None => Ty::Any,
                                    };

                                    self.declare(target_name.clone(), ty);
                                }
                            }

                            ComprehensionPart::For { target, iter }
                        }
                        ComprehensionPart::If { cond } => ComprehensionPart::If {
                            cond: self.transform_expression(cond)?,
                        },
                    });
                }

                let element = self.transform_expression(*element)?;

                self.pop_scope();

                Ok(Expression::Comprehension {
                    element: Box::new(element),
                    parts: parts_,
                })
            }
            Expression::List(value) => {
                let mut value_ = Vec::new();
                for element in value.into_iter() {
                    value_.push(self.transform_expression(element)?);
                }

                Ok(Expression::List(value_))
            }
            Expression::Id(name) => {
                if self.name_exists(&name) {
                    if self.context.in_instruction && self.context.allow_inits {
                        let ty = self.infer_type(&Expression::Id(name.clone()));
                        // Kind of a hack - since use_params can only ever contain instruction
                        // params, nothing will have an Any type. This check is needed because this
                        // code also runs on the first statement after all the inits have taken
                        // place.
                        if !self.is_account_type(&ty) && ty != Ty::Any {
                            self.context.use_params.insert(name.clone(), ty);
                        }
                    }
                    Ok(Expression::Id(name))
                } else {
                    Err(Error::UnknownVariable(name.clone()).into_core())
                }
            }
            Expression::RawCall {
                func,
                args,
                location,
            } => {
                let func = self.transform_expression(*func)?;
                match self.transform_raw_call(func, args, location) {
                    Ok(expression) => {
                        let ty = self.infer_type(&expression);
                        Ok(expression)
                    }
                    Err(err) => Err(err),
                }
            }
            Expression::RawFString { values } => {
                let mut format = String::new();
                let mut args = Vec::new();
                for value in values.into_iter() {
                    if let Expression::String(value) = value {
                        format.push_str(value.as_str());
                    } else {
                        let value = self.transform_expression(value)?;
                        let ty = self.infer_type(&value);
                        if ty.is_display() {
                            format.push_str("{}");
                        } else {
                            format.push_str("{:?}");
                        }
                        args.push(value);
                    }
                }

                Ok(Expression::FString { format, args })
            }
            expression => Ok(expression),
        }
    }

    /// Transform all the args in a CallArgs struct, and use the function params to turn it into a
    /// map from arg name to arg
    fn transform_call_args(
        &mut self,
        mut args: CallArgs,
        params: &Vec<Param>,
    ) -> Result<HashMap<String, Expression>, CoreError> {
        let mut arg_map = HashMap::new();

        for Param { name, ty, required } in params.iter() {
            if let Some(arg) = args.remove_pos_or_kw(name.as_str()) {
                let arg = self.transform_expression(arg)?;
                let arg_ty = self.infer_type(&arg);

                // TODO should handle numeric coercion here - fits_as can continue if you pass a u8
                // as a u64, for instance
                if arg_ty.fits_as(ty) {
                    arg_map.insert(name.clone(), arg);
                } else {
                    return Err(
                        Error::CallWithTypeMismatch(name.clone(), ty.clone(), arg_ty).into_core(),
                    );
                }
            } else if *required {
                return Err(Error::CallWithArgMissing(name.clone()).into_core());
            }
        }

        if args.pos.len() > 0 || args.kw.len() > 0 {
            return Err(Error::CallWithInvalidArgs(params.clone()).into_core());
        }

        return Ok(arg_map);
    }

    /// Separate transform for functions, since this might require completely changing the
    /// expression (e.g. .inits, numeric casts)
    fn transform_raw_call(
        &mut self,
        func: Expression,
        args: CallArgs,
        location: Location,
    ) -> Result<Expression, CoreError> {
        match func {
            Expression::Id(name) => match name.as_str() {
                "len" => {
                    let mut arg_map = self.transform_call_args(
                        args,
                        &vec![Param::new(
                            "0",
                            Ty::Union(vec![
                                // Ty::List(Box::new(Ty::Any)),
                                Ty::Array(Box::new(Ty::Any), TyParam::Any),
                            ]),
                        )],
                    )?;

                    let iter = arg_map.remove("0").unwrap();
                    let ty = self.infer_type(&iter);
                    match ty {
                        // Ty::List(..) => Ok(Expression::As {
                        //     value: Box::new(iter.with_call("len", vec![])),
                        //     as_type: Ty::U64,
                        // }),
                        Ty::Array(_, TyParam::Exact(len)) => Ok(Expression::Int(len as i64)),
                        _ => panic!("Unexpected type?"),
                    }
                }
                "range" => {
                    // range(stop) -> 0..stop | range(start, stop) -> start..stop
                    let mut arg_map = self.transform_call_args(
                        args,
                        &vec![
                            Param::new("start", Ty::num()),
                            Param::new("stop", Ty::num()).optional(),
                        ],
                    )?;

                    // range(stop)
                    if arg_map.len() == 1 {
                        let stop = arg_map.remove("start").unwrap();
                        Ok(Expression::Range {
                            from: Box::new(Expression::Int(0)),
                            to: Box::new(stop),
                        })
                    }
                    // range(start, stop)
                    else {
                        let start = arg_map.remove("start").unwrap();
                        let stop = arg_map.remove("stop").unwrap();

                        let start_ty = self.infer_type(&start);
                        let stop_ty = self.infer_type(&stop);
                        let (as_start_ty, as_stop_ty) =
                            Operator::Add.coercion(&start_ty, &stop_ty).unwrap();

                        let start = coerced(start, start_ty, as_start_ty);
                        let stop = coerced(stop, stop_ty, as_stop_ty);

                        Ok(Expression::Range {
                            from: Box::new(start),
                            to: Box::new(stop),
                        })
                    }
                }
                "print" => {
                    // print(...)
                    // Just hardcoding 10 args because I'm lazy
                    // If someone uses more than that, they have a serious problem
                    let params = vec![
                        Param::new("0", Ty::Any).optional(),
                        Param::new("1", Ty::Any).optional(),
                        Param::new("2", Ty::Any).optional(),
                        Param::new("3", Ty::Any).optional(),
                        Param::new("4", Ty::Any).optional(),
                        Param::new("5", Ty::Any).optional(),
                        Param::new("6", Ty::Any).optional(),
                        Param::new("7", Ty::Any).optional(),
                        Param::new("8", Ty::Any).optional(),
                        Param::new("9", Ty::Any).optional(),
                    ];
                    let arg_map = self.transform_call_args(args, &params)?;
                    let args = order_arg_map(arg_map, &params);

                    let mut format = String::new();
                    for i in 0..args.len() {
                        if format.len() > 0 {
                            format.push_str(" ");
                        }

                        let ty = self.infer_type(args.get(i).unwrap());
                        if ty.is_display() {
                            format.push_str("{}");
                        } else {
                            format.push_str("{:?}");
                        }
                    }

                    Ok(Expression::Log { format, args })
                }
                "abs" => {
                    let mut args =
                        self.transform_call_args(args, &vec![Param::new("x", Ty::num())])?;

                    let x = args.remove("x").unwrap();

                    Ok(x.with_call("abs", vec![]))
                }
                "pow" => {
                    let mut args = self.transform_call_args(
                        args,
                        &vec![Param::new("base", Ty::num()), Param::new("exp", Ty::num())],
                    )?;

                    let base = args.remove("base").unwrap();
                    let base_ty = self.infer_type(&base);
                    let exp = args.remove("exp").unwrap();
                    let exp_ty = self.infer_type(&exp);

                    match base_ty {
                        Ty::F64 => {
                            let exp = coerced(exp, exp_ty, Ty::F64);
                            Ok(base.with_call("powf", vec![exp]))
                        }
                        _ => {
                            // Can't call pow on ambiguous numeric types, so making a cast
                            // TODO could definitely just change the Rust code gen behavior to
                            // account for this + other similar situations
                            let base = Expression::As {
                                value: Box::new(base),
                                as_type: base_ty,
                            };
                            let exp = coerced(exp, exp_ty, Ty::_U32);
                            Ok(base.with_call("pow", vec![exp]))
                        }
                    }
                }
                "sorted" => {
                    let mut args = self.transform_call_args(
                        args,
                        &vec![Param::new(
                            "iterable",
                            Ty::Array(Box::new(Ty::Any), TyParam::Any),
                        )],
                    )?;

                    let array = args.remove("iterable").unwrap();

                    let name = "sorted".to_string();
                    Ok(Expression::Block {
                        body: vec![
                            Statement::Declare {
                                name: name.clone(),
                                ty: None,
                                init: Expression::Clone(Box::new(array)),
                            },
                            Statement::Expression {
                                value: Expression::Id(name.clone()).with_call("sort", vec![]),
                            },
                        ],
                        returns: Box::new(Expression::Id(name)),
                    })
                }
                "str" => {
                    let mut args =
                        self.transform_call_args(args, &vec![Param::new("object", Ty::Any)])?;

                    let object = args.remove("object").unwrap();

                    Ok(Expression::FString {
                        format: if self.infer_type(&object).is_display() {
                            "{}".to_string()
                        } else {
                            "{:?}".to_string()
                        },
                        args: vec![object],
                    })
                }
                "sum" => {
                    let mut args = self.transform_call_args(
                        args,
                        &vec![Param::new(
                            "iterable",
                            Ty::Array(Box::new(Ty::num()), TyParam::Any),
                        )],
                    )?;

                    let array = args.remove("iterable").unwrap();
                    let accum_name = "sum".to_string();
                    let temp_name = "x".to_string();
                    // Have to do all this because Rust's sum function refuses to play nice
                    Ok(Expression::Block {
                        body: vec![
                            Statement::Declare {
                                name: accum_name.clone(),
                                ty: None,
                                init: Expression::Int(0),
                            },
                            Statement::ForIn {
                                target: Pattern::Single(temp_name.clone()),
                                iter: array,
                                body: vec![Statement::OpAssign {
                                    left: Expression::Id(accum_name.clone()),
                                    op: Operator::Add,
                                    right: Expression::Id(temp_name.clone()),
                                }],
                            },
                        ],
                        returns: Box::new(Expression::Id(accum_name)),
                    })
                }
                // Constructors for numeric types
                "u8" | "u64" | "i64" | "f64" => {
                    // u8(...), u64(...), etc.
                    let mut args =
                        self.transform_call_args(args, &vec![Param::new("0", Ty::num())])?;

                    let value = args.remove("0").unwrap();
                    let ty = self.infer_type(&value);
                    let as_ty = match name.as_str() {
                        "u8" => Ty::U8,
                        "u64" => Ty::U64,
                        "i64" => Ty::I64,
                        "f64" => Ty::F64,
                        _ => panic!(),
                    };

                    Ok(coerced(value, ty, as_ty))
                }
                // f64 native methods (which come from Python's math lib)
                "round" | "ceil" | "floor" => {
                    // round(...), ceil(...), floor(...)
                    let mut args =
                        self.transform_call_args(args, &vec![Param::new("x", Ty::F64)])?;

                    let value = args.remove("x").unwrap();
                    let res = Expression::Call {
                        func: Box::new(Expression::Attribute {
                            value: Box::new(value),
                            attr: name,
                        }),
                        args: vec![],
                    };

                    Ok(res)
                }
                // Numeric math methods
                "min" | "max" => {
                    // min(...), max(...)
                    let params = vec![
                        // TODO same problem as print()
                        Param::new("0", Ty::num()),
                        Param::new("1", Ty::num()),
                        Param::new("2", Ty::num()).optional(),
                        Param::new("3", Ty::num()).optional(),
                        Param::new("4", Ty::num()).optional(),
                        Param::new("5", Ty::num()).optional(),
                        Param::new("6", Ty::num()).optional(),
                        Param::new("7", Ty::num()).optional(),
                        Param::new("8", Ty::num()).optional(),
                        Param::new("9", Ty::num()).optional(),
                    ];
                    let arg_map = self.transform_call_args(args, &params)?;
                    let mut args = order_arg_map(arg_map, &params);

                    let mut as_ty = Ty::U8;
                    for arg in args.iter() {
                        as_ty = Operator::Lt
                            .coercion(&as_ty, &self.infer_type(arg))
                            .unwrap()
                            .0;
                    }

                    let first = args.remove(0);
                    let ty = self.infer_type(&first);
                    let mut chain = coerced(first, ty, as_ty.clone());
                    for arg in args.into_iter() {
                        let ty = self.infer_type(&arg);
                        chain = Expression::Call {
                            func: Box::new(Expression::Attribute {
                                value: Box::new(chain),
                                attr: name.clone(),
                            }),
                            args: vec![coerced(arg, ty, as_ty.clone())],
                        };
                    }

                    Ok(chain)
                }
                _ => match self.get_declared(&name) {
                    Some(Ty::Function { params, returns }) => {
                        let arg_map = self.transform_call_args(args, &params)?;
                        let args = order_arg_map(arg_map, &params)
                            .into_iter()
                            .map(|arg| {
                                if self.infer_type(&arg).is_mutable() {
                                    Expression::Clone(Box::new(arg))
                                } else {
                                    arg
                                }
                            })
                            .collect::<Vec<_>>();

                        Ok(Expression::Call {
                            func: Box::new(Expression::Id(name)),
                            args,
                        })
                    }
                    _ => Err(Error::CallWithoutFunctionType(name.to_string())),
                },
            },
            Expression::Attribute { value, attr } => {
                match (self.infer_type(&*value), attr.as_str()) {
                    // (Ty::List(ty), "append") => {
                    //     let params = vec![Param::new("0", *ty.clone())];
                    //     let mut arg_map = self.transform_call_args(args, &params)?;
                    //     let element = arg_map.remove("0").unwrap();

                    //     Ok(Expression::Call {
                    //         func: Box::new(Expression::Attribute {
                    //             value,
                    //             attr: "push".to_string(),
                    //         }),
                    //         args: vec![element],
                    //     })
                    // }
                    (Ty::TokenAccount, "amount") => {
                        // token_account.amount()
                        self.transform_call_args(args, &vec![])?;

                        Ok(Expression::Attribute {
                            value,
                            attr: "amount".to_string(),
                        })
                    }
                    (Ty::TokenAccount, "authority") => {
                        // token_account.authority()
                        self.transform_call_args(args, &vec![])?;

                        Ok(Expression::Attribute {
                            value,
                            attr: "owner".to_string(),
                        })
                    }
                    (Ty::TokenAccount, "transfer") => {
                        // token_account.transfer(authority, to, amount)
                        let mut args = self.transform_call_args(
                            args,
                            &vec![
                                Param::new("authority", Ty::Any),
                                Param::new("to", Ty::TokenAccount),
                                Param::new("amount", Ty::U64),
                                Param::new("signer", Ty::Array(Box::new(Ty::Any), TyParam::Any))
                                    .optional(),
                            ],
                        )?;
                        let mut arg = move |name: &str| args.remove(name);

                        // TODO typecheck authority
                        let authority = arg("authority").unwrap();
                        let to = arg("to").unwrap();
                        let amount = arg("amount").unwrap();

                        let signer = arg("signer")
                            .map(|seeds| self.transform_seeds(seeds.as_list().unwrap()))
                            .transpose()?;

                        self.context.infer_token_program = true;

                        Ok(Expression::CpiCall {
                            cpi: Box::new(Cpi::TokenTransfer {
                                from: *value,
                                to,
                                authority,
                                amount,
                            }),
                            signer,
                        })
                    }
                    (Ty::TokenMint, "authority") => {
                        // token_mint.authority()
                        self.transform_call_args(args, &vec![])?;

                        Ok(Expression::Attribute {
                            value,
                            attr: "owner".to_string(),
                        })
                    }
                    // TODO this and burn are 100% copy-pasted from TokenAccount.transfer - maybe
                    // make this DRYer?
                    (Ty::TokenMint, "mint") => {
                        // token_mint.mint(authority, to, amount)
                        let mut args = self.transform_call_args(
                            args,
                            &vec![
                                Param::new("authority", Ty::Any),
                                Param::new("to", Ty::TokenAccount),
                                Param::new("amount", Ty::U64),
                                Param::new("signer", Ty::Array(Box::new(Ty::Any), TyParam::Any))
                                    .optional(),
                            ],
                        )?;
                        let mut arg = move |name: &str| args.remove(name);

                        let authority = arg("authority").unwrap();
                        let to = arg("to").unwrap();
                        let amount = arg("amount").unwrap();

                        let signer = arg("signer")
                            .map(|seeds| self.transform_seeds(seeds.as_list().unwrap()))
                            .transpose()?;

                        self.context.infer_token_program = true;

                        Ok(Expression::CpiCall {
                            cpi: Box::new(Cpi::Mint {
                                mint: *value,
                                to,
                                authority,
                                amount,
                            }),
                            signer,
                        })
                    }
                    (Ty::AssociatedTokenAccount, "amount") => {
                        // associated_token_account.amount()
                        self.transform_call_args(args, &vec![])?;

                        Ok(Expression::Attribute {
                            value,
                            attr: "amount".to_string(),
                        })
                    }
                    (Ty::AssociatedTokenAccount, "authority") => {
                        // associated_token_account.authority()
                        self.transform_call_args(args, &vec![])?;

                        Ok(Expression::Attribute {
                            value,
                            attr: "owner".to_string(),
                        })
                    }
                    (Ty::AssociatedTokenAccount, "transfer") => {
                        // associated_token_account.transfer(authority, to, amount)
                        let mut args = self.transform_call_args(
                            args,
                            &vec![
                                Param::new("authority", Ty::Any),
                                Param::new("to", Ty::TokenAccount),
                                Param::new("amount", Ty::U64),
                            ],
                        )?;
                        let mut arg = move |name: &str| args.remove(name);

                        // TODO typecheck authority
                        let authority = arg("authority").unwrap();
                        let to = arg("to").unwrap();
                        let amount = arg("amount").unwrap();
                        self.context.infer_token_program = true;

                        Ok(Expression::CpiCall {
                            cpi: Box::new(Cpi::TokenTransfer {
                                from: *value,
                                to,
                                authority,
                                amount,
                            }),
                            signer: None,
                        })
                    }
                    (Ty::TokenMint, "burn") => {
                        // token_mint.burn(authority, to, amount)
                        let mut args = self.transform_call_args(
                            args,
                            &vec![
                                Param::new("authority", Ty::Any),
                                Param::new("holder", Ty::TokenAccount),
                                Param::new("amount", Ty::U64),
                                Param::new("signer", Ty::Array(Box::new(Ty::Any), TyParam::Any))
                                    .optional(),
                            ],
                        )?;
                        let mut arg = move |name: &str| args.remove(name);

                        let authority = arg("authority").unwrap();
                        let holder = arg("holder").unwrap();
                        let amount = arg("amount").unwrap();

                        let signer = arg("signer")
                            .map(|seeds| self.transform_seeds(seeds.as_list().unwrap()))
                            .transpose()?;

                        self.context.infer_token_program = true;

                        Ok(Expression::CpiCall {
                            cpi: Box::new(Cpi::Burn {
                                mint: *value,
                                holder,
                                authority,
                                amount,
                            }),
                            signer,
                        })
                    }
                    (Ty::Empty(..), "bump") => {
                        // empty.bump()
                        self.transform_call_args(args, &vec![])?;

                        let name = value.as_id().unwrap();
                        Ok(Expression::GetBump { name })
                    }
                    (Ty::Empty(acc_ty), "init") => {
                        // empty.init(payer = ..., seeds = ..., ...)
                        let name = value.as_id().unwrap();
                        let index = self.context.find_index(&name).unwrap();
                        if self.context.accounts.get(index).unwrap().init.is_some() {
                            return Err(Error::InitializerRepeat.into_core().located(location));
                        }

                        let init = match *acc_ty {
                            Ty::TokenAccount => {
                                let mut args = self.transform_call_args(
                                    args,
                                    &vec![
                                        Param::new("payer", Ty::Signer),
                                        Param::new(
                                            "seeds",
                                            Ty::Array(Box::new(Ty::Any), TyParam::Any),
                                        ),
                                        Param::new("mint", Ty::TokenMint),
                                        Param::new("authority", Ty::Any),
                                    ],
                                )?;
                                let mut arg = move |name: &str| args.remove(name).unwrap();

                                let payer = arg("payer").as_id().unwrap();
                                let seeds =
                                    self.transform_seeds(arg("seeds").as_list().unwrap())?;
                                let mint = arg("mint").as_id().unwrap();
                                let authority = arg("authority").as_id().unwrap();

                                self.context.infer_token_program = true;
                                self.context.infer_rent = true;

                                AccountInit::TokenAccount {
                                    payer,
                                    seeds,
                                    mint,
                                    authority,
                                }
                            }
                            Ty::TokenMint => {
                                let mut args = self.transform_call_args(
                                    args,
                                    &vec![
                                        Param::new("payer", Ty::Signer),
                                        Param::new(
                                            "seeds",
                                            Ty::Array(Box::new(Ty::Any), TyParam::Any),
                                        ),
                                        Param::new("decimals", Ty::U8),
                                        Param::new("authority", Ty::Any),
                                    ],
                                )?;
                                let mut arg = move |name: &str| args.remove(name).unwrap();

                                let payer = arg("payer").as_id().unwrap();
                                let seeds =
                                    self.transform_seeds(arg("seeds").as_list().unwrap())?;
                                let decimals = arg("decimals").as_int().unwrap();
                                let authority = arg("authority").as_id().unwrap();

                                self.context.infer_token_program = true;
                                self.context.infer_rent = true;

                                AccountInit::TokenMint {
                                    payer,
                                    seeds,
                                    decimals: decimals as u8,
                                    authority,
                                }
                            }
                            Ty::AssociatedTokenAccount => {
                                let mut args = self.transform_call_args(
                                    args,
                                    &vec![
                                        Param::new("payer", Ty::Signer),
                                        Param::new("mint", Ty::TokenMint),
                                        Param::new("authority", Ty::Any),
                                    ],
                                )?;
                                let mut arg = move |name: &str| args.remove(name).unwrap();

                                let payer = arg("payer").as_id().unwrap();
                                let mint = arg("mint").as_id().unwrap();
                                let authority = arg("authority").as_id().unwrap();

                                self.context.infer_token_program = true;
                                self.context.infer_associated_token_program = true;
                                self.context.infer_rent = true;

                                AccountInit::AssociatedTokenAccount {
                                    payer,
                                    mint,
                                    authority,
                                }
                            }
                            Ty::ExactDefined {
                                name: acc, is_acc, ..
                            } if is_acc => {
                                let mut args = self.transform_call_args(
                                    args,
                                    &vec![
                                        Param::new("payer", Ty::Signer),
                                        Param::new(
                                            "seeds",
                                            Ty::Array(Box::new(Ty::Any), TyParam::Any),
                                        ),
                                    ],
                                )?;
                                let mut arg = move |name: &str| args.remove(name).unwrap();

                                let payer = arg("payer").as_id().unwrap();
                                let seeds =
                                    self.transform_seeds(arg("seeds").as_list().unwrap())?;

                                AccountInit::Program {
                                    account_type: Ty::ExactDefined {
                                        name: acc,
                                        is_mut: true,
                                        is_acc: true,
                                    },
                                    payer,
                                    seeds,
                                }
                            }
                            ty => {
                                return Err(Error::CannotInit(ty).into_core().located(location));
                            }
                        };

                        self.context.accounts.get_mut(index).unwrap().init = Some(init);
                        self.context.infer_system_program = true;

                        Ok(Expression::Initialized { name })
                    }
                    (ty, "transfer_lamports") if self.is_account_type(&ty) => {
                        let mut args = self.transform_call_args(
                            args,
                            &vec![Param::new("to", Ty::Any), Param::new("amount", Ty::U64)],
                        )?;
                        let mut arg = move |name: &str| args.remove(name);

                        let to = arg("to").unwrap();
                        let amount = arg("amount").unwrap();

                        self.context.infer_system_program = true;

                        Ok(Expression::SolTransfer {
                            from: value,
                            to: Box::new(to),
                            amount: Box::new(amount),
                            pda: match ty {
                                Ty::ExactDefined { is_acc, .. } => is_acc,
                                _ => false,
                            },
                        })
                    }
                    (Ty::ExactDefined { name, .. }, attr) if self.ty_defs.contains_key(&name) => {
                        if let Some((params, ..)) = self
                            .ty_defs
                            .get(&name)
                            .unwrap()
                            .get_attr(attr)
                            .and_then(|ty| ty.as_function())
                        {
                            let args = self.transform_call_args(args, &params)?;
                            let args = order_arg_map(args, &params);

                            Ok(Expression::Call {
                                func: Box::new(Expression::Attribute {
                                    value,
                                    attr: attr.to_string(),
                                }),
                                args,
                            })
                        } else {
                            Err(Error::CallWithoutFunctionType(format!("{}.{}", name, attr)))
                        }
                    }
                    (ty, attr) => {
                        if let Some((params, ..)) =
                            ty.get_attr(attr).and_then(|ty| ty.as_function())
                        {
                            let args = self.transform_call_args(args, &params)?;
                            let args = order_arg_map(args, &params)
                                .into_iter()
                                .map(|arg| {
                                    if self.infer_type(&arg).is_mutable() {
                                        Expression::Clone(Box::new(arg))
                                    } else {
                                        arg
                                    }
                                })
                                .collect::<Vec<_>>();

                            Ok(Expression::Call {
                                func: Box::new(Expression::Attribute {
                                    value,
                                    attr: attr.to_string(),
                                }),
                                args,
                            })
                        } else {
                            Err(Error::CallWithoutFunctionType(format!("{}.{}", ty, attr)))
                        }
                    }
                }
            }
            _ => Err(Error::CallWithoutFunctionType("<unknown>".to_string())),
        }
        .map_err(|err| err.into_core().located(location))
    }

    fn transform_seeds(
        &mut self,
        raw_seeds: Vec<Expression>,
    ) -> Result<Vec<Expression>, CoreError> {
        let mut seeds = Vec::new();
        for seed in raw_seeds.into_iter() {
            seeds.push(match self.infer_type(&seed) {
                Ty::String => seed
                    .with_call("as_bytes", vec![])
                    .with_call("as_ref", vec![]),
                Ty::U8 => seed
                    .with_call("to_le_bytes", vec![])
                    .with_call("as_ref", vec![]),
                Ty::U64 => seed
                    .with_call("to_le_bytes", vec![])
                    .with_call("as_ref", vec![]),
                Ty::I64 => seed
                    .with_call("to_le_bytes", vec![])
                    .with_call("as_ref", vec![]),
                Ty::Pubkey => seed.with_call("as_ref", vec![]),
                ty if self.is_account_type(&ty) => {
                    seed.with_call("key", vec![]).with_call("as_ref", vec![])
                }
                Ty::Array(el_type, ..) if *el_type == Ty::U8 => seed.with_call("as_ref", vec![]),
                ty => {
                    return Err(Error::SeedsUnsupportedType(ty).into_core());
                }
            });
        }

        return Ok(seeds);
    }
}

struct CurrentContext {
    in_instruction: bool,
    accounts: Vec<Account>,
    allow_inits: bool,
    infer_system_program: bool,
    infer_token_program: bool,
    infer_associated_token_program: bool,
    infer_rent: bool,
    use_params: HashMap<String, Ty>,
}

impl CurrentContext {
    fn new() -> Self {
        CurrentContext {
            in_instruction: false,
            accounts: Vec::new(),
            allow_inits: true,
            infer_system_program: false,
            infer_token_program: false,
            infer_associated_token_program: false,
            infer_rent: false,
            use_params: HashMap::new(),
        }
    }

    fn as_accounts_context(self) -> AccountsContext {
        let mut accounts = self.accounts;
        if self.infer_system_program {
            accounts.push(Account::system_program());
        }
        if self.infer_token_program {
            accounts.push(Account::token_program());
        }
        if self.infer_associated_token_program {
            accounts.push(Account::associated_token_program());
        }
        if self.infer_rent {
            accounts.push(Account::rent());
        }

        return AccountsContext {
            accounts,
            params: self.use_params.into_iter().collect(),
        };
    }

    fn find_index(&self, name: &String) -> Option<usize> {
        self.accounts
            .iter()
            .position(|account| account.name == *name)
    }
}

fn order_arg_map(mut arg_map: HashMap<String, Expression>, params: &Vec<Param>) -> Vec<Expression> {
    let mut args = Vec::new();
    for Param { name, .. } in params.iter() {
        if let Some(arg) = arg_map.remove(name) {
            args.push(arg);
        }
    }

    return args;
}

fn coerced(value: Expression, ty: Ty, as_ty: Ty) -> Expression {
    if ty == as_ty {
        value
    } else {
        Expression::As {
            value: Box::new(value),
            as_type: as_ty,
        }
    }
}
