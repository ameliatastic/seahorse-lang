//! The Seahorse Prelude includes a bunch of builtin types that convert to Rust/Anchor.

use crate::core::{compile::builtin::*, generate::LoadedTyExpr};
pub use crate::core::{
    compile::{ast::*, build::*, check::*, namespace::*, sign::*},
    util::*,
};
use crate::match1;
use quote::quote;
use std::collections::BTreeMap;
pub use std::collections::HashMap;

#[derive(Clone, Debug, PartialEq)]
pub enum Prelude {
    // Types
    Array,
    Enum,
    Account,
    Event,
    Signer,
    Empty,
    Program,
    TokenMint,
    TokenAccount,
    UncheckedAccount,
    Clock,
    CpiAccount,
    Pubkey,
    RustInt(bool, usize),
    RustFloat,
    // Meta types
    Seed,
    InitAccount, // Type used to get account data for `Empty[T].init` - regular `Account` cast doesn't work because of CPIs
    // Functions
    Floor,
    Ceil,
    ArrayConstructor,
    IntBytes,
    Size,
    // Directives
    DeclareId,
    // Decorators
    Instruction,
}

/// Create the seahorse.prelude namespace.
pub fn namespace() -> Namespace {
    let data = [
        ("Array", Prelude::Array),
        ("Enum", Prelude::Enum),
        ("Event", Prelude::Event),
        ("Account", Prelude::Account),
        ("Signer", Prelude::Signer),
        ("Empty", Prelude::Empty),
        ("Program", Prelude::Program),
        ("TokenMint", Prelude::TokenMint),
        ("TokenAccount", Prelude::TokenAccount),
        ("UncheckedAccount", Prelude::UncheckedAccount),
        ("Clock", Prelude::Clock),
        ("CpiAccount", Prelude::CpiAccount),
        ("Pubkey", Prelude::Pubkey),
        ("u8", Prelude::RustInt(false, 8)),
        ("u16", Prelude::RustInt(false, 16)),
        ("u32", Prelude::RustInt(false, 32)),
        ("u64", Prelude::RustInt(false, 64)),
        ("u128", Prelude::RustInt(false, 128)),
        ("i8", Prelude::RustInt(true, 8)),
        ("i16", Prelude::RustInt(true, 16)),
        ("i32", Prelude::RustInt(true, 32)),
        ("i64", Prelude::RustInt(true, 64)),
        ("i128", Prelude::RustInt(true, 128)),
        ("f64", Prelude::RustFloat),
        ("floor", Prelude::Floor),
        ("ceil", Prelude::Ceil),
        ("int_bytes", Prelude::IntBytes),
        ("size", Prelude::Size),
        ("array", Prelude::ArrayConstructor),
        ("declare_id", Prelude::DeclareId),
        ("instruction", Prelude::Instruction),
    ];

    let mut namespace = BTreeMap::new();
    for (name, obj) in data.into_iter() {
        namespace.insert(
            name.to_string(),
            NamespacedObject::Automatic(Builtin::Prelude(obj)),
        );
    }

    return namespace;
}

impl BuiltinSource for Prelude {
    fn name(&self) -> String {
        match self {
            Self::Array => "Array",
            Self::Enum => "Enum",
            Self::Account => "Account",
            Self::Event => "Event",
            Self::Signer => "Signer",
            Self::Empty => "Empty",
            Self::Program => "Program",
            Self::TokenMint => "TokenMint",
            Self::TokenAccount => "TokenAccount",
            Self::UncheckedAccount => "UncheckedAccount",
            Self::Clock => "Clock",
            Self::CpiAccount => "CpiAccount",
            Self::Pubkey => "Pubkey",
            Self::RustInt(signed, bits) => {
                return format!("{}{}", if *signed { 'i' } else { 'u' }, bits);
            }
            Self::RustFloat => "f64",
            Self::Seed => "<Seed>",
            Self::InitAccount => "<InitAccount>",
            Self::Floor => "floor",
            Self::Ceil => "ceil",
            Self::IntBytes => "int_bytes",
            Self::Size => "size",
            Self::ArrayConstructor => "array",
            Self::DeclareId => "declare_id",
            Self::Instruction => "instruction",
        }
        .to_string()
    }

    fn ty(&self) -> Ty {
        match self {
            // u8/i32/etc. + f64
            Self::RustInt(..) | Self::RustFloat => Ty::Type(
                TyName::Builtin(Builtin::Prelude(self.clone())),
                Some(
                    Ty::new_function(
                        vec![
                            ("x", Ty::Any, ParamType::Required)
                        ],
                        Ty::Transformed(
                            Ty::prelude(self.clone(), vec![]).into(),
                            {
                                let ty = TyExpr::Generic {
                                    mutability: Mutability::Immutable,
                                    name: vec![format!("{}", Ty::prelude(self.clone(), vec![]))],
                                    params: vec![],
                                    is_loadable: false
                                };

                                Transformation::new(move |mut expr| {
                                    let x = match1!(expr.obj, ExpressionObj::Call { args, .. } => args.into_iter().next().unwrap());
                                    let ty = LoadedTyExpr(&ty);

                                    expr.obj = ExpressionObj::Rendered(quote! {
                                        <#ty as TryFrom<_>>::try_from(#x).unwrap()
                                    });

                                    Ok(Transformed::Expression(expr))
                                })
                            }
                        )
                    ).into()
                )
            ),
            Self::Array => Ty::Type(
                TyName::Builtin(Builtin::Prelude(self.clone())),
                Some(Ty::ArrayConstructor1.into())
            ),
            Self::CpiAccount => Ty::Type(
                TyName::Builtin(Builtin::Prelude(self.clone())),
                Some(Ty::new_function(
                    vec![
                        ("account", Ty::Cast(Ty::prelude(Self::Account, vec![]).into()), ParamType::Required),
                        ("mut", Ty::python(Python::Bool, vec![]), ParamType::Optional),
                        ("signer", Ty::python(Python::Bool, vec![]), ParamType::Optional),
                        ("seeds", Ty::python(Python::List, vec![Ty::Cast(Ty::prelude(Self::Seed, vec![]).into())]), ParamType::Optional)
                    ],
                    Ty::Transformed(
                        Ty::prelude(self.clone(), vec![]).into(),
                        Transformation::new(|mut expr| {
                            let mut args = match1!(expr.obj, ExpressionObj::Call { args, .. } => args.into_iter());
                            let account = args.next().unwrap();
                            let mut_ = args.next().unwrap().optional()
                                .unwrap_or(ExpressionObj::Literal(Literal::Bool(false)).into());
                            let signer = args.next().unwrap().optional();
                            let seeds = args.next().unwrap().optional();

                            if signer.is_some() && seeds.is_some() {
                                return Err(CoreError::make_raw(
                                    "only one of signer/seeds may be provided when creating a CPI account",
                                    "Hint: if you want your CpiAccount to be a PDA signer, provide a value for seeds - otherwise, you can set signer to True to use this account as a regular instruction signer. By default, signer is False."
                                ));
                            }

                            let signer = signer.unwrap_or(ExpressionObj::Literal(Literal::Bool(false)).into());

                            let seeds = seeds
                                .map(|TypedExpression { ty, obj }| {
                                    TypedExpression {
                                        ty,
                                        obj: ExpressionObj::Rendered(quote! {
                                            Some(#obj.borrow().iter().map(|seed| Vec::from(*seed)).collect())
                                        })
                                    }
                                })
                                .unwrap_or(ExpressionObj::Rendered(quote! { None }).into());

                            expr.obj = ExpressionObj::Rendered(quote! {
                                CpiAccount {
                                    account_info: #account.to_account_info(),
                                    is_writable: #mut_,
                                    is_signer: #signer,
                                    seeds: #seeds
                                }
                            });

                            Ok(Transformed::Expression(expr))
                        })
                    )
                ).into())
            ),
            // floor(f64) -> i128
            Self::Floor => Ty::new_function(
                vec![(
                    "x",
                    Ty::prelude(Self::RustFloat, vec![]),
                    ParamType::Required,
                )],
                Ty::Transformed(
                    Ty::prelude(Self::RustInt(true, 128), vec![]).into(),
                    Transformation::new(|mut expr| {
                        let args = match1!(expr.obj, ExpressionObj::Call { args, .. } => args);
                        let x = args.into_iter().next().unwrap();

                        expr.obj = ExpressionObj::As {
                            value: ExpressionObj::Rendered(quote! { #x.floor() }).into(),
                            ty: TyExpr::new_specific(vec!["i128"], Mutability::Immutable)
                        };

                        Ok(Transformed::Expression(expr))
                    }),
                ),
            ),
            // ceil(f64) -> i128
            Self::Ceil => Ty::new_function(
                vec![(
                    "x",
                    Ty::prelude(Self::RustFloat, vec![]),
                    ParamType::Required,
                )],
                Ty::Transformed(
                    Ty::prelude(Self::RustInt(true, 128), vec![]).into(),
                    Transformation::new(|mut expr| {
                        let args = match1!(expr.obj, ExpressionObj::Call { args, .. } => args);
                        let x = args.into_iter().next().unwrap();

                        expr.obj = ExpressionObj::As {
                            value: ExpressionObj::Rendered(quote! { #x.ceil() }).into(),
                            ty: TyExpr::new_specific(vec!["i128"], Mutability::Immutable)
                        };

                        Ok(Transformed::Expression(expr))
                    }),
                ),
            ),
            // int_bytes(x: {Numeric} T, be?: bool) -> List[u8]
            Self::IntBytes => Ty::new_function(
                vec![
                    ("x", Ty::Anonymous(0), ParamType::Required),
                    ("be", Ty::python(Python::Bool, vec![]), ParamType::Optional)
                ],
                Ty::Transformed(
                    Ty::python(Python::List, vec![Ty::prelude(Self::RustInt(false, 8), vec![])]).into(),
                    Transformation::new(|mut expr| {
                        let mut args = match1!(expr.obj, ExpressionObj::Call { args, .. } => args.into_iter());
                        let x = args.next().unwrap();
                        let be = args.next().unwrap().optional();

                        expr.obj = if let Some(be) = be {
                            ExpressionObj::Rendered(quote! {
                                Mutable::new((if #be {
                                    #x.to_le_bytes()
                                } else {
                                    #x.to_be_bytes()
                                }).into_iter().collect::<Vec<_>>())
                            })
                        } else {
                            ExpressionObj::Rendered(quote! {
                                Mutable::new(#x.to_le_bytes().into_iter().collect::<Vec<_>>())
                            })
                        };

                        Ok(Transformed::Expression(expr))
                    })
                )
            ),
            // size(str) -> u64
            Self::Size => Ty::new_function(
                vec![
                    ("ob", Ty::python(Python::Str, vec![]), ParamType::Required)
                ],
                Ty::Transformed(
                    Ty::prelude(Self::RustInt(false, 64), vec![]).into(),
                    Transformation::new(|mut expr| {
                        let args = match1!(expr.obj, ExpressionObj::Call { args, .. } => args);
                        let ob = args.into_iter().next().unwrap();

                        expr.obj = ExpressionObj::As {
                            value: ExpressionObj::Rendered(quote! { #ob.len() }).into(),
                            ty: TyExpr::new_specific(vec!["u64"], Mutability::Immutable)
                        };

                        Ok(Transformed::Expression(expr))
                    }),
                )
            ),
            Self::ArrayConstructor => Ty::ArrayConstructor2,
            // declare_id
            Self::DeclareId => Ty::new_function(
                vec![
                    ("id", Ty::python(Python::Str, vec![]), ParamType::Required)
                ],
                Ty::Transformed(
                    Ty::python(Python::Tuple, vec![]).into(),
                    Transformation::new(|expr| {
                        let id = match1!(expr.obj, ExpressionObj::Call { args, .. } => args.into_iter().next().unwrap());

                        let id = match id.obj {
                            ExpressionObj::Literal(Literal::Str(id)) => id,
                            _ => return Err(CoreError::make_raw(
                                "the argument to declare_id must be a string literal",
                                ""
                            ))
                        };

                        Ok(Transformed::Directive(Directive::DeclareId(id)))
                    })
                )
            ),
            name => Ty::Type(TyName::Builtin(Builtin::Prelude(name.clone())), None),
        }
    }

    fn as_instance(&self, params: &Vec<Ty>) -> CResult<()> {
        match self {
            Self::Array if params.len() == 2 => Ok(()),
            Self::Empty if params.len() == 1 => Ok(()),
            _ if params.len() == 0 => Ok(()),
            _ => Err(CoreError::make_raw("invalid type", "")),
        }
    }

    fn attr(&self, attr: &String) -> Option<(Ty, Ty)> {
        match (self, attr.as_str()) {
            // Signer.key() -> Pubkey
            (Self::Signer, "key") => Some((
                Ty::prelude(Self::Signer, vec![]),
                Ty::new_function(vec![], Ty::prelude(Self::Pubkey, vec![])),
            )),
            // Signer.transfer_lamports(Cast(Account), u64) -> None
            (Self::Signer, "transfer_lamports") => Some((
                Ty::prelude(self.clone(), vec![]),
                Ty::new_function(
                    vec![
                        (
                            "to",
                            Ty::Cast(Ty::prelude(Prelude::Account, vec![]).into()),
                            ParamType::Required,
                        ),
                        (
                            "amount",
                            Ty::prelude(Prelude::RustInt(false, 64), vec![]),
                            ParamType::Required,
                        ),
                    ],
                    Ty::Transformed(
                        Ty::python(Python::Tuple, vec![]).into(),
                        Transformation::new(|mut expr| {
                            let (function, mut args) = match1!(expr.obj, ExpressionObj::Call { function, args, } => (function, args.into_iter()));
                            let account = match1!(function.obj, ExpressionObj::Attribute { value, .. } => *value);

                            let to = args.next().unwrap();
                            let amount = args.next().unwrap();

                            expr.obj = ExpressionObj::Rendered(quote! {
                                solana_program::program::invoke(
                                    &solana_program::system_instruction::transfer(
                                        &#account.key(),
                                        &#to.key(),
                                        #amount
                                    ),
                                    &[
                                        #account.to_account_info(),
                                        #to.to_account_info(),
                                        #account.programs.get("system_program").clone()
                                    ]
                                ).unwrap()
                            });

                            Ok(Transformed::Cpi {
                                expr,
                                program: AccountTyExpr::SystemProgram,
                            })
                        }),
                    ),
                ),
            )),
            // Empty[T].init(...) -> T
            (Self::Empty, "init") => Some((
                Ty::prelude(Self::Empty, vec![Ty::Anonymous(0)]),
                Ty::new_function(
                    vec![
                        (
                            "payer",
                            Ty::Cast(Ty::prelude(Self::InitAccount, vec![]).into()),
                            ParamType::Required,
                        ),
                        (
                            "seeds",
                            Ty::new_list(Ty::Cast(Ty::prelude(Self::Seed, vec![]).into())),
                            ParamType::Optional,
                        ),
                        (
                            "mint",
                            Ty::prelude(Self::TokenMint, vec![]),
                            ParamType::Optional,
                        ),
                        (
                            "authority",
                            Ty::Cast(Ty::prelude(Self::InitAccount, vec![]).into()),
                            ParamType::Optional,
                        ),
                        (
                            "decimals",
                            Ty::prelude(Self::RustInt(false, 8), vec![]),
                            ParamType::Optional,
                        ),
                        (
                            "associated",
                            Ty::python(Python::Bool, vec![]),
                            ParamType::Optional,
                        ),
                        (
                            "space",
                            Ty::prelude(Self::RustInt(false, 64), vec![]),
                            ParamType::Optional,
                        ),
                        (
                            "padding",
                            Ty::prelude(Self::RustInt(false, 64), vec![]),
                            ParamType::Optional,
                        ),
                    ],
                    Ty::Transformed(
                        Ty::Anonymous(0).into(),
                        Transformation::new_with_context(
                            |mut expr, _| {
                                let (function, args) = match1!(expr.obj, ExpressionObj::Call { function, args } => (function, args));
                                let empty = match1!(function.obj, ExpressionObj::Attribute { value, .. } => *value);
                                let name =
                                    match1!(&empty.obj, ExpressionObj::Id(var) => var.clone());
                                let mut args = args.into_iter();

                                let mut annotation = AccountAnnotation::new();
                                annotation.is_mut = false;
                                annotation.init = true;

                                let payer = args.next().unwrap();
                                let seeds = args.next().unwrap().optional();

                                let mint = args.next().unwrap().optional();
                                let authority = args.next().unwrap().optional();
                                let decimals = args.next().unwrap().optional();
                                let associated = match args.next().unwrap().obj {
                                    ExpressionObj::Literal(Literal::Bool(associated)) => {
                                        Some(associated)
                                    }
                                    ExpressionObj::Placeholder => None,
                                    _ => {
                                        return Err(CoreError::make_raw(
                                        "invalid argument to Empty.init()",
                                        "Hint: if you provide a value for \"associated\", it must be a boolean literal."
                                    ));
                                    }
                                };
                                let space = args.next().unwrap().optional();
                                let padding = args.next().unwrap().optional();

                                annotation.payer = Some(payer);
                                annotation.seeds = seeds.map(|seeds| {
                                    let seeds =
                                        match1!(seeds.obj, ExpressionObj::Vec(list) => list);
                                    seeds
                                });

                                match &expr.ty {
                                    Ty::Generic(name, _) => match name {
                                        TyName::Defined(_, DefinedType::Account) => {
                                            if mint.is_some()
                                                || authority.is_some()
                                                || decimals.is_some()
                                                || associated.is_some()
                                            {
                                                return Err(CoreError::make_raw(
                                                "invalid argument to Empty.init() for a program account",
                                                "Hint: you can only pass in a payer and optionally a list of seeds."
                                            ));
                                            }

                                            if space.is_some() && padding.is_some() {
                                                return Err(CoreError::make_raw(
                                                "invalid argument to Empty.init() for a program account",
                                                "Hint: you can only pass one of space and padding",
                                            ));
                                            }

                                            annotation.space = space;
                                            annotation.padding = padding;
                                        }

                                        TyName::Builtin(Builtin::Prelude(Self::TokenMint)) => {
                                            if mint.is_some()
                                                || authority.is_none()
                                                || decimals.is_none()
                                                || associated.is_some()
                                                || space.is_some()
                                            {
                                                return Err(CoreError::make_raw(
                                                "invalid argument to Empty[TokenMint].init()",
                                                "Hint: you can only pass in a payer, an authority, a number of decimals, and optionally a list of seeds."
                                            ));
                                            }

                                            annotation.mint_authority = authority;
                                            annotation.mint_decimals = decimals;
                                        }
                                        TyName::Builtin(Builtin::Prelude(Self::TokenAccount)) => {
                                            if mint.is_none()
                                                || authority.is_none()
                                                || decimals.is_some()
                                                || space.is_some()
                                            {
                                                return Err(CoreError::make_raw(
                                                "invalid argument to Empty[TokenAccount].init()",
                                                "Hint: you can only pass in a payer, an authority, a mint, and optionally a list of seeds."
                                            ));
                                            }

                                            if annotation.seeds.is_some()
                                                && associated == Some(true)
                                            {
                                                return Err(CoreError::make_raw(
                                                "invalid argument to Empty[TokenAccount].init()",
                                                "Hint: you may not initialize an associated token account with seeds."
                                            ));
                                            }

                                            annotation.token_authority = authority;
                                            annotation.token_mint = mint;

                                            if associated == Some(true) {
                                                annotation.is_associated = true;
                                            }
                                        }
                                        _ => {
                                            return Err(CoreError::make_raw(
                                            format!("could not initialize account type \"{}\"", expr.ty),
                                            "Help: you can only initialize program accounts (owned by your program), SPL token mints, and SPL token accounts."
                                        ));
                                        }
                                    },
                                    _ => panic!(),
                                }

                                expr.obj = ExpressionObj::Rendered(quote! {
                                    #empty.account.clone()
                                });

                                Ok(Transformed::AccountInit {
                                    expr,
                                    name,
                                    annotation,
                                })
                            },
                            Some(ExprContext::Seed),
                        ),
                    ),
                ),
            )),
            // Empty[T].bump() -> u8
            (Self::Empty, "bump") => Some((
                Ty::prelude(Self::Empty, vec![Ty::Anonymous(0)]),
                Ty::new_function(
                    vec![],
                    Ty::Transformed(
                        Ty::prelude(Self::RustInt(false, 8), vec![]).into(),
                        Transformation::new(|mut expr| {
                            let function = match1!(expr.obj, ExpressionObj::Call { function, .. } => *function);
                            let empty = match1!(function.obj, ExpressionObj::Attribute { value, .. } => *value);

                            expr.obj = ExpressionObj::Rendered(quote! {
                                #empty.bump.unwrap()
                            });

                            Ok(Transformed::Expression(expr))
                        }),
                    ),
                ),
            )),
            // Empty[T].key() -> Pubkey
            (Self::Empty, "key") => Some((
                Ty::prelude(Self::Empty, vec![Ty::Anonymous(0)]),
                Ty::new_function(
                    vec![],
                    Ty::Transformed(
                        Ty::prelude(Self::Pubkey, vec![]).into(),
                        Transformation::new(|mut expr| {
                            let function =
                                match1!(expr.obj, ExpressionObj::Call { function, .. } => function);
                            let empty = match1!(function.obj, ExpressionObj::Attribute { value, .. } => *value);

                            expr.obj = ExpressionObj::Rendered(quote! {
                                #empty.account.key()
                            });

                            Ok(Transformed::Expression(expr))
                        }),
                    ),
                ),
            )),
            // Program.invoke(accounts: List[CpiAccount], data: List[u8]) -> None
            (Self::Program, "invoke") => Some((
                Ty::prelude(self.clone(), vec![]),
                Ty::new_function(
                    vec![
                        (
                            "accounts",
                            Ty::python(Python::List, vec![Ty::prelude(Self::CpiAccount, vec![])]),
                            ParamType::Required,
                        ),
                        (
                            "data",
                            Ty::python(
                                Python::List,
                                vec![Ty::prelude(Self::RustInt(false, 8), vec![])],
                            ),
                            ParamType::Required,
                        ),
                    ],
                    Ty::Transformed(
                        Ty::python(Python::Tuple, vec![]).into(),
                        Transformation::new(|mut expr| {
                            let (function, mut args) = match1!(expr.obj, ExpressionObj::Call { function, args } => (*function, args.into_iter()));
                            let program = match1!(function.obj, ExpressionObj::Attribute { value, .. } => *value);

                            let accounts = args.next().unwrap();
                            let data = args.next().unwrap();

                            let setup = quote! {
                                let program = #program;
                                let cpi_accounts = #accounts;

                                let instruction = solana_program::instruction::Instruction {
                                    program_id: program.key(),
                                    data: #data.borrow().clone(),
                                    accounts: cpi_accounts
                                        .borrow()
                                        .iter()
                                        .map(|CpiAccount { account_info, is_signer, is_writable, .. }| AccountMeta {
                                            pubkey: account_info.key(),
                                            is_signer: *is_signer,
                                            is_writable: *is_writable
                                        })
                                        .collect()
                                };

                                let account_infos = [program.to_account_info()]
                                    .into_iter()
                                    .chain(cpi_accounts
                                        .borrow()
                                        .iter()
                                        .map(|CpiAccount { account_info, .. }| account_info.clone())
                                    )
                                    .collect::<Vec<_>>();

                                // The problem: all of the seeds come from different vecs of vecs
                                // of bytes, but we need a slice of slices of slices of bytes
                                // The solution: make a vec of vecs of vecs of bytes, turn that into
                                // a vec of vecs of slices of bytes, into a vec of slices of slices
                                // of bytes, and finally into a slice of slices of slices of bytes.
                                //
                                // Rust, I love you, but sometimes I feel like I shouldn't.
                                let seeds0 = cpi_accounts
                                    .borrow()
                                    .iter()
                                    .filter_map(|cpi_account| cpi_account.seeds.clone())
                                    .collect::<Vec<_>>();

                                let seeds1 = seeds0
                                    .iter()
                                    .map(|seeds1| seeds1
                                        .iter()
                                        .map(|seeds2| &seeds2[..])
                                        .collect::<Vec<_>>()
                                    ).collect::<Vec<_>>();

                                let seeds2 = seeds1
                                    .iter()
                                    .map(|seeds1| &seeds1[..])
                                    .collect::<Vec<_>>();
                            };

                            expr.obj = ExpressionObj::Rendered(quote! {
                                {
                                    #setup

                                    solana_program::program::invoke_signed(
                                        &instruction,
                                        account_infos.as_ref(),
                                        &seeds2[..]
                                    ).unwrap();
                                }
                            });

                            Ok(Transformed::Expression(expr))
                        }),
                    ),
                ),
            )),
            // Clock.slot() -> u64
            (Self::Clock, "slot") => Some((
                Ty::prelude(self.clone(), vec![]),
                Ty::new_function(
                    vec![],
                    Ty::Transformed(
                        Ty::prelude(Self::RustInt(false, 64), vec![]).into(),
                        Transformation::new(|mut expr| {
                            expr.obj = match1!(expr.obj, ExpressionObj::Call { function, .. } => function.obj);
                            Ok(Transformed::Expression(expr))
                        }),
                    ),
                ),
            )),
            // Clock.epoch_start_timestamp() -> i64
            (Self::Clock, "epoch_start_timestamp") => Some((
                Ty::prelude(self.clone(), vec![]),
                Ty::new_function(
                    vec![],
                    Ty::Transformed(
                        Ty::prelude(Self::RustInt(true, 64), vec![]).into(),
                        Transformation::new(|mut expr| {
                            expr.obj = match1!(expr.obj, ExpressionObj::Call { function, .. } => function.obj);
                            Ok(Transformed::Expression(expr))
                        }),
                    ),
                ),
            )),
            // Clock.epoch() -> u64
            (Self::Clock, "epoch") => Some((
                Ty::prelude(self.clone(), vec![]),
                Ty::new_function(
                    vec![],
                    Ty::Transformed(
                        Ty::prelude(Self::RustInt(false, 64), vec![]).into(),
                        Transformation::new(|mut expr| {
                            expr.obj = match1!(expr.obj, ExpressionObj::Call { function, .. } => function.obj);
                            Ok(Transformed::Expression(expr))
                        }),
                    ),
                ),
            )),
            // Clock.leader_schedule_epoch() -> u64
            (Self::Clock, "leader_schedule_epoch") => Some((
                Ty::prelude(self.clone(), vec![]),
                Ty::new_function(
                    vec![],
                    Ty::Transformed(
                        Ty::prelude(Self::RustInt(false, 64), vec![]).into(),
                        Transformation::new(|mut expr| {
                            expr.obj = match1!(expr.obj, ExpressionObj::Call { function, .. } => function.obj);
                            Ok(Transformed::Expression(expr))
                        }),
                    ),
                ),
            )),
            // Clock.unix_timestamp() -> u64
            (Self::Clock, "unix_timestamp") => Some((
                Ty::prelude(self.clone(), vec![]),
                Ty::new_function(
                    vec![],
                    Ty::Transformed(
                        Ty::prelude(Self::RustInt(true, 64), vec![]).into(),
                        Transformation::new(|mut expr| {
                            expr.obj = match1!(expr.obj, ExpressionObj::Call { function, .. } => function.obj);
                            Ok(Transformed::Expression(expr))
                        }),
                    ),
                ),
            )),
            // TokenMint.mint(authority = Cast(Account), to = TokenAccount, amount = u64, signer = List[Cast(Seed)]?) -> None
            (Self::TokenMint, "mint") => Some((
                Ty::prelude(Self::TokenMint, vec![]),
                Ty::new_function(
                    vec![
                        (
                            "authority",
                            Ty::Cast(Ty::prelude(Self::Account, vec![]).into()),
                            ParamType::Required,
                        ),
                        (
                            "to",
                            Ty::prelude(Self::TokenAccount, vec![]),
                            ParamType::Required,
                        ),
                        (
                            "amount",
                            Ty::prelude(Self::RustInt(false, 64), vec![]),
                            ParamType::Required,
                        ),
                        (
                            "signer",
                            Ty::new_list(Ty::Cast(Ty::prelude(Self::Seed, vec![]).into())),
                            ParamType::Optional,
                        ),
                    ],
                    Ty::Transformed(
                        Ty::python(Python::Tuple, vec![]).into(),
                        Transformation::new(|mut expr| {
                            let (function, args) = match1!(expr.obj, ExpressionObj::Call { function, args, } => (function, args));
                            let mint = match1!(function.obj, ExpressionObj::Attribute { value, .. } => *value);
                            let mut args = args.into_iter();
                            let authority = args.next().unwrap();
                            let to = args.next().unwrap();
                            let amount = args.next().unwrap();
                            let signer = args.next().unwrap();

                            let program_and_accounts = quote! {
                                #mint.programs.get("token_program"),
                                token::MintTo {
                                    mint: #mint.to_account_info(),
                                    authority: #authority.to_account_info(),
                                    to: #to.to_account_info()
                                }
                            };

                            let cpi_context = match signer.obj {
                                ExpressionObj::Placeholder => quote! {
                                    CpiContext::new(#program_and_accounts)
                                },
                                seeds => quote! {
                                    CpiContext::new_with_signer(
                                        #program_and_accounts,
                                        &[#seeds.borrow().as_slice()]
                                    )
                                },
                            };

                            expr.obj = ExpressionObj::Rendered(quote! {
                                token::mint_to(
                                    #cpi_context,
                                    #amount
                                ).unwrap();
                            });

                            Ok(Transformed::Cpi {
                                expr,
                                program: AccountTyExpr::TokenProgram,
                            })
                        }),
                    ),
                ),
            )),
            // TokenMint.burn(authority = Cast(Account), holder = TokenAccount, amount = u64, signer = List[Cast(Seed)]?) -> None
            (Self::TokenMint, "burn") => Some((
                Ty::prelude(Self::TokenMint, vec![]),
                Ty::new_function(
                    vec![
                        (
                            "authority",
                            Ty::Cast(Ty::prelude(Self::Account, vec![]).into()),
                            ParamType::Required,
                        ),
                        (
                            "holder",
                            Ty::prelude(Self::TokenAccount, vec![]),
                            ParamType::Required,
                        ),
                        (
                            "amount",
                            Ty::prelude(Self::RustInt(false, 64), vec![]),
                            ParamType::Required,
                        ),
                        (
                            "signer",
                            Ty::new_list(Ty::Cast(Ty::prelude(Self::Seed, vec![]).into())),
                            ParamType::Optional,
                        ),
                    ],
                    Ty::Transformed(
                        Ty::python(Python::Tuple, vec![]).into(),
                        Transformation::new(|mut expr| {
                            let (function, args) = match1!(expr.obj, ExpressionObj::Call { function, args, } => (function, args));
                            let mint = match1!(function.obj, ExpressionObj::Attribute { value, .. } => *value);
                            let mut args = args.into_iter();
                            let authority = args.next().unwrap();
                            let holder = args.next().unwrap();
                            let amount = args.next().unwrap();
                            let signer = args.next().unwrap();

                            let program_and_accounts = quote! {
                                #mint.programs.get("token_program"),
                                token::Burn {
                                    mint: #mint.to_account_info(),
                                    authority: #authority.to_account_info(),
                                    from: #holder.to_account_info()
                                }
                            };

                            let cpi_context = match signer.obj {
                                ExpressionObj::Placeholder => quote! {
                                    CpiContext::new(#program_and_accounts)
                                },
                                seeds => quote! {
                                    CpiContext::new_with_signer(
                                        #program_and_accounts,
                                        &[#seeds.borrow().as_slice()]
                                    )
                                },
                            };

                            expr.obj = ExpressionObj::Rendered(quote! {
                                token::burn(
                                    #cpi_context,
                                    #amount
                                ).unwrap();
                            });

                            Ok(Transformed::Cpi {
                                expr,
                                program: AccountTyExpr::TokenProgram,
                            })
                        }),
                    ),
                ),
            )),
            // TokenMint.key() -> Pubkey
            (Self::TokenMint, "key") => Some((
                Ty::prelude(Self::TokenMint, vec![]),
                Ty::new_function(vec![], Ty::prelude(Self::Pubkey, vec![])),
            )),
            // TokenMint.authority() -> Pubkey
            (Self::TokenMint, "authority") => Some((
                Ty::prelude(Self::TokenMint, vec![]),
                Ty::new_function(
                    vec![],
                    Ty::Transformed(
                        Ty::prelude(Self::Pubkey, vec![]).into(),
                        Transformation::new(|mut expr| {
                            let function =
                                match1!(expr.obj, ExpressionObj::Call { function, .. } => function);
                            let mint = match1!(function.obj, ExpressionObj::Attribute { value, .. } => *value);

                            expr.obj = ExpressionObj::Rendered(quote! {
                                #mint.mint_authority.unwrap()
                            });

                            Ok(Transformed::Expression(expr))
                        }),
                    ),
                ),
            )),
            // TokenMint.freeze_authority() -> Pubkey
            (Self::TokenMint, "freeze_authority") => Some((
                Ty::prelude(Self::TokenMint, vec![]),
                Ty::new_function(
                    vec![],
                    Ty::Transformed(
                        Ty::prelude(Self::Pubkey, vec![]).into(),
                        Transformation::new(|mut expr| {
                            let function =
                                match1!(expr.obj, ExpressionObj::Call { function, .. } => function);
                            let mint = match1!(function.obj, ExpressionObj::Attribute { value, .. } => *value);

                            expr.obj = ExpressionObj::Rendered(quote! {
                                #mint.freeze_authority.unwrap()
                            });

                            Ok(Transformed::Expression(expr))
                        }),
                    ),
                ),
            )),
            // TokenMint.decimals() -> u8
            (Self::TokenMint, "decimals") => Some((
                Ty::prelude(Self::TokenMint, vec![]),
                Ty::new_function(
                    vec![],
                    Ty::Transformed(
                        Ty::prelude(Self::RustInt(false, 8), vec![]).into(),
                        Transformation::new(|mut expr| {
                            let function =
                                match1!(expr.obj, ExpressionObj::Call { function, .. } => function);
                            let mint = match1!(function.obj, ExpressionObj::Attribute { value, .. } => *value);

                            expr.obj = ExpressionObj::Rendered(quote! {
                                #mint.decimals
                            });

                            Ok(Transformed::Expression(expr))
                        }),
                    ),
                ),
            )),
            // TokenMint.supply() -> u64
            (Self::TokenMint, "supply") => Some((
                Ty::prelude(Self::TokenMint, vec![]),
                Ty::new_function(
                    vec![],
                    Ty::Transformed(
                        Ty::prelude(Self::RustInt(false, 64), vec![]).into(),
                        Transformation::new(|mut expr| {
                            let function =
                                match1!(expr.obj, ExpressionObj::Call { function, .. } => function);
                            let mint = match1!(function.obj, ExpressionObj::Attribute { value, .. } => *value);

                            expr.obj = ExpressionObj::Rendered(quote! {
                                #mint.supply
                            });

                            Ok(Transformed::Expression(expr))
                        }),
                    ),
                ),
            )),
            // TokenAccount.transfer(authority = Cast(Account), to = TokenAccount, amount = u64, signer = List[Cast(Seed)]?) -> None
            (Self::TokenAccount, "transfer") => Some((
                Ty::prelude(Self::TokenAccount, vec![]),
                Ty::new_function(
                    vec![
                        (
                            "authority",
                            Ty::Cast(Ty::prelude(Self::Account, vec![]).into()),
                            ParamType::Required,
                        ),
                        (
                            "to",
                            Ty::prelude(Self::TokenAccount, vec![]),
                            ParamType::Required,
                        ),
                        (
                            "amount",
                            Ty::prelude(Self::RustInt(false, 64), vec![]),
                            ParamType::Required,
                        ),
                        (
                            "signer",
                            Ty::new_list(Ty::Cast(Ty::prelude(Self::Seed, vec![]).into())),
                            ParamType::Optional,
                        ),
                    ],
                    Ty::Transformed(
                        Ty::python(Python::Tuple, vec![]).into(),
                        Transformation::new(|mut expr| {
                            let (function, args) = match1!(expr.obj, ExpressionObj::Call { function, args, } => (function, args));
                            let from = match1!(function.obj, ExpressionObj::Attribute { value, .. } => *value);
                            let mut args = args.into_iter();
                            let authority = args.next().unwrap();
                            let to = args.next().unwrap();
                            let amount = args.next().unwrap();
                            let signer = args.next().unwrap();

                            let program_and_accounts = quote! {
                                #from.programs.get("token_program"),
                                token::Transfer {
                                    from: #from.to_account_info(),
                                    authority: #authority.to_account_info(),
                                    to: #to.to_account_info()
                                }
                            };

                            let cpi_context = match signer.obj {
                                ExpressionObj::Placeholder => quote! {
                                    CpiContext::new(#program_and_accounts)
                                },
                                seeds => quote! {
                                    CpiContext::new_with_signer(
                                        #program_and_accounts,
                                        &[#seeds.borrow().as_slice()]
                                    )
                                },
                            };

                            expr.obj = ExpressionObj::Rendered(quote! {
                                token::transfer(
                                    #cpi_context,
                                    #amount
                                ).unwrap();
                            });

                            Ok(Transformed::Cpi {
                                expr,
                                program: AccountTyExpr::TokenProgram,
                            })
                        }),
                    ),
                ),
            )),
            // TokenAccount.key() -> Pubkey
            (Self::TokenAccount, "key") => Some((
                Ty::prelude(Self::TokenAccount, vec![]),
                Ty::new_function(vec![], Ty::prelude(Self::Pubkey, vec![])),
            )),
            // TokenAccount.authority() -> Pubkey
            (Self::TokenAccount, "authority") => Some((
                Ty::prelude(Self::TokenAccount, vec![]),
                Ty::new_function(
                    vec![],
                    Ty::Transformed(
                        Ty::prelude(Self::Pubkey, vec![]).into(),
                        Transformation::new(|mut expr| {
                            let function =
                                match1!(expr.obj, ExpressionObj::Call { function, .. } => function);
                            let account = match1!(function.obj, ExpressionObj::Attribute { value, .. } => *value);

                            expr.obj = ExpressionObj::Rendered(quote! {
                                #account.owner
                            });

                            Ok(Transformed::Expression(expr))
                        }),
                    ),
                ),
            )),
            // TokenAccount.amount() -> u64
            (Self::TokenAccount, "amount") => Some((
                Ty::prelude(Self::TokenAccount, vec![]),
                Ty::new_function(
                    vec![],
                    Ty::Transformed(
                        Ty::prelude(Self::RustInt(false, 64), vec![]).into(),
                        Transformation::new(|mut expr| {
                            let function =
                                match1!(expr.obj, ExpressionObj::Call { function, .. } => function);
                            let account = match1!(function.obj, ExpressionObj::Attribute { value, .. } => *value);

                            expr.obj = ExpressionObj::Rendered(quote! {
                                #account.amount
                            });

                            Ok(Transformed::Expression(expr))
                        }),
                    ),
                ),
            )),
            // TokenAccount.mint() -> Pubkey
            (Self::TokenAccount, "mint") => Some((
                Ty::prelude(Self::TokenAccount, vec![]),
                Ty::new_function(
                    vec![],
                    Ty::Transformed(
                        Ty::prelude(Self::Pubkey, vec![]).into(),
                        Transformation::new(|mut expr| {
                            let function =
                                match1!(expr.obj, ExpressionObj::Call { function, .. } => function);
                            let account = match1!(function.obj, ExpressionObj::Attribute { value, .. } => *value);

                            expr.obj = ExpressionObj::Rendered(quote! {
                                #account.mint
                            });

                            Ok(Transformed::Expression(expr))
                        }),
                    ),
                ),
            )),
            // UncheckedAccount.key() -> Pubkey
            (Self::UncheckedAccount, "key") => Some((
                Ty::prelude(Self::UncheckedAccount, vec![]),
                Ty::new_function(vec![], Ty::prelude(Self::Pubkey, vec![])),
            )),
            _ => None,
        }
    }

    fn index(&self) -> Option<(Ty, Ty)> {
        match self {
            // Array[T].__index__(i128) -> T
            Self::Array => Some((
                Ty::prelude(Self::Array, vec![Ty::Anonymous(0), Ty::Anonymous(1)]),
                Ty::new_function(
                    vec![(
                        "",
                        Ty::Cast(Ty::prelude(Prelude::RustInt(true, 128), vec![]).into()),
                        ParamType::Required,
                    )],
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
                        }),
                    ),
                ),
            )),
            _ => None,
        }
    }

    fn static_attr(&self, attr: &String) -> Option<Ty> {
        match (self, attr.as_str()) {
            (Self::Pubkey, "find_program_address") => Some(Ty::new_function(
                vec![
                    (
                        "seeds",
                        Ty::python(
                            Python::List,
                            vec![Ty::Cast(Ty::prelude(Self::Seed, vec![]).into())],
                        ),
                        ParamType::Required,
                    ),
                    (
                        "program_id",
                        Ty::prelude(Self::Pubkey, vec![]),
                        ParamType::Optional,
                    ),
                ],
                Ty::Transformed(
                    Ty::python(
                        Python::Tuple,
                        vec![
                            Ty::prelude(Self::Pubkey, vec![]),
                            Ty::prelude(Self::RustInt(false, 8), vec![]),
                        ],
                    )
                    .into(),
                    Transformation::new(|mut expr| {
                        let mut args =
                            match1!(expr.obj, ExpressionObj::Call { args, .. } => args.into_iter());
                        let seeds = args.next().unwrap();
                        let program_id = match args.next().unwrap().obj {
                            ExpressionObj::Placeholder => quote! { &id() },
                            obj => quote! { &#obj },
                        };

                        expr.obj = ExpressionObj::Rendered(quote! {
                            Pubkey::find_program_address(
                                #seeds.borrow().as_slice(),
                                #program_id
                            )
                        });

                        Ok(Transformed::Expression(expr))
                    }),
                ),
            )),
            _ => None,
        }
    }

    fn casted(&self, ty: &Ty) -> Option<(Ty, Ty)> {
        let builtin = if let Ty::Generic(TyName::Builtin(builtin), _) = ty {
            builtin
        } else {
            return None;
        };

        match self {
            Self::RustInt(s1, b1) => match builtin {
                Builtin::Prelude(Self::RustInt(s2, b2)) if (!s1 && b1 < b2) => Some((
                    Ty::prelude(self.clone(), vec![]),
                    Ty::Transformed(
                        ty.clone().into(),
                        Transformation::new(|mut expr| {
                            expr.obj = ExpressionObj::As {
                                ty: TyExpr::Generic {
                                    mutability: Mutability::Immutable,
                                    name: vec![format!("{}", expr.ty)],
                                    params: vec![],
                                    is_loadable: false
                                },
                                value: expr.obj.into(),
                            };
                            Ok(Transformed::Expression(expr))
                        }),
                    ),
                )),
                Builtin::Prelude(Self::RustFloat) => Some((
                    Ty::prelude(self.clone(), vec![]),
                    Ty::Transformed(
                        ty.clone().into(),
                        Transformation::new(|mut expr| {
                            expr.obj = ExpressionObj::As {
                                ty: TyExpr::Generic {
                                    mutability: Mutability::Immutable,
                                    name: vec![format!("{}", expr.ty)],
                                    params: vec![],
                                    is_loadable: false
                                },
                                value: expr.obj.into(),
                            };
                            Ok(Transformed::Expression(expr))
                        }),
                    ),
                )),
                Builtin::Prelude(Self::Seed) => Some((
                    Ty::prelude(self.clone(), vec![]),
                    Ty::Transformed(
                        ty.clone().into(),
                        Transformation::new(|mut expr| {
                            let obj = expr.obj.without_borrows();
                            expr.obj = ExpressionObj::Rendered(quote! {
                                #obj.to_le_bytes().as_ref()
                            });

                            Ok(Transformed::Expression(expr))
                        }),
                    ),
                )),
                _ => None,
            },
            Self::Array => match builtin {
                Builtin::Python(Python::AsLen) => Some((
                    Ty::prelude(self.clone(), vec![Ty::Anonymous(0), Ty::Anonymous(1)]),
                    Ty::Transformed(
                        Ty::python(Python::AsLen, vec![]).into(),
                        Transformation::new(|mut expr| {
                            let array = expr.obj;

                            expr.obj = ExpressionObj::Rendered(quote! {
                                (#array.borrow().len() as u64)
                            });

                            Ok(Transformed::Expression(expr))
                        }),
                    ),
                )),
                Builtin::Python(Python::Iter) => Some((
                    Ty::prelude(self.clone(), vec![Ty::Anonymous(0), Ty::Anonymous(1)]),
                    Ty::Transformed(
                        Ty::python(Python::Iter, vec![Ty::Anonymous(0)]).into(),
                        Transformation::new(|mut expr| {
                            let array = expr.obj;

                            expr.obj = ExpressionObj::Rendered(quote! {
                                (#array.borrow().iter().map(|element| element.clone()))
                            });

                            Ok(Transformed::Expression(expr))
                        }),
                    ),
                )),
                _ => None,
            },
            Self::Signer => match builtin {
                Builtin::Prelude(Self::Account | Self::InitAccount) => {
                    Some((Ty::prelude(self.clone(), vec![]), ty.clone()))
                }
                Builtin::Prelude(Self::Seed) => Some((
                    Ty::prelude(self.clone(), vec![]).into(),
                    Ty::Transformed(
                        ty.clone().into(),
                        Transformation::new(|mut expr| {
                            let obj = expr.obj.without_borrows();
                            expr.obj = ExpressionObj::Rendered(quote! {
                                #obj.key().as_ref()
                            });

                            Ok(Transformed::Expression(expr))
                        }),
                    ),
                )),
                _ => None,
            },
            Self::Pubkey => match builtin {
                Builtin::Prelude(Self::Seed) => Some((
                    Ty::prelude(self.clone(), vec![]).into(),
                    Ty::Transformed(
                        ty.clone().into(),
                        Transformation::new(|mut expr| {
                            let obj = expr.obj.without_borrows();
                            expr.obj = ExpressionObj::Rendered(quote! {
                                #obj.as_ref()
                            });

                            Ok(Transformed::Expression(expr))
                        }),
                    ),
                )),
                _ => None,
            },
            // TODO copied straight from Signer, should extract to a function or something
            Self::Program => match builtin {
                Builtin::Prelude(Self::Account | Self::InitAccount) => {
                    Some((Ty::prelude(self.clone(), vec![]), ty.clone()))
                }
                Builtin::Prelude(Self::Seed) => Some((
                    Ty::prelude(self.clone(), vec![]).into(),
                    Ty::Transformed(
                        ty.clone().into(),
                        Transformation::new(|mut expr| {
                            let obj = expr.obj.without_borrows();
                            expr.obj = ExpressionObj::Rendered(quote! {
                                #obj.key().as_ref()
                            });

                            Ok(Transformed::Expression(expr))
                        }),
                    ),
                )),
                _ => None,
            },
            Self::TokenMint => match builtin {
                Builtin::Prelude(Self::Account | Self::InitAccount) => {
                    Some((Ty::prelude(self.clone(), vec![]), ty.clone()))
                }
                Builtin::Prelude(Self::Seed) => Some((
                    Ty::prelude(self.clone(), vec![]).into(),
                    Ty::Transformed(
                        ty.clone().into(),
                        Transformation::new(|mut expr| {
                            let obj = expr.obj.without_borrows();
                            expr.obj = ExpressionObj::Rendered(quote! {
                                #obj.key().as_ref()
                            });

                            Ok(Transformed::Expression(expr))
                        }),
                    ),
                )),
                _ => None,
            },
            Self::TokenAccount => match builtin {
                Builtin::Prelude(Self::Account | Self::InitAccount) => {
                    Some((Ty::prelude(self.clone(), vec![]), ty.clone()))
                }
                Builtin::Prelude(Self::Seed) => Some((
                    Ty::prelude(self.clone(), vec![]).into(),
                    Ty::Transformed(
                        ty.clone().into(),
                        Transformation::new(|mut expr| {
                            // Remove the borrows from the account, then return immediately
                            let obj = expr.obj.without_borrows();
                            expr.obj = ExpressionObj::Rendered(quote! {
                                #obj.key().as_ref()
                            });

                            Ok(Transformed::Expression(expr))
                        }),
                    ),
                )),
                _ => None,
            },
            Self::UncheckedAccount => match builtin {
                Builtin::Prelude(Self::Account | Self::InitAccount) => {
                    Some((Ty::prelude(self.clone(), vec![]), ty.clone()))
                }
                _ => None,
            },
            _ => None,
        }
    }
}
