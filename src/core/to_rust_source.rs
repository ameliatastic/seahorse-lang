use crate::core::{seahorse_ast::*, CoreError};

use heck::ToSnakeCase;
use proc_macro2::{Ident, Literal, TokenStream};
use quote::{format_ident, quote, ToTokens};
use regex::{Captures, Regex};
use rustfmt_wrapper::{config::*, rustfmt_config, Error as RustfmtError};

/// Convenience function for turning strings into Idents
fn ident<S: ToString>(name: &S) -> Ident {
    format_ident!("{}", name.to_string())
}

impl ToTokens for Def {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        tokens.extend(match self {
            Def::TyDef(def) if def.is_account() => quote! {
                #[derive(Debug)]
                #[account]
                #def
            },
            Def::TyDef(def) if def.is_enum() => quote! {
                #[derive(AnchorSerialize, AnchorDeserialize, Debug, Clone, PartialEq)]
                #def
            },
            Def::TyDef(def) => quote! {
                #[derive(AnchorSerialize, AnchorDeserialize, Default, Debug, Clone, PartialEq)]
                #def
            },
            Def::FunctionDef(def) => quote! {
                #def
            },
            Def::RawInstructionDef { .. } | Def::RawDeclareId(..) | Def::RawImport => {
                panic!("Encountered an unformattable def {:?}", self)
            }
        });
    }
}

impl ToTokens for Instruction {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let Instruction {
            name,
            params,
            handler_name,
            ..
        } = self;

        let name = ident(&self.name);
        let handler_name = ident(&self.handler_name);
        let args = params
            .iter()
            .map(|param| Expression::Id(param.name.clone()));

        tokens.extend(quote! {
            pub fn #name(#(#params),*) -> Result<()> {
                #handler_name(#(#args),*)
            }
        });
    }
}

impl ToTokens for Account {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let Account {
            name,
            account_type,
            init,
        } = self;
        let name = ident(name);

        tokens.extend(match (account_type, init) {
            (Ty::SystemProgram, None) => quote! {
                pub system_program: Program<'info, System>
            },
            (Ty::TokenProgram, None) => quote! {
                pub token_program: Program<'info, token::Token>
            },
            (Ty::AssociatedTokenProgram, None) => quote! {
                pub associated_token_program: Program<'info, associated_token::AssociatedToken>
            },
            (Ty::Rent, None) => quote! {
                pub rent: Sysvar<'info, Rent>
            },
            (ty, init) => {
                let account_type = InAccounts(&account_type);

                if let Some(init) = init {
                    quote! {
                        #init
                        pub #name: #account_type
                    }
                } else {
                    quote! {
                        #[account(mut)]
                        pub #name: #account_type
                    }
                }
            }
        });
    }
}

/// Newtype to correctly display accounts that are part of an accounts context.
struct InAccounts<'t>(&'t Ty);
impl<'t> ToTokens for InAccounts<'t> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        tokens.extend(match self.0 {
            Ty::Signer => quote! { Signer<'info> },
            Ty::TokenAccount => quote! { Box<Account<'info, token::TokenAccount>> },
            Ty::TokenMint => quote! { Box<Account<'info, token::Mint>> },
            Ty::AssociatedTokenAccount => quote! { Box<Account<'info, token::TokenAccount>> },
            Ty::Empty(ty) => {
                let ty = InAccounts(&ty);

                quote! { #ty }
            }
            Ty::ExactDefined { name, .. } => {
                let name = ident(&name);

                quote! {
                    Box<Account<'info, #name>>
                }
            }
            _ => panic!("Encountered an unformattable account type {:?}", self.0),
        });
    }
}

impl ToTokens for AccountInit {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        // NOTE explanation for the hack you're about to see:
        //
        // Rustfmt doesn't like formatting attributes, and the Anchor #[account(init, ...)] lines
        // are far too long for it. The solution is to use a dummy macro (here I'm using a dummy
        // field set to the "type" of the macro), let rustfmt format the macro contents, then
        // replace the dummy field with #[account(...)]. Using triple square brackets because the
        // replacement is done with regex, and we don't want to match the closing bracket of a
        // seeds = [...] field.
        //
        // Entirely possible that this fails due to a well-crafted string literal key in the seeds.
        // Just gonna look the other way on that for now.

        tokens.extend(match self {
            AccountInit::Program { account_type, payer, seeds } => {
                let payer = ident(payer);
                let account_type = match account_type {
                    Ty::ExactDefined { name, .. } => ident(&name),
                    _ => panic!("Encountered an unexpected non-program account")
                };

                quote! {
                    // #[account(init, payer = #payer, seeds = [#(#seeds),*], bump, space = 8 + std::mem::size_of::<#account_type>())]
                    __SEAHORSE_INIT__: account![[[init, payer = #payer, seeds = [#(#seeds),*], bump, space = 8 + std::mem::size_of::<#account_type>()]]],
                }
            }
            AccountInit::TokenAccount { payer, seeds, mint, authority } => {
                let payer = ident(payer);
                let mint = ident(mint);
                let authority = ident(authority);

                quote! {
                    __SEAHORSE_INIT__: account![[[init, payer = #payer, seeds = [#(#seeds),*], bump, token::mint = #mint, token::authority = #authority]]],
                }
            },
            AccountInit::TokenMint { payer, seeds, decimals, authority } => {
                let payer = ident(payer);
                let decimals = Literal::u8_unsuffixed(*decimals);
                let authority = ident(authority);

                quote! {
                    __SEAHORSE_INIT__: account![[[init, payer = #payer, seeds = [#(#seeds),*], bump, mint::decimals = #decimals, mint::authority = #authority]]],
                }
            },
            AccountInit::AssociatedTokenAccount { payer, mint, authority } => {
              let payer = ident(payer);
              let mint = ident(mint);
              let authority = ident(authority);

              quote! {
                  __SEAHORSE_INIT__: account![[[init, payer = #payer, associated_token::mint = #mint, associated_token::authority = #authority]]],
              }
          }
        })
    }
}

impl ToTokens for TyDef {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            TyDef::Struct { name, fields, .. } => {
                let name = ident(name);

                tokens.extend(quote! {
                    pub struct #name {
                        #(#fields),*
                    }
                });
            }
            TyDef::Enum { name, options } => {
                let name = ident(name);
                let options = options.iter().map(|option| ident(option));

                tokens.extend(quote! {
                    pub enum #name {
                        // #[default]
                        #(#options),*
                    }
                });
            }
        }
    }
}

impl ToTokens for Field {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let name = ident(&self.name);
        let ty = &self.ty;

        tokens.extend(quote! {
            #name: #ty
        });
    }
}

impl ToTokens for Param {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let name = ident(&self.name);
        let ty = &self.ty;

        tokens.extend(quote! {
            #name: #ty
        });
    }
}

/// Newtype for a mutable parameter.
struct Mut<'p>(&'p Param);
impl<'p> ToTokens for Mut<'p> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let param = self.0;

        tokens.extend(quote! { mut #param })
    }
}

impl ToTokens for Ty {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        tokens.extend(match self {
            Ty::U8 => quote! { u8 },
            Ty::_U32 => quote! { u32 },
            Ty::U64 => quote! { u64 },
            Ty::I64 => quote! { i64 },
            Ty::F64 => quote! { f64 },
            Ty::Bool => quote! { bool },
            Ty::String => quote! { String },
            Ty::Unit => quote! { () },
            Ty::Array(ty, TyParam::Exact(len)) => {
                let len = Literal::usize_unsuffixed(*len);

                quote! { [#ty; #len] }
            }
            Ty::Pubkey => quote! { Pubkey },
            Ty::DefinedName(name) => {
                let name = ident(&name);

                quote! { #name }
            }
            Ty::ProgramResult => quote! { Result<()> },
            Ty::Context(name) => {
                let name = ident(&name);

                quote! { Context<#name> }
            }
            Ty::Signer => quote! { Signer },
            Ty::Empty(ty) => quote! { #ty },
            Ty::TokenAccount => quote! { token::TokenAccount },
            Ty::TokenMint => quote! { token::Mint },
            Ty::AssociatedTokenAccount => quote! { token::TokenAccount },
            Ty::ExactDefined {
                name, is_acc: true, ..
            } => {
                let name = ident(&name);

                quote! { Account<'_, #name> }
            }
            Ty::ExactDefined { name, .. } => {
                let name = ident(&name);

                quote! { #name }
            }
            Ty::Function { .. }
            | Ty::List(..)
            | Ty::Array(_, TyParam::Any)
            | Ty::Iter(..)
            | Ty::SystemProgram
            | Ty::TokenProgram
            | Ty::AssociatedTokenProgram
            | Ty::Rent
            | Ty::Defined(..)
            | Ty::Tuple(..)
            | Ty::Union(..)
            | Ty::Never
            | Ty::Any => {
                panic!("Encountered an unformattable type {:?}", self)
            }
        })
    }
}

impl ToTokens for FunctionDef {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let FunctionDef {
            name,
            params,
            returns,
            body,
        } = self;
        let name = ident(name);

        let params = params.iter().map(Mut);

        tokens.extend(match returns {
            Ty::Unit => quote! {
                pub fn #name(#(#params),*) {
                    #(#body)*
                }
            },
            returns => quote! {
                pub fn #name(#(#params),*) -> #returns {
                    #(#body)*
                }
            },
        });
    }
}

impl ToTokens for Statement {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        tokens.extend(match self {
            Statement::Break => quote! {
                break;
            },
            Statement::Continue => quote! {
                continue;
            },
            Statement::Return { value } => {
                let value = match value {
                    Some(value) => Some(Grouped(&value)),
                    None => None,
                };

                quote! {
                    return #value;
                }
            }
            Statement::ReturnOk => quote! {
                Ok(())
            },
            Statement::Assign { left, right } => {
                let right = Grouped(&right);

                quote! {
                    #left = #right;
                }
            }
            Statement::OpAssign { left, op, right } => {
                let right = Grouped(&right);
                let oa = match op {
                    Operator::Add => quote! { += },
                    Operator::Sub => quote! { -= },
                    Operator::Mul => quote! { *= },
                    Operator::Div => quote! { /= },
                    Operator::FloorDiv => quote! { /= },
                    Operator::Mod => quote! { %= },
                    // Operator::Pow => quote! { **= }
                    _ => {
                        panic!("Encountered an unformattable op-assign");
                    }
                };

                quote! {
                     #left #oa #right;
                }
            }
            Statement::Expression { value } => {
                let value = Grouped(&value);

                quote! {
                    #value;
                }
            }
            Statement::If {
                cond,
                body,
                or_else,
            } => {
                let cond = Grouped(&cond);

                match or_else {
                    None => quote! {
                        if #cond {
                            #(#body)*
                        }
                    },
                    Some(or_else) => quote! {
                        if #cond {
                            #(#body)*
                        }
                        else {
                            #(#or_else)*
                        }
                    },
                }
            }
            Statement::While { cond, body } => {
                let cond = Grouped(&cond);

                quote! {
                    while #cond {
                        #(#body)*
                    }
                }
            }
            Statement::ForIn { target, iter, body } => {
                // Unnecessary, but makes range iters cleaner
                let iter = match iter {
                    Expression::Range { .. } => quote! { #iter },
                    _ => quote! { #iter.clone().into_iter() },
                };

                quote! {
                    for #target in #iter {
                        #(#body)*
                    }
                }
            }
            Statement::Noop => quote! {},
            Statement::Declare { name, ty, init } => {
                let name = ident(name);
                let init = Grouped(&init);

                match ty {
                    None => quote! {
                        let mut #name = #init;
                    },
                    Some(ty) => quote! {
                        let mut #name: #ty = #init;
                    },
                }
            }
            Statement::DeclareFromContext { name, .. } => {
                let name = ident(&name);

                quote! {
                    let mut #name = &mut ctx.accounts.#name;
                }
            }
            Statement::Require { cond, throw } => {
                let cond = Grouped(&cond);
                let throw = ident(&throw);

                quote! {
                    require!(#cond, ProgramError::#throw);
                }
            }
            Statement::RawAssert { .. }
            | Statement::RawAssign { .. }
            | Statement::RawExpression { .. } => {
                panic!("Encountered an unformattable statement")
            }
        });
    }
}

/// Newtype for an expression that has already been grouped by a syntax element outside of it, and
/// therefore doesn't need extra parentheses if it's a binary/unary operation. Just to clean up the
/// generated code a bit.
struct Grouped<'e>(&'e Expression);

impl<'e> ToTokens for Grouped<'e> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        tokens.extend(match self.0 {
            Expression::BinOp { left, op, right } => quote! {
                #left #op #right
            },
            Expression::UnOp { op, value } => quote! {
                #op #value
            },
            Expression::As { value, as_type } => quote! {
                #value as #as_type
            },
            _ => {
                let expression = self.0;
                quote! { #expression }
            }
        });
    }
}

impl ToTokens for Expression {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        tokens.extend(match self {
            Expression::BinOp { left, op, right } => quote! {
                // Parenthesizing everything, not going to bother figuring out operator precedence
                (#left #op #right)
            },
            Expression::Attribute { value, attr } => {
                let attr = ident(&attr);

                quote! { #value.#attr }
            }
            Expression::StaticAttribute { value, attr } => {
                let attr = ident(&attr);

                quote! { #value::#attr }
            }
            Expression::Index { value, index } => {
                let index = Grouped(&index);

                quote! {
                    #value[#index as usize]
                }
            }
            Expression::UnOp { op, value } => quote! {
                (#op #value)
            },
            Expression::Call { func, args } => {
                let args = args.iter().map(|arg| Grouped(arg));

                quote! {
                    #func(#(#args),*)
                }
            }
            Expression::Range { from, to } => quote! {
                #from .. #to
            },
            Expression::Comprehension { element, parts } => {
                let mut comprehension = quote! { list.push(#element); };
                for part in parts.iter().rev() {
                    comprehension = match part {
                        ComprehensionPart::For { target, iter } => {
                            let iter = Grouped(&iter);

                            quote! {
                                for #target in #iter {
                                    #comprehension
                                }
                            }
                        }
                        ComprehensionPart::If { cond } => {
                            let cond = Grouped(&cond);

                            quote! {
                                if #cond {
                                    #comprehension
                                }
                            }
                        }
                    }
                }

                quote! {
                    {
                        let mut list = Vec::new();
                        #comprehension
                        list
                    }
                }
            }
            Expression::As { value, as_type } => quote! {
                (#value as #as_type)
            },
            Expression::Coerce { value, as_type } => {
                let value = Grouped(&value);

                quote! {
                    <#as_type as TryFrom<_>>::try_from(#value).unwrap()
                }
            }
            Expression::Block { body, returns } => quote! {
                {
                    #(#body)*
                    #returns
                }
            },
            Expression::Log { format, args } => {
                let args = args.iter().map(|arg| Grouped(arg));

                quote! {
                    msg!(#format, #(#args),*)
                }
            }
            Expression::Initialized { name } => {
                let name = ident(&name);

                quote! { #name }
            }
            Expression::SolTransfer {
                from,
                to,
                amount,
                pda,
            } => {
                if *pda {
                    quote! {
                        {
                            let amount = #amount;
                            **#from.to_account_info().try_borrow_mut_lamports()? -= amount;
                            **#to.to_account_info().try_borrow_mut_lamports()? += amount;
                        }
                    }
                } else {
                    let from_key = from.clone().with_call("key", vec![]);
                    let to_key = to.clone().with_call("key", vec![]);

                    quote! {
                        solana_program::program::invoke(
                            &solana_program::system_instruction::transfer(
                                &#from_key,
                                &#to_key,
                                #amount
                            ),
                            &[
                                (#from).to_account_info(),
                                (#to).to_account_info(),
                                ctx.accounts.system_program.to_account_info()
                            ]
                        )
                    }
                }
            }
            Expression::CpiCall { cpi, signer } => {
                let (program, accounts) = match &**cpi {
                    Cpi::TokenTransfer {
                        from,
                        authority,
                        to,
                        ..
                    } => (
                        quote! { ctx.accounts.token_program.to_account_info() },
                        quote! {
                            token::Transfer {
                                from: #from.to_account_info(),
                                authority: #authority.to_account_info(),
                                to: #to.to_account_info()
                            }
                        },
                    ),
                    Cpi::Mint {
                        mint,
                        authority,
                        to,
                        ..
                    } => (
                        quote! { ctx.accounts.token_program.to_account_info() },
                        quote! {
                            token::MintTo {
                                mint: #mint.to_account_info(),
                                authority: #authority.to_account_info(),
                                to: #to.to_account_info()
                            }
                        },
                    ),
                    Cpi::Burn {
                        mint,
                        authority,
                        holder,
                        ..
                    } => (
                        quote! { ctx.accounts.token_program.to_account_info() },
                        quote! {
                            token::Burn {
                                mint: #mint.to_account_info(),
                                authority: #authority.to_account_info(),
                                from: #holder.to_account_info()
                            }
                        },
                    ),
                };

                let cpi_context = match signer {
                    Some(seeds) => quote! {
                        CpiContext::new_with_signer(#program, #accounts, &[&[#(#seeds),*]])
                    },
                    None => quote! { CpiContext::new(#program, #accounts) },
                };

                match &**cpi {
                    Cpi::TokenTransfer { amount, .. } => {
                        let amount = Grouped(&amount);

                        quote! {
                            token::transfer(#cpi_context, #amount)?
                        }
                    }
                    Cpi::Mint { amount, .. } => {
                        let amount = Grouped(&amount);

                        quote! {
                            token::mint_to(#cpi_context, #amount)?
                        }
                    }
                    Cpi::Burn { amount, .. } => {
                        let amount = Grouped(&amount);

                        quote! {
                            token::burn(#cpi_context, #amount)?
                        }
                    }
                }
            }
            Expression::GetBump { name } => quote! {
                *ctx.bumps.get(#name).unwrap()
            },
            Expression::FString { format, args } => quote! {
                format!(#format, #(#args),*)
            },
            Expression::Clone(value) => {
                let value = Grouped(&value);

                quote! { (#value).clone() }
            }
            Expression::List(value) => {
                let value = value.iter().map(|element| Grouped(element));

                quote! {
                    [#(#value),*]
                }
            }
            Expression::Tuple(value) => {
                let value = value.iter().map(|element| Grouped(element));

                quote! {
                    (#(#value),*)
                }
            }
            Expression::Int(value) => {
                let value = Literal::i64_unsuffixed(*value);

                quote! {
                    #value
                }
            }
            Expression::Float(value) => quote! {
                #value
            },
            Expression::String(value) => quote! {
                #value
            },
            Expression::Bool(value) => quote! {
                #value
            },
            Expression::Id(name) => {
                let name = ident(name);

                quote! { #name }
            }
            Expression::RawCall { .. } | Expression::RawFString { .. } => {
                panic!("Encountered an unformattable expression")
            }
        });
    }
}

impl ToTokens for Operator {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        tokens.extend(match self {
            Operator::Add => quote! { + },
            Operator::Sub => quote! { - },
            Operator::Mul => quote! { * },
            Operator::Div => quote! { / },
            Operator::FloorDiv => quote! { / },
            Operator::Mod => quote! { % },
            Operator::Pow => quote! { ** },

            Operator::LShift => quote! { << },
            Operator::RShift => quote! { >> },
            Operator::BitOr => quote! { | },
            Operator::BitXor => quote! { ^ },
            Operator::BitAnd => quote! { & },

            Operator::And => quote! { && },
            Operator::Or => quote! { || },

            Operator::Lt => quote! { < },
            Operator::Gt => quote! { > },
            Operator::Lte => quote! { <= },
            Operator::Gte => quote! { >= },
            Operator::Eq => quote! { == },
            Operator::NotEq => quote! { != },
        });
    }
}

impl ToTokens for UnaryOperator {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        tokens.extend(match self {
            UnaryOperator::Pos => quote! { + },
            UnaryOperator::Neg => quote! { - },
            UnaryOperator::Not => quote! { ! },
            UnaryOperator::Inv => quote! { ~ },
        })
    }
}

impl ToTokens for Pattern {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        tokens.extend(match self {
            Pattern::Single(name) => {
                let name = ident(&name);

                quote! { #name }
            }
        })
    }
}

pub fn from_seahorse_ast(ast: Program, program_name: String) -> Result<String, CoreError> {
    // println!("{:#?}", ast);

    let Program {
        id,
        defs,
        instructions,
        errors,
    } = ast;

    let mut contexts = Vec::new();
    for instruction in instructions.iter() {
        contexts.push((
            instruction.context_name.clone(),
            instruction.accounts_context.clone(),
        ));
    }

    let mut tokens = TokenStream::new();

    tokens.extend(quote! {
        use anchor_lang::prelude::*;
        use anchor_spl::token;
        use anchor_spl::associated_token;
        use anchor_lang::solana_program;
        use std::convert::TryFrom;

        declare_id!(#id);

        #(#defs)*
    });

    for (name, accounts_context) in contexts.into_iter() {
        let name = ident(&name);
        let AccountsContext { accounts, params } = accounts_context;
        let params = params.into_iter().map(|(name, ty)| {
            let name = ident(&name);

            quote! { #name: #ty }
        });

        if params.len() > 0 {
            tokens.extend(quote! {
                #[derive(Accounts)]
                #[instruction(#(#params),*)]
                pub struct #name<'info> {
                    #(#accounts),*
                }
            });
        } else {
            tokens.extend(quote! {
                #[derive(Accounts)]
                pub struct #name<'info> {
                    #(#accounts),*
                }
            });
        }
    }

    let errors = errors.into_iter().map(|(throw, msg)| {
        let throw = ident(&throw);

        quote! {
            #[msg(#msg)]
            #throw
        }
    });

    let error_name = ident(&"ProgramError");
    let program_name = ident(&program_name.to_snake_case());
    tokens.extend(quote! {
        #[program]
        pub mod #program_name {
            use super::*;

            #(#instructions)*
        }
    });

    if errors.len() > 0 {
        tokens.extend(quote! {
            #[error_code]
            pub enum #error_name {
                #(#errors),*
            }
        });
    }

    // Run rustfmt
    let config = Config {
        // Maybe there will be something here one day
        ..Config::default()
    };
    let mut source = rustfmt_config(config, tokens).map_err(|err| match err {
        RustfmtError::NoRustfmt => CoreError::make_raw(
            "rustfmt not installed",
            "Help: Seahorse depends on rustfmt, which is part of the Rust toolchain. To install:\n\n    rustup components add rustfmt"
        ),
        RustfmtError::Rustfmt(message) => CoreError::make_raw(
            "rustfmt error",
            format!("{}This is most likely an error in Seahorse.", message)
        ),
        _ => CoreError::make_raw("unknown rustfmt error", ""),
    })?;

    // Put init lines back
    let re = Regex::new(r"(?s)__SEAHORSE_INIT__: account!\[\[\[(.*?)\]\]\],").unwrap();
    source = re.replace_all(&source, "#[account($1)]").to_string();

    // Perform some simple regex-based transformations
    // NOTE makes some bold assumptions about the spacing in rustfmt's output. Likely won't cause
    // any major problems, at worst has the potential to corrupt very weird strings

    // Add a blank line after the end of statements or blocks (semicolon or right curly at line
    // end), but not before the end of a block
    let re = Regex::new(r"([};])\n(\s*[^\s}])").unwrap();
    source = re.replace_all(&source, "$1\n\n$2").to_string();

    // Remove blank lines between blocks of (single-line) "use ...;"
    // Regex::replace_all only catches non-overlapping occurrences of the pattern, so we have to
    // run this twice to get everything
    let re = Regex::new(r"(use .*?;)\n\n(\s*use )").unwrap();
    source = re.replace_all(&source, "$1\n$2").to_string();
    let re = Regex::new(r"(use .*?;)\n\n(\s*use )").unwrap();
    source = re.replace_all(&source, "$1\n$2").to_string();

    // Remove blank lines between blocks of (single-line) "let ...;"
    // ^ ditto with use statements, run this twice to catch everything
    let re = Regex::new(r"(let .*?;)\n\n(\s*let )").unwrap();
    source = re.replace_all(&source, "$1\n$2").to_string();
    let re = Regex::new(r"(let .*?;)\n\n(\s*let )").unwrap();
    source = re.replace_all(&source, "$1\n$2").to_string();

    // Collapse any accidental doubled blank lines
    let re = Regex::new(r"\n\n\n+").unwrap();
    source = re.replace_all(&source, "\n\n").to_string();

    return Ok(source);
}
