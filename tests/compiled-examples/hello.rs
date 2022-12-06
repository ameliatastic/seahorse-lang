// ===== dot/mod.rs =====

pub mod program;

// ===== dot/program.rs =====

#![allow(unused_imports)]
#![allow(unused_variables)]
#![allow(unused_mut)]
use crate::{assign, id, index_assign, seahorse_util::*};
use anchor_lang::{prelude::*, solana_program};
use anchor_spl::token::{self, Mint, Token, TokenAccount};
use std::{cell::RefCell, rc::Rc};

#[account]
#[derive(Debug)]
pub struct Hello {
    pub bump: u8,
}

impl<'info, 'entrypoint> Hello {
    pub fn load(
        account: &'entrypoint mut Box<Account<'info, Self>>,
        programs_map: &'entrypoint ProgramsMap<'info>,
    ) -> Mutable<LoadedHello<'info, 'entrypoint>> {
        let bump = account.bump;

        Mutable::new(LoadedHello {
            __account__: account,
            __programs__: programs_map,
            bump,
        })
    }

    pub fn store(loaded: Mutable<LoadedHello>) {
        let mut loaded = loaded.borrow_mut();
        let bump = loaded.bump;

        loaded.__account__.bump = bump;
    }
}

#[derive(Debug)]
pub struct LoadedHello<'info, 'entrypoint> {
    pub __account__: &'entrypoint mut Box<Account<'info, Hello>>,
    pub __programs__: &'entrypoint ProgramsMap<'info>,
    pub bump: u8,
}

pub fn init_handler<'info>(
    mut owner: SeahorseSigner<'info, '_>,
    mut hello: Empty<Mutable<LoadedHello<'info, '_>>>,
    mut mint: Empty<SeahorseAccount<'info, '_, Mint>>,
) -> () {
    let mut bump = hello.bump.unwrap();
    let mut hello = hello.account.clone();

    mint.account.clone();

    assign!(hello.borrow_mut().bump, bump);
}

pub fn say_hello_handler<'info>(
    mut user_acc: SeahorseAccount<'info, '_, TokenAccount>,
    mut hello: Mutable<LoadedHello<'info, '_>>,
    mut mint: SeahorseAccount<'info, '_, Mint>,
) -> () {
    let mut bump = hello.borrow().bump;

    solana_program::msg!("{}", format!("Hello {:?}, have a token!", user_acc.owner));

    token::mint_to(
        CpiContext::new_with_signer(
            mint.programs.get("token_program"),
            token::MintTo {
                mint: mint.to_account_info(),
                authority: hello.borrow().__account__.to_account_info(),
                to: user_acc.to_account_info(),
            },
            &[Mutable::new(vec![
                "hello".as_bytes().as_ref(),
                bump.to_le_bytes().as_ref(),
            ])
            .borrow()
            .as_slice()],
        ),
        <u64 as TryFrom<_>>::try_from(1).unwrap(),
    )
    .unwrap();
}

// ===== lib.rs =====

#![allow(unused_imports)]
#![allow(unused_variables)]
#![allow(unused_mut)]

pub mod dot;

use anchor_lang::prelude::*;
use anchor_spl::{
    associated_token::{self, AssociatedToken},
    token::{self, Mint, Token, TokenAccount},
};

use dot::program::*;
use std::{cell::RefCell, rc::Rc};

declare_id!("Fg6PaFpoGXkYsidMpWTK6W2BeZ7FEfcYkg476zPFsLnS");

pub mod seahorse_util {
    use super::*;

    #[cfg(feature = "pyth-sdk-solana")]
    pub use pyth_sdk_solana::{load_price_feed_from_account_info, PriceFeed};
    use std::{collections::HashMap, fmt::Debug, ops::Deref};

    pub struct Mutable<T>(Rc<RefCell<T>>);

    impl<T> Mutable<T> {
        pub fn new(obj: T) -> Self {
            Self(Rc::new(RefCell::new(obj)))
        }
    }

    impl<T> Clone for Mutable<T> {
        fn clone(&self) -> Self {
            Self(self.0.clone())
        }
    }

    impl<T> Deref for Mutable<T> {
        type Target = Rc<RefCell<T>>;

        fn deref(&self) -> &Self::Target {
            &self.0
        }
    }

    impl<T: Debug> Debug for Mutable<T> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "{:?}", self.0)
        }
    }

    impl<T: Default> Default for Mutable<T> {
        fn default() -> Self {
            Self::new(T::default())
        }
    }

    impl<T: Clone> Mutable<Vec<T>> {
        pub fn wrapped_index(&self, mut index: i128) -> usize {
            if index >= 0 {
                return index.try_into().unwrap();
            }

            index += self.borrow().len() as i128;

            return index.try_into().unwrap();
        }
    }

    impl<T: Clone, const N: usize> Mutable<[T; N]> {
        pub fn wrapped_index(&self, mut index: i128) -> usize {
            if index >= 0 {
                return index.try_into().unwrap();
            }

            index += self.borrow().len() as i128;

            return index.try_into().unwrap();
        }
    }

    #[derive(Clone)]
    pub struct Empty<T: Clone> {
        pub account: T,
        pub bump: Option<u8>,
    }

    #[derive(Clone, Debug)]
    pub struct ProgramsMap<'info>(pub HashMap<&'static str, AccountInfo<'info>>);

    impl<'info> ProgramsMap<'info> {
        pub fn get(&self, name: &'static str) -> AccountInfo<'info> {
            self.0.get(name).unwrap().clone()
        }
    }

    #[derive(Clone, Debug)]
    pub struct WithPrograms<'info, 'entrypoint, A> {
        pub account: &'entrypoint A,
        pub programs: &'entrypoint ProgramsMap<'info>,
    }

    impl<'info, 'entrypoint, A> Deref for WithPrograms<'info, 'entrypoint, A> {
        type Target = A;

        fn deref(&self) -> &Self::Target {
            &self.account
        }
    }

    pub type SeahorseAccount<'info, 'entrypoint, A> =
        WithPrograms<'info, 'entrypoint, Box<Account<'info, A>>>;

    pub type SeahorseSigner<'info, 'entrypoint> = WithPrograms<'info, 'entrypoint, Signer<'info>>;

    #[derive(Clone, Debug)]
    pub struct CpiAccount<'info> {
        #[doc = "CHECK: CpiAccounts temporarily store AccountInfos."]
        pub account_info: AccountInfo<'info>,
        pub is_writable: bool,
        pub is_signer: bool,
        pub seeds: Option<Vec<Vec<u8>>>,
    }

    #[macro_export]
    macro_rules! assign {
        ($ lval : expr , $ rval : expr) => {{
            let temp = $rval;

            $lval = temp;
        }};
    }

    #[macro_export]
    macro_rules! index_assign {
        ($ lval : expr , $ idx : expr , $ rval : expr) => {
            let temp_rval = $rval;
            let temp_idx = $idx;

            $lval[temp_idx] = temp_rval;
        };
    }
}

#[program]
mod hello {
    use super::*;
    use seahorse_util::*;
    use std::collections::HashMap;

    #[derive(Accounts)]
    pub struct Init<'info> {
        #[account(mut)]
        pub owner: Signer<'info>,
        # [account (init , space = std :: mem :: size_of :: < dot :: program :: Hello > () + 8 , payer = owner , seeds = ["hello" . as_bytes () . as_ref ()] , bump)]
        pub hello: Box<Account<'info, dot::program::Hello>>,
        # [account (init , payer = owner , seeds = ["hello-mint" . as_bytes () . as_ref ()] , bump , mint :: decimals = 0 , mint :: authority = hello)]
        pub mint: Box<Account<'info, Mint>>,
        pub rent: Sysvar<'info, Rent>,
        pub system_program: Program<'info, System>,
        pub token_program: Program<'info, Token>,
    }

    pub fn init(ctx: Context<Init>) -> Result<()> {
        let mut programs = HashMap::new();

        programs.insert(
            "system_program",
            ctx.accounts.system_program.to_account_info(),
        );

        programs.insert(
            "token_program",
            ctx.accounts.token_program.to_account_info(),
        );

        let programs_map = ProgramsMap(programs);
        let owner = SeahorseSigner {
            account: &ctx.accounts.owner,
            programs: &programs_map,
        };

        let hello = Empty {
            account: dot::program::Hello::load(&mut ctx.accounts.hello, &programs_map),
            bump: ctx.bumps.get("hello").map(|bump| *bump),
        };

        let mint = Empty {
            account: SeahorseAccount {
                account: &ctx.accounts.mint,
                programs: &programs_map,
            },
            bump: ctx.bumps.get("mint").map(|bump| *bump),
        };

        init_handler(owner.clone(), hello.clone(), mint.clone());

        dot::program::Hello::store(hello.account);

        return Ok(());
    }

    #[derive(Accounts)]
    pub struct SayHello<'info> {
        #[account(mut)]
        pub user_acc: Box<Account<'info, TokenAccount>>,
        #[account(mut)]
        pub hello: Box<Account<'info, dot::program::Hello>>,
        #[account(mut)]
        pub mint: Box<Account<'info, Mint>>,
        pub token_program: Program<'info, Token>,
    }

    pub fn say_hello(ctx: Context<SayHello>) -> Result<()> {
        let mut programs = HashMap::new();

        programs.insert(
            "token_program",
            ctx.accounts.token_program.to_account_info(),
        );

        let programs_map = ProgramsMap(programs);
        let user_acc = SeahorseAccount {
            account: &ctx.accounts.user_acc,
            programs: &programs_map,
        };

        let hello = dot::program::Hello::load(&mut ctx.accounts.hello, &programs_map);
        let mint = SeahorseAccount {
            account: &ctx.accounts.mint,
            programs: &programs_map,
        };

        say_hello_handler(user_acc.clone(), hello.clone(), mint.clone());

        dot::program::Hello::store(hello);

        return Ok(());
    }
}

