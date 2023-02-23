// ===== dot/mod.rs =====

pub mod program;

// ===== dot/program.rs =====

#![allow(unused_imports)]
#![allow(unused_variables)]
#![allow(unused_mut)]
use crate::{id, seahorse_util::*};
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
                to: user_acc.clone().to_account_info(),
            },
            &[Mutable::new(vec![
                "hello".to_string().as_bytes().as_ref(),
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
    use std::{
        collections::HashMap,
        fmt::Debug,
        ops::{Deref, Index, IndexMut},
    };

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

    pub trait IndexWrapped {
        type Output;

        fn index_wrapped(&self, index: i128) -> &Self::Output;
    }

    pub trait IndexWrappedMut: IndexWrapped {
        fn index_wrapped_mut(&mut self, index: i128) -> &mut <Self as IndexWrapped>::Output;
    }

    impl<T> IndexWrapped for Vec<T> {
        type Output = T;

        fn index_wrapped(&self, mut index: i128) -> &Self::Output {
            if index < 0 {
                index += self.len() as i128;
            }

            let index: usize = index.try_into().unwrap();

            self.index(index)
        }
    }

    impl<T> IndexWrappedMut for Vec<T> {
        fn index_wrapped_mut(&mut self, mut index: i128) -> &mut <Self as IndexWrapped>::Output {
            if index < 0 {
                index += self.len() as i128;
            }

            let index: usize = index.try_into().unwrap();

            self.index_mut(index)
        }
    }

    impl<T, const N: usize> IndexWrapped for [T; N] {
        type Output = T;

        fn index_wrapped(&self, mut index: i128) -> &Self::Output {
            if index < 0 {
                index += N as i128;
            }

            let index: usize = index.try_into().unwrap();

            self.index(index)
        }
    }

    impl<T, const N: usize> IndexWrappedMut for [T; N] {
        fn index_wrapped_mut(&mut self, mut index: i128) -> &mut <Self as IndexWrapped>::Output {
            if index < 0 {
                index += N as i128;
            }

            let index: usize = index.try_into().unwrap();

            self.index_mut(index)
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
    macro_rules! seahorse_const {
        ($ name : ident , $ value : expr) => {
            macro_rules! $name {
                () => {
                    $value
                };
            }

            pub(crate) use $name;
        };
    }

    pub trait Loadable {
        type Loaded;

        fn load(stored: Self) -> Self::Loaded;

        fn store(loaded: Self::Loaded) -> Self;
    }

    macro_rules! Loaded {
        ($ name : ty) => {
            <$name as Loadable>::Loaded
        };
    }

    pub(crate) use Loaded;

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

    pub(crate) use assign;

    pub(crate) use index_assign;

    pub(crate) use seahorse_const;
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

