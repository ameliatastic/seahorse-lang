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
pub struct FizzBuzz {
    pub fizz: bool,
    pub buzz: bool,
    pub n: u64,
}

impl<'info, 'entrypoint> FizzBuzz {
    pub fn load(
        account: &'entrypoint mut Box<Account<'info, Self>>,
        programs_map: &'entrypoint ProgramsMap<'info>,
    ) -> Mutable<LoadedFizzBuzz<'info, 'entrypoint>> {
        let fizz = account.fizz.clone();
        let buzz = account.buzz.clone();
        let n = account.n;

        Mutable::new(LoadedFizzBuzz {
            __account__: account,
            __programs__: programs_map,
            fizz,
            buzz,
            n,
        })
    }

    pub fn store(loaded: Mutable<LoadedFizzBuzz>) {
        let mut loaded = loaded.borrow_mut();
        let fizz = loaded.fizz.clone();

        loaded.__account__.fizz = fizz;

        let buzz = loaded.buzz.clone();

        loaded.__account__.buzz = buzz;

        let n = loaded.n;

        loaded.__account__.n = n;
    }
}

#[derive(Debug)]
pub struct LoadedFizzBuzz<'info, 'entrypoint> {
    pub __account__: &'entrypoint mut Box<Account<'info, FizzBuzz>>,
    pub __programs__: &'entrypoint ProgramsMap<'info>,
    pub fizz: bool,
    pub buzz: bool,
    pub n: u64,
}

pub fn do_fizzbuzz_handler<'info>(
    mut fizzbuzz: Mutable<LoadedFizzBuzz<'info, '_>>,
    mut n: u64,
) -> () {
    assign!(fizzbuzz.borrow_mut().fizz, (n % 3) == 0);

    assign!(fizzbuzz.borrow_mut().buzz, (n % 5) == 0);

    if (!fizzbuzz.borrow().fizz) && (!fizzbuzz.borrow().buzz) {
        assign!(fizzbuzz.borrow_mut().n, n);
    } else {
        assign!(fizzbuzz.borrow_mut().n, 0);
    }
}

pub fn init_handler<'info>(
    mut owner: SeahorseSigner<'info, '_>,
    mut fizzbuzz: Empty<Mutable<LoadedFizzBuzz<'info, '_>>>,
) -> () {
    fizzbuzz.account.clone();
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
mod fizzbuzz {
    use super::*;
    use seahorse_util::*;
    use std::collections::HashMap;

    #[derive(Accounts)]
    # [instruction (n : u64)]
    pub struct DoFizzbuzz<'info> {
        #[account(mut)]
        pub fizzbuzz: Box<Account<'info, dot::program::FizzBuzz>>,
    }

    pub fn do_fizzbuzz(ctx: Context<DoFizzbuzz>, n: u64) -> Result<()> {
        let mut programs = HashMap::new();
        let programs_map = ProgramsMap(programs);
        let fizzbuzz = dot::program::FizzBuzz::load(&mut ctx.accounts.fizzbuzz, &programs_map);

        do_fizzbuzz_handler(fizzbuzz.clone(), n);

        dot::program::FizzBuzz::store(fizzbuzz);

        return Ok(());
    }

    #[derive(Accounts)]
    pub struct Init<'info> {
        #[account(mut)]
        pub owner: Signer<'info>,
        # [account (init , space = std :: mem :: size_of :: < dot :: program :: FizzBuzz > () + 8 , payer = owner , seeds = ["fizzbuzz" . as_bytes () . as_ref () , owner . key () . as_ref ()] , bump)]
        pub fizzbuzz: Box<Account<'info, dot::program::FizzBuzz>>,
        pub rent: Sysvar<'info, Rent>,
        pub system_program: Program<'info, System>,
    }

    pub fn init(ctx: Context<Init>) -> Result<()> {
        let mut programs = HashMap::new();

        programs.insert(
            "system_program",
            ctx.accounts.system_program.to_account_info(),
        );

        let programs_map = ProgramsMap(programs);
        let owner = SeahorseSigner {
            account: &ctx.accounts.owner,
            programs: &programs_map,
        };

        let fizzbuzz = Empty {
            account: dot::program::FizzBuzz::load(&mut ctx.accounts.fizzbuzz, &programs_map),
            bump: ctx.bumps.get("fizzbuzz").map(|bump| *bump),
        };

        init_handler(owner.clone(), fizzbuzz.clone());

        dot::program::FizzBuzz::store(fizzbuzz.account);

        return Ok(());
    }
}

