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
pub struct Calculator {
    pub owner: Pubkey,
    pub display: i64,
}

impl<'info, 'entrypoint> Calculator {
    pub fn load(
        account: &'entrypoint mut Box<Account<'info, Self>>,
        programs_map: &'entrypoint ProgramsMap<'info>,
    ) -> Mutable<LoadedCalculator<'info, 'entrypoint>> {
        let owner = account.owner.clone();
        let display = account.display;

        Mutable::new(LoadedCalculator {
            __account__: account,
            __programs__: programs_map,
            owner,
            display,
        })
    }

    pub fn store(loaded: Mutable<LoadedCalculator>) {
        let mut loaded = loaded.borrow_mut();
        let owner = loaded.owner.clone();

        loaded.__account__.owner = owner;

        let display = loaded.display;

        loaded.__account__.display = display;
    }
}

#[derive(Debug)]
pub struct LoadedCalculator<'info, 'entrypoint> {
    pub __account__: &'entrypoint mut Box<Account<'info, Calculator>>,
    pub __programs__: &'entrypoint ProgramsMap<'info>,
    pub owner: Pubkey,
    pub display: i64,
}

#[derive(Clone, Debug, PartialEq, AnchorSerialize, AnchorDeserialize, Copy)]
pub enum Operation {
    ADD,
    SUB,
    MUL,
    DIV,
}

impl Default for Operation {
    fn default() -> Self {
        Operation::ADD
    }
}

pub fn do_operation_handler<'info>(
    mut owner: SeahorseSigner<'info, '_>,
    mut calculator: Mutable<LoadedCalculator<'info, '_>>,
    mut op: Operation,
    mut num: i64,
) -> () {
    if !(owner.key() == calculator.borrow().owner) {
        panic!("This is not your calculator!");
    }

    if op == Operation::ADD {
        assign!(
            calculator.borrow_mut().display,
            calculator.borrow().display + num
        );
    } else {
        if op == Operation::SUB {
            assign!(
                calculator.borrow_mut().display,
                calculator.borrow().display - num
            );
        } else {
            if op == Operation::MUL {
                assign!(
                    calculator.borrow_mut().display,
                    calculator.borrow().display * num
                );
            } else {
                if op == Operation::DIV {
                    assign!(
                        calculator.borrow_mut().display,
                        calculator.borrow().display / num
                    );
                }
            }
        }
    }
}

pub fn init_calculator_handler<'info>(
    mut owner: SeahorseSigner<'info, '_>,
    mut calculator: Empty<Mutable<LoadedCalculator<'info, '_>>>,
) -> () {
    let mut calculator = calculator.account.clone();

    assign!(calculator.borrow_mut().owner, owner.key());
}

pub fn reset_calculator_handler<'info>(
    mut owner: SeahorseSigner<'info, '_>,
    mut calculator: Mutable<LoadedCalculator<'info, '_>>,
) -> () {
    solana_program::msg!(
        "{:?} {} {:?}",
        owner.key(),
        "is resetting a calculator".to_string(),
        calculator.borrow().__account__.key()
    );

    if !(owner.key() == calculator.borrow().owner) {
        panic!("This is not your calculator!");
    }

    assign!(calculator.borrow_mut().display, 0);
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
mod calculator {
    use super::*;
    use seahorse_util::*;
    use std::collections::HashMap;

    #[derive(Accounts)]
    # [instruction (op : Operation , num : i64)]
    pub struct DoOperation<'info> {
        #[account(mut)]
        pub owner: Signer<'info>,
        #[account(mut)]
        pub calculator: Box<Account<'info, dot::program::Calculator>>,
    }

    pub fn do_operation(ctx: Context<DoOperation>, op: Operation, num: i64) -> Result<()> {
        let mut programs = HashMap::new();
        let programs_map = ProgramsMap(programs);
        let owner = SeahorseSigner {
            account: &ctx.accounts.owner,
            programs: &programs_map,
        };

        let calculator =
            dot::program::Calculator::load(&mut ctx.accounts.calculator, &programs_map);

        do_operation_handler(owner.clone(), calculator.clone(), op, num);

        dot::program::Calculator::store(calculator);

        return Ok(());
    }

    #[derive(Accounts)]
    pub struct InitCalculator<'info> {
        #[account(mut)]
        pub owner: Signer<'info>,
        # [account (init , space = std :: mem :: size_of :: < dot :: program :: Calculator > () + 8 , payer = owner , seeds = ["Calculator" . as_bytes () . as_ref () , owner . key () . as_ref ()] , bump)]
        pub calculator: Box<Account<'info, dot::program::Calculator>>,
        pub rent: Sysvar<'info, Rent>,
        pub system_program: Program<'info, System>,
    }

    pub fn init_calculator(ctx: Context<InitCalculator>) -> Result<()> {
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

        let calculator = Empty {
            account: dot::program::Calculator::load(&mut ctx.accounts.calculator, &programs_map),
            bump: ctx.bumps.get("calculator").map(|bump| *bump),
        };

        init_calculator_handler(owner.clone(), calculator.clone());

        dot::program::Calculator::store(calculator.account);

        return Ok(());
    }

    #[derive(Accounts)]
    pub struct ResetCalculator<'info> {
        #[account(mut)]
        pub owner: Signer<'info>,
        #[account(mut)]
        pub calculator: Box<Account<'info, dot::program::Calculator>>,
    }

    pub fn reset_calculator(ctx: Context<ResetCalculator>) -> Result<()> {
        let mut programs = HashMap::new();
        let programs_map = ProgramsMap(programs);
        let owner = SeahorseSigner {
            account: &ctx.accounts.owner,
            programs: &programs_map,
        };

        let calculator =
            dot::program::Calculator::load(&mut ctx.accounts.calculator, &programs_map);

        reset_calculator_handler(owner.clone(), calculator.clone());

        dot::program::Calculator::store(calculator);

        return Ok(());
    }
}

