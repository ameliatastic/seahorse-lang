// ===== dot/mod.rs =====

pub mod program;

pub mod util;

// ===== dot/program.rs =====

#![allow(unused_imports)]
#![allow(unused_variables)]
#![allow(unused_mut)]
use crate::dot::util::more_data::MoreData;
use crate::{id, seahorse_util::*};
use anchor_lang::{prelude::*, solana_program};
use anchor_spl::token::{self, Mint, Token, TokenAccount};
use std::{cell::RefCell, rc::Rc};

#[account]
#[derive(Debug)]
pub struct Data {
    pub array_2d: [[i32; 2]; 2],
    pub int_list: Vec<i32>,
    pub int_list_2d: Vec<Vec<i32>>,
    pub string: String,
    pub nested: Nested,
    pub nested_list: Vec<Nested>,
    pub flag: Flag,
    pub more_data: MoreData,
}

impl<'info, 'entrypoint> Data {
    pub fn load(
        account: &'entrypoint mut Box<Account<'info, Self>>,
        programs_map: &'entrypoint ProgramsMap<'info>,
    ) -> Mutable<LoadedData<'info, 'entrypoint>> {
        let array_2d = Mutable::new(
            account
                .array_2d
                .clone()
                .map(|element| Mutable::new(element.map(|element| element))),
        );

        let int_list = Mutable::new(
            account
                .int_list
                .clone()
                .into_iter()
                .map(|element| element)
                .collect(),
        );

        let int_list_2d = Mutable::new(
            account
                .int_list_2d
                .clone()
                .into_iter()
                .map(|element| Mutable::new(element.into_iter().map(|element| element).collect()))
                .collect(),
        );

        let string = account.string.clone();
        let nested = Mutable::new(Nested::load(account.nested.clone()));
        let nested_list = Mutable::new(
            account
                .nested_list
                .clone()
                .into_iter()
                .map(|element| Mutable::new(Nested::load(element)))
                .collect(),
        );

        let flag = account.flag.clone();
        let more_data = Mutable::new(MoreData::load(account.more_data.clone()));

        Mutable::new(LoadedData {
            __account__: account,
            __programs__: programs_map,
            array_2d,
            int_list,
            int_list_2d,
            string,
            nested,
            nested_list,
            flag,
            more_data,
        })
    }

    pub fn store(loaded: Mutable<LoadedData>) {
        let mut loaded = loaded.borrow_mut();
        let array_2d = loaded
            .array_2d
            .clone()
            .borrow()
            .clone()
            .map(|element| element.borrow().clone().map(|element| element));

        loaded.__account__.array_2d = array_2d;

        let int_list = loaded
            .int_list
            .clone()
            .borrow()
            .clone()
            .into_iter()
            .map(|element| element)
            .collect();

        loaded.__account__.int_list = int_list;

        let int_list_2d = loaded
            .int_list_2d
            .clone()
            .borrow()
            .clone()
            .into_iter()
            .map(|element| {
                element
                    .borrow()
                    .clone()
                    .into_iter()
                    .map(|element| element)
                    .collect()
            })
            .collect();

        loaded.__account__.int_list_2d = int_list_2d;

        let string = loaded.string.clone();

        loaded.__account__.string = string;

        let nested = Nested::store(loaded.nested.clone().borrow().clone());

        loaded.__account__.nested = nested;

        let nested_list = loaded
            .nested_list
            .clone()
            .borrow()
            .clone()
            .into_iter()
            .map(|element| Nested::store(element.borrow().clone()))
            .collect();

        loaded.__account__.nested_list = nested_list;

        let flag = loaded.flag.clone();

        loaded.__account__.flag = flag;

        let more_data = MoreData::store(loaded.more_data.clone().borrow().clone());

        loaded.__account__.more_data = more_data;
    }
}

#[derive(Debug)]
pub struct LoadedData<'info, 'entrypoint> {
    pub __account__: &'entrypoint mut Box<Account<'info, Data>>,
    pub __programs__: &'entrypoint ProgramsMap<'info>,
    pub array_2d: Mutable<[Mutable<[i32; 2]>; 2]>,
    pub int_list: Mutable<Vec<i32>>,
    pub int_list_2d: Mutable<Vec<Mutable<Vec<i32>>>>,
    pub string: String,
    pub nested: Mutable<Loaded!(Nested)>,
    pub nested_list: Mutable<Vec<Mutable<Loaded!(Nested)>>>,
    pub flag: Flag,
    pub more_data: Mutable<Loaded!(MoreData)>,
}

#[derive(AnchorSerialize, AnchorDeserialize, Clone, Debug)]
pub struct Deep {
    pub num: i32,
}

#[derive(Clone, Debug, Default)]
pub struct LoadedDeep {
    pub num: i32,
}

impl Mutable<LoadedDeep> {
    pub fn __init__(&self, mut num: i32) -> () {
        assign!(self.borrow_mut().num, num);
    }
}

impl LoadedDeep {
    pub fn __new__(num: i32) -> Mutable<Self> {
        let obj = Mutable::new(LoadedDeep::default());

        obj.__init__(num);

        return obj;
    }
}

impl Loadable for Deep {
    type Loaded = LoadedDeep;

    fn load(stored: Self) -> Self::Loaded {
        Self::Loaded { num: stored.num }
    }

    fn store(loaded: Self::Loaded) -> Self {
        Self { num: loaded.num }
    }
}

#[derive(Clone, Debug, PartialEq, AnchorSerialize, AnchorDeserialize, Copy)]
pub enum Flag {
    OFF,
    ON,
}

impl Default for Flag {
    fn default() -> Self {
        Flag::OFF
    }
}

#[event]
pub struct MyEvent {
    pub nums: Vec<i32>,
}

#[derive(Clone, Debug, Default)]
pub struct LoadedMyEvent {
    pub nums: Mutable<Vec<i32>>,
}

impl Mutable<LoadedMyEvent> {
    fn __emit__(&self) {
        let e = self.borrow();

        emit!(MyEvent {
            nums: e
                .nums
                .clone()
                .borrow()
                .clone()
                .into_iter()
                .map(|element| element)
                .collect()
        })
    }
}

impl Loadable for MyEvent {
    type Loaded = LoadedMyEvent;

    fn load(stored: Self) -> Self::Loaded {
        Self::Loaded {
            nums: Mutable::new(stored.nums.into_iter().map(|element| element).collect()),
        }
    }

    fn store(loaded: Self::Loaded) -> Self {
        Self {
            nums: loaded
                .nums
                .clone()
                .borrow()
                .clone()
                .into_iter()
                .map(|element| element)
                .collect(),
        }
    }
}

#[derive(AnchorSerialize, AnchorDeserialize, Clone, Debug)]
pub struct Nested {
    pub deep: Deep,
}

#[derive(Clone, Debug, Default)]
pub struct LoadedNested {
    pub deep: Mutable<Loaded!(Deep)>,
}

impl Mutable<LoadedNested> {
    pub fn __init__(&self, mut num: i32) -> () {
        assign!(
            self.borrow_mut().deep,
            <Loaded!(Deep)>::__new__(num.clone())
        );
    }

    pub fn reset(&self) -> () {
        assign!(self.borrow_mut().deep, <Loaded!(Deep)>::__new__(0));
    }
}

impl LoadedNested {
    pub fn __new__(num: i32) -> Mutable<Self> {
        let obj = Mutable::new(LoadedNested::default());

        obj.__init__(num);

        return obj;
    }
}

impl Loadable for Nested {
    type Loaded = LoadedNested;

    fn load(stored: Self) -> Self::Loaded {
        Self::Loaded {
            deep: Mutable::new(Deep::load(stored.deep)),
        }
    }

    fn store(loaded: Self::Loaded) -> Self {
        Self {
            deep: Deep::store(loaded.deep.clone().borrow().clone()),
        }
    }
}

pub fn init_handler<'info>(
    mut signer: SeahorseSigner<'info, '_>,
    mut data: Empty<Mutable<LoadedData<'info, '_>>>,
) -> () {
    let mut init_data = data.account.clone();

    assign!(init_data.borrow_mut().int_list, Mutable::new(vec![1, 2]));

    assign!(
        init_data.borrow_mut().int_list_2d,
        Mutable::new(vec![Mutable::new(vec![3, 4]), Mutable::new(vec![5, 6])])
    );

    assign!(init_data.borrow_mut().string, "Hello".to_string());

    assign!(init_data.borrow_mut().nested, <Loaded!(Nested)>::__new__(7));

    assign!(
        init_data.borrow_mut().nested_list,
        Mutable::new(vec![
            <Loaded!(Nested)>::__new__(8),
            <Loaded!(Nested)>::__new__(9)
        ])
    );

    assign!(
        init_data.borrow_mut().more_data,
        <Loaded!(MoreData)>::__new__(10)
    );
}

pub fn test_stored_mutables_handler<'info>(
    mut signer: SeahorseSigner<'info, '_>,
    mut data: Mutable<LoadedData<'info, '_>>,
) -> () {
    assign!(
        (*(*data
            .borrow_mut()
            .array_2d
            .borrow_mut()
            .index_wrapped_mut(0.into()))
        .borrow_mut()
        .index_wrapped_mut(0.into())),
        1
    );

    data.borrow().int_list.borrow_mut().push(0);

    assign!(
        (*(*data
            .borrow_mut()
            .int_list_2d
            .borrow_mut()
            .index_wrapped_mut(0.into()))
        .borrow_mut()
        .index_wrapped_mut((-1).into())),
        0
    );

    assign!(
        data.borrow_mut().string,
        data.borrow().string.clone() + &" World".to_string()
    );

    data.borrow().nested.reset();

    (*data.borrow().nested_list.borrow().index_wrapped(0.into())).reset();

    data.borrow()
        .nested_list
        .borrow_mut()
        .push(<Loaded!(Nested)>::__new__(10));

    assign!(data.borrow_mut().flag, Flag::ON);

    assign!(
        data.borrow_mut().more_data,
        <Loaded!(MoreData)>::__new__(11)
    );
}

// ===== dot/util/mod.rs =====

pub mod more_data;

// ===== dot/util/more_data.rs =====

#![allow(unused_imports)]
#![allow(unused_variables)]
#![allow(unused_mut)]
use crate::{id, seahorse_util::*};
use anchor_lang::{prelude::*, solana_program};
use anchor_spl::token::{self, Mint, Token, TokenAccount};
use std::{cell::RefCell, rc::Rc};

#[derive(AnchorSerialize, AnchorDeserialize, Clone, Debug)]
pub struct MoreData {
    pub num: i32,
}

#[derive(Clone, Debug, Default)]
pub struct LoadedMoreData {
    pub num: i32,
}

impl Mutable<LoadedMoreData> {
    pub fn __init__(&self, mut num: i32) -> () {
        assign!(self.borrow_mut().num, num);
    }
}

impl LoadedMoreData {
    pub fn __new__(num: i32) -> Mutable<Self> {
        let obj = Mutable::new(LoadedMoreData::default());

        obj.__init__(num);

        return obj;
    }
}

impl Loadable for MoreData {
    type Loaded = LoadedMoreData;

    fn load(stored: Self) -> Self::Loaded {
        Self::Loaded { num: stored.num }
    }

    fn store(loaded: Self::Loaded) -> Self {
        Self { num: loaded.num }
    }
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
mod stored_mutables {
    use super::*;
    use seahorse_util::*;
    use std::collections::HashMap;

    #[derive(Accounts)]
    pub struct Init<'info> {
        #[account(mut)]
        pub signer: Signer<'info>,
        # [account (init , space = std :: mem :: size_of :: < dot :: program :: Data > () + 8 + (1024 as usize) , payer = signer , seeds = [signer . key () . as_ref ()] , bump)]
        pub data: Box<Account<'info, dot::program::Data>>,
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
        let signer = SeahorseSigner {
            account: &ctx.accounts.signer,
            programs: &programs_map,
        };

        let data = Empty {
            account: dot::program::Data::load(&mut ctx.accounts.data, &programs_map),
            bump: ctx.bumps.get("data").map(|bump| *bump),
        };

        init_handler(signer.clone(), data.clone());

        dot::program::Data::store(data.account);

        return Ok(());
    }

    #[derive(Accounts)]
    pub struct TestStoredMutables<'info> {
        #[account(mut)]
        pub signer: Signer<'info>,
        #[account(mut)]
        pub data: Box<Account<'info, dot::program::Data>>,
    }

    pub fn test_stored_mutables(ctx: Context<TestStoredMutables>) -> Result<()> {
        let mut programs = HashMap::new();
        let programs_map = ProgramsMap(programs);
        let signer = SeahorseSigner {
            account: &ctx.accounts.signer,
            programs: &programs_map,
        };

        let data = dot::program::Data::load(&mut ctx.accounts.data, &programs_map);

        test_stored_mutables_handler(signer.clone(), data.clone());

        dot::program::Data::store(data);

        return Ok(());
    }
}

