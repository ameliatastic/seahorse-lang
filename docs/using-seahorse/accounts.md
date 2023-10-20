# Accounts

### Account (Base type) <a href="#account-base-type" id="account-base-type"></a>

By deriving from the base `Account` type, you can make your own program accounts.

Usage:

```
class MyAccount(Account):
  data: u64
  
@instruction
def use_my_account(my_account: MyAccount):
  # Gets the pubkey from this account.
  key = my_account.key()
  # Sets the value of `data` in this account.
  my_account.data = 1
```

Right now, accounts (and other classes in Seahorse) can only define their underlying data type by using type-annotated fields with no default value. You're also allowed to define enums to use in your accounts, like so:

```
class MyAccount(Account):
  enum: MyEnum
  data: u64


class MyEnum(Enum):
  # Enums are the only place where you need to use this syntax, which should be
  # familiar if you've used Python enums before. The numbers don't actually do
  # anything here, they're just there for parsing.
  OPTION_ONE = 1
  OPTION_TWO = 2
  OPTION_N = 3
```

### Built-in account types <a href="#built-in-account-types" id="built-in-account-types"></a>

#### Signer <a href="#signer" id="signer"></a>

Wallet that signed the transaction with this instruction call. Often used as an account payer or seed.

Usage:

```
@instruction
def use_signer(signer: Signer):
  # Gets the pubkey of the signer (as a Pubkey type).
  key = signer.key()
```

#### Empty <a href="#empty" id="empty"></a>

Account that will be initialized by this instruction. These accounts also save the `bump` used to create them, if seeds were provided in their initialization. If you create an account without seeds and try to access the `bump`, _your program will error at runtime!_

Seeds may be strings, integer numbers, lists of bytes, or other accounts. Importantly, seeds must either be literals (like a quoted string or a raw number) or instruction parameters. In order to become a seed, each of these types must be converted to _bytes_. Strings are converted into the bytes of their UTF-8 representation, integers are converted into their little-endian representation, and accounts are converted into their key.

Usage:

```
@instruction
def use_empty(empty: Empty[MyAccount], signer: Signer):
  # Initializes the empty account. `signer` will pay for the cost of creating
  # the account (rent).
  # Because the seeds uniquely identify this account among all other accounts
  # created by this program, if the same signer calls this instruction again,
  # it will fail due to trying to re-initialize an existing Solana account.
  my_account = empty.init(
    payer = signer,
    seeds = ['MyAccount', signer]
  )


  # Retrieve the bump seed used to create the PDA.
  bump = empty.bump()
```

**Padding and space**

Since accounts in Solana need to have a static size, it's difficult to know how to store data with a variable size (like a string). Seahorse makes this easy by providing two extra options when initializing an account: `padding` and `space`.

Initializing an account with `padding` gives you extra bytes to work with, which are used to store any heap-stored objects like the data of a string. You can use it like this:

```
@instruction
def use_empty(empty: Empty[MyAccount], signer: Signer):
  my_account = empty.init(
    payer = signer,
    seeds = ['MyAccount', signer],
    # 64 bytes of padding, letting you store a string up to 64 chars long!
    padding = 64
  )
```

Padding adds to the necessary size of the account's struct, meaning the underlying struct that Seahorse generates and stores on-chain. For example, a Rust `u64` takes up 8 bytes and a `String` takes up 24 bytes. If you have an account with a `u64`, a `String`, and `padding = 32`, then the entire account will be 8 + 24 + 32 = 64 bytes large.

If you want to get the size of a string, you should use Seahorse's prelude `size` function (`padding = size(string)`) to get the size of the string's data in bytes.

Alternatively, you can provide an absolute amount for the total account size with `space`:

```
@instruction
def use_empty(empty: Empty[MyAccount], signer: Signer):
  my_account = empty.init(
    payer = signer,
    seeds = ['MyAccount', signer],
    # 512 bytes of account space total - you can store a string up to
    # (512 - n) chars long, if n is the size of the account struct
    space = 512
  )
```

`padding` and `space` are mutually exclusive, specifying both will cause an error.

To summarize: the size of an account in Seahorse is either the size of its underlying Rust struct + `padding`, or just `space`. If you don't want to have to figure out the size of your account's struct, you should use `padding`.

#### UncheckedAccount <a href="#unchecked-account" id="unchecked-account"></a>

Maps directly to an Anchor `UncheckedAccount`. An account that goes through no checks to determine its type/owning program.

Usage:

```
@instruction
def use_unchecked(unchecked: UncheckedAccount):
  # Pretty much all you can do with these is get the key and use them in CPIs.
  key = unchecked.key()
```

#### Program <a href="#program" id="program"></a>

Account that allows you to invoke CPI calls! More on this in [CPI Section](https://www.seahorse.dev/using-seahorse/cross-program-invocations-cpis).

Usage:

```
@instruction
def use_program(program: Program):
  accounts = ...
  data = ...


  program.invoke(
    accounts,
    data
  )
```

#### Clock <a href="#clock" id="clock"></a>

Solana's `Clock` sysvar. The API is nearly identical to what you would expect from using the clock in a Rust program.

Usage:

```
@instruction
def use_clock(clock: Clock):
  slot:  u64 = clock.slot()
  epoch: u64 = clock.epoch()
  time:  i64 = clock.unix_timestamp()
```

#### Finding PDAs <a href="#finding-pd-as" id="finding-pd-as"></a>

Program-derived addresses can be found using the `Pubkey.find_program_address()` function, just like in Solana for Rust.

If you don't supply an argument for the second parameter `program`, then the current program's address (as defined by `declare_id`, see ) will be used.

```
# Finds a PDA owned by this program with the seeds ['seed', signer.key()].
pda1 = Pubkey.find_program_address(['seed', signer])


# Finds a PDA with the same seeds as above, but owned by `prog`.
pda2 = Pubkey.find_program_address(['seed', signer], prog.key())
```
