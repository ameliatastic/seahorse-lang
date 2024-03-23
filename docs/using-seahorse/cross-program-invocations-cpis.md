# Cross Program Invocations (CPIs)

Seahorse's `Program` type is a special account that allows you to make any CPI call you want. However, you have to go through Solana's low-level interface to get there, which might be a bit tedious. Make sure you know what you're doing before you use this!

### CpiAccount <a href="#cpi-account" id="cpi-account"></a>

Each account that gets passed into a Solana CPI needs to have some metadata associated with it - Solana needs to know if the account is an instruction signer, and if the account is mutable. If the account is an instruction signer, then Solana might also need this account's signer seeds (if the account is a PDA).

All of this information is provided to the Seahorse CPI interface via the `CpiAccount` class. The constructor has the following signature:

```
CpiAccount(
  account: Account,
  mut:     bool = False,
  signer:  bool = False,
  seeds:   List[Seed] = None
)
```

`mut` and `signer` are false by default, so if you just want an immutable, non-signer account, you can just pass that. Otherwise, you need to use those parameters:

```
cpi_account1 = CpiAccount(account1)
cpi_account2 = CpiAccount(account2, mut=True)
cpi_account3 = CpiAccount(signer, signer=True)
```

If your account is a PDA, you need to give its signer seeds as well. This looks exactly like making a list of signer seeds for an account init:

```
cpi_account4 = CpiAccount(program_account, seeds=['program', signer])
```

The `signer` and `seeds` parameters are mutually exclusive - you can either provide neither or one of them.

### Instruction data <a href="#instruction-data" id="instruction-data"></a>

Instruction data is passed as a list of bytes (`u8`). If you need to serialize a number, the Seahorse prelude provides the `int_bytes` function to make little-endian (default) or big-endian byte lists out of any of the Rust integer types.

```
le: List[u8] = int_bytes(u64(100))
be: List[u8] = int_bytes(u64(100), be=True)
```

From here, you're on your own. Eventually I'll expand Seahorse's byte-manipulation capabilities, but for now you'll likely have to build list of bytes manually.

```
data1 = [0x01, 0x00] + int_bytes(x) + int_bytes(y)
data2 = [0x02, 0x00] + int_bytes(x)
```

### Making the CPI call <a href="#making-the-cpi-call" id="making-the-cpi-call"></a>

Now, with a `Program` account, you can make your CPI call! Under the hood, this will take your list of `CpiAccounts` and extract the necessary data out of them to pass along to Solana's `invoke` function, including things like signer seeds and the program's address itself.

```
@instruction
def make_cpi(
  program: Program,
  signer: Signer,
  account1: UncheckedAccount,
  account2: Account,
  n: u64
):
  # You're responsible for checking the program's key, along with doing any
  # other necessary validation
  assert program.key() == ...


  # Invoke an instruction to the given program
  program.invoke(
    accounts = [
      CpiAccount(signer, signer=True),
      CpiAccount(account1),
      CpiAccount(account2, mut=True, seeds=['MyAccount', signer])
    ],
    data = [0x01] + int_bytes(n)
  )
```

Hopefully that wasn't too bad. The API is intentionally a little bit obscure, partially to discourage use of it - eventually, Seahorse will get better support for CPIs (enabled by Anchor IDLs), and program authors should not _have_ to rely on this forever. It's there as an escape hatch until better support is released.

Since "raw" CPIs don't count as a special CPI call that needs to have its program inferred, you can extract the logic to a function and make the call from there. This also gives program authors the ability to create their own API for making CPIs to their programs.
