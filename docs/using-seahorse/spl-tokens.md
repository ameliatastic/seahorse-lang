# SPL tokens

### The SPL token program <a href="#the-spl-token-program" id="the-spl-token-program"></a>

Solana's standard way of creating new token types is by creating a new Mint with the SPL token program. Seahorse provides convenient built-in support for the SPL token program.

You can see an example of creating and using a token mint [here](../../examples/hello.py).

#### TokenAccount <a href="#token-account" id="token-account"></a>

Account that holds SPL tokens. Accounts of this type are owned by the SPL token program.

You can initialize these accounts with an `Empty[TokenAccount]`, the `.init()` method will require some extra arguments.

Usage:

```
@instruction
def init_token_account(
  new_token_account: Empty[TokenAccount],
  mint: TokenMint,
  signer: Signer
):
  # On top of the regular init args, you need to provide:
  #   - the mint that this token account will hold tokens of
  #   - the account that has authority over this account.
  new_token_account.init(
    payer = signer,
    seeds = ['token-account', signer],
    mint = mint,
    authority = signer
  )
  
@instruction
def use_token_account(
  signer_account: TokenAccount,
  recipient: TokenAccount,
  signer: Signer
):
  # Transfers 100 tokens from `signer_account` to `recipient`.
  # `signer` must be the authority (owner) for `signer_token_account`.
  # Note that the amounts here are in *native* token quantities - you need to
  # account for decimals when you make calls to .transfer().
  signer_account.transfer(
    authority = signer,
    to = recipient,
    amount = 100
  )
```

You can also create associated token accounts, which still have the `TokenAccount` type:

```
@instruction
def init_token_account(
  new_token_account: Empty[TokenAccount],
  mint: TokenMint,
  signer: Signer
):
  # Just like a PDA token account creation, with associated set to True. Seeds
  # may not be provided.
  new_token_account.init(
    payer = signer,
    mint = mint,
    authority = signer,
    associated = True
  )
```

#### TokenMint <a href="#token-mint" id="token-mint"></a>

Account that can create SPL tokens. Accounts of this type are owned by the SPL token program.

You can initialize these accounts with an `Empty[TokenMint]`, the `.init()` method will take a special form.

Usage:

```
@instruction
def init_token_mint(
  new_token_mint: Empty[TokenMint],
  signer: Signer
):
  # On top of the regular init args, you need to provide:
  #   - the number of decimals that this token will have
  #   - the account that has authority over this account.
  new_token_mint.init(
    payer = signer,
    seeds = ['token-mint', signer],
    decimals = 6,
    authority = signer
  )
  
@instruction
def use_token_mint(
  mint: TokenMint,
  recipient: TokenAccount,
  signer: Signer,
  recipient_signer: Signer
):
  # Mint 100 tokens from our `mint` to `recipient`.
  # `signer` must be the authority (owner) for `mint`.
  # Note that the amounts here are in *native* token quantities - you need to
  # account for decimals when you make calls to .mint().
  mint.mint(
    authority = signer,
    to = recipient,
    amount = 100
  )
  
  # Burn 99 tokens from the `recipient` account (so after this instruction,
  # `recipient` will gain exactly 1 token.)
  # `recipient_signer` must be the authority for the `recipient` token account.
  mint.burn(
    authority = recipient_signer,
    holder = recipient,
    amount = 99
  )
```

