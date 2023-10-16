---
description: Dip your toes in the water by writing a simple program in Seahorse.
---

# Calculator: Your first Seahorse program

Full code [here](https://github.com/ameliatastic/seahorse-lang/blob/main/examples/calculator.py).

### Initializing the project <a href="#initializing-the-project" id="initializing-the-project"></a>

From your terminal:

```
seahorse init calculator
```

This should initialize a new project directory called **calculator**. Open it up and you should see something like this:

```
calculator
|\_ app
|\_ migrations
|\_ node_modules
|\_ programs
|\_ programs_py
    |\_ seahorse
    |\_ calculator.py
    :
|\_ target
|\_ tests
:
```

Your code will go under the **programs\_py** folder in **calculator.py**. The other directories are generated when an Anchor project is initialized. Your Seahorse/Python code lives inside the Anchor project (at **programs\_py**) and generates intermediate Rust artifacts in the **programs** directory, which Anchor then compiles to bytecode for Solana.

Anchor lets us easily use on-chain programs by providing auto-generated TypeScript bindings for your program's instructions and accounts. More on that later.

Now, `cd` into the **calculator** directory and try building the project. It might take a few minutes the first time, since none of the Rust code has been compiled yet - don't worry, subsequent compilations will take much less time.

```
cd calculator
seahorse build
```

Congrats, you've built your first Seahorse program! Unfortunately it doesn't do anything... yet.

You can inspect the generated code if you want to (at **programs/calculator/src/lib.rs**), but it's not that interesting. We'll fix that soon - but first, let's describe what we're trying to build.

### The Calculator <a href="#the-calculator" id="the-calculator"></a>

The calculator is a simple four-function calculator that operates on-chain. Users will be able to:

* Create a calculator associated with their wallet,
* Perform operations on the number stored in the calculator,
* And reset the calculator back to 0.

It's a very simple idea, but should get us used to the basics of Seahorse - creating on-chain accounts, manipulating their data with instructions, and performing simple safety checks.

### Adding accounts <a href="#adding-accounts" id="adding-accounts"></a>

We need a single account type to represent a user's calculator. In Seahorse, accounts are just classes that derive from the `Account` base class. Add the following to your **calculator.py**:

```
class Calculator(Account):
  owner: Pubkey
  display: i64
```

The fields of `Calculator` define the data that will be stored in it. Note the type annotations: these are optional in regular Python, but in Seahorse, they're mandatory for class fields. This lets us know that each `Calculator` account has an `owner` Pubkey, and a `display` i64 (signed integer).

### Adding instructions <a href="#adding-instructions" id="adding-instructions"></a>

Now that we have an account defined, we want to be able to create instances of it and interact with them. To do this, we need to write some _instructions_ for our program. In Seahorse, these are just normal Python functions with the `@instruction` decorator attached. Add the following function def to your **calculator.py**:

```
@instruction
def init_calculator(owner: Signer, calculator: Empty[Calculator]):
  # Initialize the calculator and set the owner
  # ...
```

This instruction just takes two accounts and does some stuff with them. We're introducing two new account types that are native to Seahorse - `Signer` and `Empty`.

`Signer` accounts are special accounts belonging to whoever signed the instruction - when a user makes a transaction that calls `init_calculator`, they'll sign the transaction with their Solana keypair and their account will show up as `owner` in the instruction.

The `Empty[Calculator]` indicates an account that is empty (i.e. has not yet been initialized) of type `Calculator`.

Now let's make our accounts do something - add the following to the `init_calculator` function:

```
  # ...
  # Initialize the calculator and set the owner
  calculator = calculator.init(
    payer = owner,
    seeds = ['Calculator', owner]
  )
  calculator.owner = owner.key()
```

Calling `.init()` on an `Empty` account will initialize it. Two arguments are required:

* `payer`, the account that will pay for the new account's rent
* `seeds`, the program-derived address (PDA) seeds.



{% hint style="info" %}
### PDAs

PDAs are outside the scope of this tutorial, but all you need to know for now is that whenever you create an account via your program, you need to give it a list of things that will uniquely identify it among all accounts that your program has made.

In Seahorse, these can be string literals or other accounts.
{% endhint %}

`.init()` passes back the new account, which we can now use as a `Calculator`. All we want to do now is set the `owner` field - to get the pubkey of the instruction signer, we call `.key()` on it.

And there you have it, the `init_calculator` instruction is done!

### Simple instructions with prints and asserts <a href="#simple-instructions-with-prints-and-asserts" id="simple-instructions-with-prints-and-asserts"></a>

Now that we can make calculators, let's give them some life. Add a new instruction:

```
@instruction
def reset_calculator(owner: Signer, calculator: Calculator):
  print(owner.key(), 'is resetting', calculator.key())


  # Verify owner
  assert owner.key() == calculator.owner, 'This is not your calculator!'


  calculator.display = 0
```

This instruction has two new things - a `print()` statement and an `assert` statement.

Assertions are the primary way to provide safety in Seahorse programs. If the assertion fails, then just like in regular Python, the instruction will exit early and log the message you give it.

Seahorse `print` statements are pretty much the same as Python `print` statements, and you can use them to print debug your programs. Under the hood, they get translated to an equivalant call to the Solana logger.

Here, we use an `assert` to make sure that the wallet that signed the transaction is the owner of the calculator. If we don't perform this check, then anybody could reset anybody else's calculator!

### Instructions with parameters and Enums <a href="#instructions-with-parameters-and-enums" id="instructions-with-parameters-and-enums"></a>

With this out of the way, we can finish implementing the calculator. We'll just add another instruction that modifies it based on two parameters that get passed in, an operation and a number.

The operation will be an enum (enumerated type), which is a type that can simply have one of any value. This allows you to keep magic numbers out of your code. Add the new class to your `calculator.py`:

```
class Operation(Enum):
  ADD = 0
  SUB = 1
  MUL = 2
  DIV = 3
```

For parsing purposes, each variant in the enum needs a unique number associated with it. These have no bearing on the generated code.

Instruction parameters are just like normal function parameters - data you pass to the function. In a normal Solana instruction call, _accounts_ and _parameters_ are treated separately. Seahorse simplifies this by letting them both live together in the instruction params.

Here's the code for the new instruction:

```
@instruction
def do_operation(owner: Signer, calculator: Calculator, op: Operation, num: i64):
  # Verify owner, like before
  assert owner.key() == calculator.owner, 'This is not your calculator!'


  if op == Operation.ADD:
    calculator.display += num
  elif op == Operation.SUB:
    calculator.display -= num
  elif op == Operation.MUL:
    calculator.display *= num
  elif op == Operation.DIV:
    calculator.display //= num
```

And that's it for our calculator program! Make sure it builds before we move on:

```
seahorse build
```

You can see the full code [here](https://github.com/ameliatastic/seahorse-lang/blob/main/examples/calculator.py).

### Testing with Anchor <a href="#testing-with-anchor" id="testing-with-anchor"></a>

Once your program builds, you can start using all the tools that Anchor provides. To Anchor, there's no difference between your Seahorse program and a hand-written Rust program. There's enough documentation on using Anchor elsewhere, but we'll still show you some basics on how to write tests and use your program.

Anchor should have generated a default test file in **tests/calculator.ts**. Open that and replace the code with this:

```
import * as anchor from '@project-serum/anchor'
import { BN, Program, web3 } from '@project-serum/anchor'
const assert = require('assert')


import { Calculator } from '../target/types/calculator'


describe('calculator', () => {
  // Run some tests on our calculator program
  // ...
})
```

Besides some simple things from Anchor/Node, we have one interesting import - `import { Calculator } from "../target/types/calculator"`. This is the power that Anchor gives us - when the Seahorse CLI builds your program using Anchor, it auto-generates TypeScript types that let you easily interact with your program through Anchor's TypeScript package.

Now, to start filling in the tests:

```
// Run some tests on our calculator program
const provider = anchor.AnchorProvider.env()
anchor.setProvider(provider)


const program = anchor.workspace.Calculator as Program<Calculator>


// Set up some common accounts we'll be using later
const owner = provider.wallet.publicKey
const calculator = web3.PublicKey.findProgramAddressSync(
  [Buffer.from('Calculator'), owner.toBuffer()],
  program.programId
)[0]


// Try initializing the calculator
// ...


// Do some operations on the calculator
// ...


// Make sure our calculator is secure
// ...
```

Some of this is Anchor boilerplate, and some is us setting up accounts for later. `owner` is the address of the Anchor provider's wallet, which signs every transaction that we send to our Solana localnet. `calculator` is the address of the Calculator that belongs to `owner`. Again, PDAs (Program-Derived Addresses) are outside the scope of this tutorial, but take note of how this address is generated - we pass in (essentially) the same seeds that we did during our call to `calculator.init()` during the `init_calculator` instruction a while ago.

`program` is an interface to the Calculator program, made from Anchor's auto-generated definitions.

```
// Try initializing the calculator
it('Inits a calculator', async () => {
  await program.methods.initCalculator().accounts({ owner, calculator }).rpc()
})
```

And here's everything working together to initialize the `calculator`. `program.methods` gives us access to all of our program's instructions. We build the `initCalculator` (`init_calculator` in our Seahorse code) call with the necessary accounts, then call `.rpc()` to sign + send the instruction as a transaction.

This will initialize the calculator!

```
// Do some operations on the calculator
it('Does some operations', async () => {
  const add2 = await program.methods
    .doOperation({ add: true }, new BN(2))
    .accounts({ owner, calculator })
    .instruction()


  const mul3 = await program.methods
    .doOperation({ mul: true }, new BN(3))
    .accounts({ owner, calculator })
    .instruction()


  const sub1 = await program.methods
    .doOperation({ sub: true }, new BN(1))
    .accounts({ owner, calculator })
    .instruction()


  const tx = new web3.Transaction()
  tx.add(add2, mul3, sub1)
  await provider.sendAndConfirm(tx)


  // Get the calculator's on-chain data
  const calculatorAccount = await program.account.calculator.fetch(calculator)


  assert.ok(calculatorAccount.display.toNumber() === 5)
})
```

Now let's do something with it - we create 3 instructions (+ 2, \* 3, and - 1) to run some operations on our calculator, then package them as a single transaction and send it. Afterward, we can use Anchor to grab the calculator's data from the chain and see what its `display` says. This should give us the correct result from our 3 operations.

Also note that we're passing new `BN` objects to our instruction, not regular JavaScript numbers. The numeric type we used, `i64`, covers a larger range of integers than JavaScript numbers can, so the conversion is needed for safety. When we grab the on-chain data, we convert back to a number as well.

```
// Make sure our calculator is secure
it('Prevents fraudulent transactions', async () => {
  let hackerman = new web3.Keypair()


  let shouldFail = await program.methods
    .resetCalculator()
    .accounts({
      owner: hackerman.publicKey,
      calculator,
    })
    .instruction()


  let tx = new web3.Transaction()
  tx.add(shouldFail)
  await provider
    .sendAndConfirm(tx, [hackerman])
    .then(() => assert.ok(false)) // Error on success, we want a failure
    .catch(console.log)
})
```

Finally, let's make sure our security guarantees are as airtight as we thought. We create a new keypair called `hackerman` to sign a fraudulent transaction. We'll try to get this new wallet, who does not own our `calculator`, to sign a transaction resetting our display and erasing all of our hard work.

This transaction will fail due to our assertions in `reset_calculator` - `hackerman` does not have the same key as the calculator's owner, so an error is thrown and the transaction fails. Safe!

An error message something like this should be printed to the console - note that it contains the log from our `print()` earlier:

```
SendTransactionError: failed to send transaction: Transaction simulation failed: Error processing Instruction 0: custom program error: 0x1770
    at ... {
  logs: [
    'Program Fg6PaFpoGXkYsidMpWTK6W2BeZ7FEfcYkg476zPFsLnS invoke [1]',
    'Program log: Instruction: ResetCalculator',
    'Program log: BxooiAL7QrzeWYjiQ2veesVcimnCQaMBKmh3mMoVzLHn "is resetting" BwjpFHQjRYUG6Kp5dZpkqTYRdhUFRH6fv36MAQ9z1hAe',
    'Program log: AnchorError thrown in programs/calculator/src/lib.rs:69. Error Code: E000. Error Number: 6000. Error Message: This is not your calculator!.',
    'Program Fg6PaFpoGXkYsidMpWTK6W2BeZ7FEfcYkg476zPFsLnS consumed 36107 of 200000 compute units',
    'Program Fg6PaFpoGXkYsidMpWTK6W2BeZ7FEfcYkg476zPFsLnS failed: custom program error: 0x1770'
  ]
}
```
