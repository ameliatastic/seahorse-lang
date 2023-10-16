# Instructions

### The @instruction decorator <a href="#the-instruction-decorator" id="the-instruction-decorator"></a>

Converting a function to an instruction is as simple as adding the `@instruction` decorator.

```
@instruction
def simple_instruction(signer: Signer):
  # ...


@instruction
def more_complex_instruction(signer: Signer, data: u64):
  # ...
```

The parameters of an instruction can include both accounts and regular parameters. On the Rust/Anchor side, accounts are separated and put into an accounts context struct.

### Debug logging with print <a href="#debug-logging-with-print" id="debug-logging-with-print"></a>

You can write to Solana's program log with the builtin `print` function:

```
@instruction
def use_print(signer: Signer, account: TokenAccount):
  # Keys are printed in their base58 representation.
  print(signer.key(), account.amount())


  # This works pretty well with f-strings too!
  print(f'Token account {account.key()} has {account.amount()} tokens.')
```

### Structured logging with Events <a href="#structured-logging-with-events" id="structured-logging-with-events"></a>

Seahorse also supports [Anchor events](https://solana.stackexchange.com/a/2116), a structured method of logging subscribable information (via Anchor's TypeScript SDK). To use an event, create a class with `Event` as its parent class:

```
def MyEvent(Event):
  data: u64


  def __init__(self, data: u64):
    self.data = u64
```

When a class inherits from `Event`, it gains the `.emit()` function, which is used just like Anchor's `emit!` to log the event:

```
event = MyEvent(100)
# Emits the `MyEvent` to program log
event.emit()
```

### Inferred program accounts <a href="#inferred-program-accounts" id="inferred-program-accounts"></a>

In Solana, programs are a special type of accounts. When writing a Solana program, you need to pass in _every_ account that gets used - programs included. Seahorse simplifies this by inferring the necessity of certain program accounts, so that you don't have to include them in your instruction params.

The following table summarizes which programs Seahorse infers

| Program               | Needed when...          |
| --------------------- | ----------------------- |
| **System program**    | Initializing accounts   |
| **SPL Token program** | Transferring SPL tokens |

Instructions are special - they are the only place where programs accounts can be inferred. This also means that you can't make certain calls from _outside_ of an instruction (for example, `Empty.init(...)`), otherwise the compiler won't be able to attach the inferred System program account to anything. For now this just becomes an error, a future update may make this more flexible.

