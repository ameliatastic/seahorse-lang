# seahorse: Write Solana programs in Python

## The ease of Python with the safety of Rust.

Seahorse lets you write Solana programs in Python. It is a community-led project built on [Anchor](https://github.com/coral-xyz/anchor).

Developers gain Python's ease-of-use, while still having the same safety guarantees of every Rust program on the Solana chain. Low-level memory problems are handled by default, letting you worry about the important stuff.

### Features

- **Compile-time type safety**
- **Fully interoperable with Rust code**
- **Compatibility with Anchor**

The Seahorse compiler generates intermediate Rust artifacts and uses Anchor to do some of the heavy lifting.

_Seahorse is beta software. Many features are unimplemented and it's not production-ready._

[**Get started**](https://seahorse-lang.org/)

[**Installation**](https://seahorse-lang.org/docs/installation)

[**Examples**](/examples/)

## Example: FizzBuzz

Here's a very simple program that does something similar to the classic [FizzBuzz](https://en.wikipedia.org/wiki/Fizz_buzz#Programming) problem.

```py
# fizzbuzz
# Built with Seahorse v0.1.0
#
# On-chain, persistent FizzBuzz!

from seahorse.prelude import *

declare_id('Fg6PaFpoGXkYsidMpWTK6W2BeZ7FEfcYkg476zPFsLnS')

class FizzBuzz(Account):
  fizz: bool
  buzz: bool
  n: u64

@instruction
def init(owner: Signer, fizzbuzz: Empty[FizzBuzz]):
  fizzbuzz.init(payer = owner, seeds = ['fizzbuzz', owner])

@instruction
def do_fizzbuzz(fizzbuzz: FizzBuzz, n: u64):
  fizzbuzz.fizz = n % 3 == 0
  fizzbuzz.buzz = n % 5 == 0
  if not fizzbuzz.fizz and not fizzbuzz.buzz:
    fizzbuzz.n = n
  else:
    fizzbuzz.n = 0
```

This shows some basic Seahorse functionality, like account initialization and creating instructions. For more, check out [Calculator: Your first Seahorse program](https://seahorse-lang.org/docs/your-first-seahorse-program) or other examples [here](/examples/).

The compiler architecture changed entirely in v0.2.0, here's a brief overview - more details [here](/src/core/README.md):

```
SEAHORSE CORE: THE COMPILER (v0.2.0)
┌───────────────────────────────────────────┐
│                                           │
│ ┌───────────────────────────────────────┐ │
│ │ PARSE                                 │ │
│ │                                       │ │
│ │ Turn Seahorse source code into Python │ │
│ │ AST. Handled by rustpython.           │ │
│ └───────────────────┬───────────────────┘ │
│                     │                     │
│                    AST                    │
│                     │                     │
│ ┌───────────────────▼───────────────────┐ │
│ │ CLEAN                                 │ │
│ │                                       │ │
│ │ Remove unsupported parts from the AST │ │
│ │ (like yields statements). Does some   │ │
│ │ minor changes to make compilation     │ │
│ │ easier.                               │ │
│ └───────────────────┬───────────────────┘ │
│                     │                     │
│                    AST                    │
│                     │                     │
│ ┌───────────────────▼───────────────────┐ │
│ │ PREPROCESS                            │ │
│ │                                       │ │
│ │ Find the source files for every       │ │
│ │ import - recursively calls the first  │ │
│ │ two steps as well.                    │ │
│ │                                       │ │
│ │ Outputs a "module registry" which has │ │
│ │ every parsed+cleaned source file.     │ │
│ └───────────────────┬───────────────────┘ │
│                     │                     │
│                  registry                 │
│                     │                     │
│ ┌───────────────────▼───────────────────┐ │
│ │ COMPILE                               │ │
│ │ ┌───────────────────────────────────┐ │ │
│ │ │ NAMESPACE                         │ │ │
│ │ │                                   │ │ │
│ │ │ Resolve the location of every     │ │ │
│ │ │ import/export in each module.     │ │ │
│ │ └─────────────────┬─────────────────┘ │ │
│ │                   │                   │ │
│ │         registry & namespaces         │ │
│ │                   │                   │ │
│ │ ┌─────────────────▼─────────────────┐ │ │
│ │ │ SIGN                              │ │ │
│ │ │                                   │ │ │
│ │ │ Find types of everything outside  │ │ │
│ │ │ function bodies - class fields,   │ │ │
│ │ │ function params/return type.      │ │ │
│ │ └─────────────────┬─────────────────┘ │ │
│ │                   │                   │ │
│ │         registry & signatures         │ │
│ │                   │                   │ │
│ │ ┌─────────────────▼─────────────────┐ │ │
│ │ │ CHECK                             │ │ │
│ │ │                                   │ │ │
│ │ │ Type check function bodies. Also  │ │ │
│ │ │ outputs the type of each expres-  │ │ │
│ │ │ sion, used for doing syntactic    │ │ │
│ │ │ transformations later.            │ │ │
│ │ └─────────────────┬─────────────────┘ │ │
│ │                   │                   │ │
│ │         registry & expr. types        │ │
│ │                   │                   │ │
│ │ ┌─────────────────▼─────────────────┐ │ │
│ │ │ BUILD                             │ │ │
│ │ │                                   │ │ │
│ │ │ Turn the original Python AST into │ │ │
│ │ │ a Rust-like AST, assisted by the  │ │ │
│ │ │ type information from CHECK.      │ │ │
│ │ │                                   │ │ │
│ │ │ The new AST includes special      │ │ │
│ │ │ constructs for things native to   │ │ │
│ │ │ Anchor, like ix contexts.         │ │ │
│ │ └───────────────────────────────────┘ │ │
│ │                                       │ │
│ └───────────────────┬───────────────────┘ │
│                     │                     │
│                    AST                    │
│                     │                     │
│ ┌───────────────────▼───────────────────┐ │
│ │ GENERATE                              │ │
│ │                                       │ │
│ │ Finally, turn the Rust-like AST into  │ │
│ │ Rust source code. Generates code for  │ │
│ │ each source file individually, as     │ │
│ │ well as a lib.rs that contains the    │ │
│ │ "real" instruction entrypoints.       │ │
│ └───────────────────────────────────────┘ │
│                                           │
└───────────────────────────────────────────┘
```
