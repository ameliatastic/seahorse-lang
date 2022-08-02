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
