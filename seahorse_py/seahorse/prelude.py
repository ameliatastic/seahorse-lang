# seahorse.prelude: the basis for writing Seahorse programs.
#
# NOTE: this file just contains types and documentation for your editor. This
# is NOT executable code, and you won't be able to change the behavior of your
# Seahorse programs by editing this file.

from typing import *

T = TypeVar('T')

class u64:
    """64-bit unsigned integer."""

    def __add__(self, _: Any):
        # Necessary for type inference
        return self

    def __sub__(self, _: Any):
        return self

class i64:
    """64-bit signed integer."""

    # ... same stuff as u64

class Pubkey:
    """32-byte account identifier."""
    
class ProgramResult:
    """Ok(())"""

class Account:
    """Generic Solana account."""
    def key(self) -> Pubkey:
        """Get this account's key."""

class Signer(Account):
    """Instruction signer."""

class Initializer(Generic[T]):
    """A function that initializes an account."""

    def __call__(self, payer: Account, init: List[Union[Account, str, u64]]) -> T:
        """Initialize the account."""

def instruction(function: Callable[..., None]) -> Callable[..., ProgramResult]:
    """Turn a function into a program instruction."""
