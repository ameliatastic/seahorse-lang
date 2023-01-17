from seahorse.prelude import *

declare_id('4SEMJzX6o2YQNws7yrsfUdjJCR4B5Z3GyR2Pj7UgzDy2')

# This test case checks that we can use account attributes/functions in both seed and non-seed contexts
# See https://github.com/ameliatastic/seahorse-lang/issues/62 for the original issue


class User(Account):
    data: u8


class Another(Account):
    data: u8


@instruction
def ix(payer: Signer, user: User, another: Empty[Another]):
    # use key() as a seed
    a = another.init(payer, seeds=[user.key()])
    # check key() works correctly when not in a seed
    print(user.key())
    # check we can read arbitrary class fields of accounts
    print(user.data)
