# constants
# Built with Seahorse v0.1.0
#
# Demonstrate simple use of constants

from seahorse.prelude import *

declare_id('Fg6PaFpoGXkYsidMpWTK6W2BeZ7FEfcYkg476zPFsLnS')


MIN = 2
MAX = 7
RANGE = MAX - MIN

MESSAGE = 'Hello constants'


@instruction
def use_constants(signer: Signer):
    print(MESSAGE)
    
    for i in range(MIN, MAX):
        print('Step:', i)
    
    print('Range:', RANGE)