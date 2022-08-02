from seahorse.prelude import *

class Protocol(Account):
    owner: Pubkey

@instruction
def initialize(owner: Signer, init_protocol: Initializer[Protocol]):
    protocol = init_protocol(owner, ['protocol'])
    protocol.owner = owner.key()