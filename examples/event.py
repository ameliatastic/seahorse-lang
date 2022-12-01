# event
# Built with Seahorse v0.1.0
#
# Emit on-chain events

from seahorse.prelude import *

declare_id('Fg6PaFpoGXkYsidMpWTK6W2BeZ7FEfcYkg476zPFsLnS')


class HelloEvent(Event):
    data: u8
    title: str
    owner: Pubkey
    # Not yet supported: https://github.com/ameliatastic/seahorse-lang/issues/64
    # items: List[u8]
    # pair: Array[u8, 2]

    def __init__(
        self,
        data: u8,
        title: str,
        owner: Pubkey,
        # items: List[u8],
        # pair: Array[u8, 2],

    ):
        self.data = data
        self.title = title
        self.owner = owner
        # self.items = items
        # self.pair = pair


@instruction
def send_event(
    sender: Signer,
    data: u8,
    title: str
):
    event = HelloEvent(data, title, sender.key())
    event.emit()
