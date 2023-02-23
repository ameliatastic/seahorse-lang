from seahorse.prelude import *
from util.more_data import MoreData


declare_id('Fg6PaFpoGXkYsidMpWTK6W2BeZ7FEfcYkg476zPFsLnS')


class MyEvent(Event):
    nums: List[i32]


class Deep:
    num: i32

    def __init__(self, num: i32):
        self.num = num


class Nested:
    deep: Deep

    def __init__(self, num: i32):
        self.deep = Deep(num)
    
    def reset(self):
        self.deep = Deep(0)


class Flag(Enum):
    OFF = 1
    ON = 2


class Data(Account):
    array_2d: Array[Array[i32, 2], 2]
    int_list: List[i32]
    int_list_2d: List[List[i32]]
    string: str
    nested: Nested
    nested_list: List[Nested]
    flag: Flag
    more_data: MoreData


@instruction
def init(signer: Signer, data: Empty[Data]):
    init_data = data.init(
        payer=signer,
        seeds=[signer],
        padding=1024
    )

    init_data.int_list = [1, 2]
    init_data.int_list_2d = [[3, 4], [5, 6]]
    init_data.string = 'Hello'
    init_data.nested = Nested(7)
    init_data.nested_list = [Nested(8), Nested(9)]
    init_data.more_data = MoreData(10)


@instruction
def test_stored_mutables(signer: Signer, data: Data):
    # Modify everything in the Data account 

    # [[0, 0], [0, 0]] -> [[1, 0], [0, 0]]
    data.array_2d[0][0] = 1
    # [1, 2] -> [1, 2, 0]
    data.int_list.append(0)
    # [[3, 4], [5, 6]] -> [[3, 0], [5, 6]]
    data.int_list_2d[0][-1] = 0
    # "Hello" -> "Hello World"
    data.string = data.string + ' World'
    # N(D(7)) -> N(D(0))
    data.nested.reset()
    # [N(D(8)), N(D(9))] -> [N(D(0)), N(D(9)), N(D(10))]
    data.nested_list[0].reset()
    data.nested_list.append(Nested(10))
    # OFF -> ON
    data.flag = Flag.ON
    # MD(10) -> MD(11)
    data.more_data = MoreData(11)
