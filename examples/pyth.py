from seahorse.prelude import *
from seahorse.pyth import *

declare_id('EkY7qZD2RCr1LpUzADJkzbjGaWfbvGYB9eJe7DYCgGF8')


@instruction
def use_sol_usd_price(price_account: PriceAccount):
    price_feed = price_account.validate_price_feed('devnet-SOL/USD')
    price = price_feed.get_price()
    price = price.num()
    print(price)
