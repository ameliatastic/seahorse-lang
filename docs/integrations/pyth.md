# Pyth

Seahorse provides built-in integration with the [Pyth](https://pyth.network/), one of Solana's most popular on-chain oracles!

### Brief overview <a href="#brief-overview" id="brief-overview"></a>

**Pyth** relies on off-chain oracles sending information to on-chain accounts, known as price feeds. The information in these feeds is continuously aggregated from several high-quality sources, giving you real-time market data.

Data isn't limited to just crypto pairs - there's also price feeds for [various US equities, forex pairs, and even some precious metals](https://pyth.network/price-feeds?cluster=mainnet-beta)! All of these can be used natively in Seahorse.

Since this data is just stored in on-chain accounts, it's extremely important to validate the account's key to make sure that you have the right account. Seahorse prevents you from loading the price feed data at all until you've validated the key:

```
from seahorse.pyth import *


@instruction
def use_sol_usd_price(price_account: PriceAccount):
  price_feed = price_account.validate_price_feed('SOL/USD')
```

The `validate_price_feed` method returns a price feed from a Pyth account. Seahorse knows all of Pyth's price feed keys, and will generate a check for you (in this case, `H6ARHf6YXhGYeQfUzQNGk6rDNnLBQKrenN712K4AQJEG`, since that's the key of the [mainnet SOL/USD price feed](https://pyth.network/price-feeds/crypto-sol-usd?cluster=mainnet-beta)). If you pass the wrong account into the instruction, an error will be thrown.

The sole parameter must be a _string literal_ - the price feed you want must be known at compile time (otherwise, Seahorse would have to dump hundreds of pubkeys into your program data just to perform the check!). The format of the string is `[cluster-]BASE/QUOTE`. The cluster is optional, and defaults to mainnet. For example, mainnet SOL/USD is just `SOL/USD` or `mainnet-SOL/USD`. Devnet USD/JPY is `devnet-USD/JPY`.

Once you have a validated `PriceFeed` object, you can fetch the `Price` from there:

```
  # This will panic if the price is currently not available in Pyth
  price = price_feed.get_price()


  # Retrieves the price as a floating-point number
  x: f64 = price.num()
  # Get the raw fields of the price data - price, conf, and expo.
  # price = (p ± c) * 10^e
  p: i64 = price.price
  c: u64 = price.conf
  e: i32 = price.expo
```

Read more about the fields of a `Price` [here](https://docs.rs/pyth-sdk-solana/0.6.1/pyth\_sdk\_solana/struct.Price.html).

### Addendum: Testing with Anchor <a href="#addendum-testing-with-anchor" id="addendum-testing-with-anchor"></a>

When you run tests using a local network, the Pyth price accounts won't exist - you need to bring them in manually. You can get specific keys on [this page](https://pyth.network/developers/price-feed-ids#solana-mainnet-beta), then:

1. In your **Anchor.toml**, add a `[test.validator]` table:

```
[test.validator]
url = "https://api.mainnet-beta.solana.com"
```

1. Add a `[test.validator.clone]` table for each account you want added:

```
[[test.validator.clone]]
address = "H6ARHf6YXhGYeQfUzQNGk6rDNnLBQKrenN712K4AQJEG"
```

Anchor will then clone the mainnet account `H6ARHf6YXhGYeQfUzQNGk6rDNnLBQKrenN712K4AQJEG` when your validator starts up, allowing you to test your code as if you were testing on mainnet.

[Reference.](https://www.anchor-lang.com/docs/manifest#test-validator)

