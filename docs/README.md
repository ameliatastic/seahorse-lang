---
description: Write Anchor-compatible Solana programs in Python
layout:
  title:
    visible: true
  description:
    visible: true
  tableOfContents:
    visible: true
  outline:
    visible: false
  pagination:
    visible: true
---

# Seahorse  (beta)

### The ease of Python with the safety of Rust. <a href="#the-ease-of-python-with-the-safety-of-rust" id="the-ease-of-python-with-the-safety-of-rust"></a>

Seahorse lets you write Solana programs in Python. It is a community-led project built on [Anchor](https://github.com/coral-xyz/anchor).

Developers gain Python's ease-of-use, while still having the same safety guarantees of every Rust program on the Solana chain. Low-level memory problems are handled by default, letting you worry about the important stuff.



{% code title="game.py" fullWidth="false" %}
```python
@instruction
def init_player(owner: Signer, player: Empty[Player]):
  player = player.init(
    payer = owner,
    seeds = ['player-account', owner]
  )

  player.owner = owner.key()
  player.health = 100
  player.gold = 0
```
{% endcode %}

#### Features <a href="#features" id="features"></a>

* **Compile-time type safety**
* **Fully interoperable with Rust code**
* **Compatibility with Anchor**
* **Built-in integration with Pyth**

The Seahorse compiler generates intermediate Rust artifacts and uses Anchor to do some of the heavy lifting.

_Seahorse is beta software. Many features are unimplemented and it's not production-ready._



#### Community <a href="#community" id="community"></a>

Seahorse is more than just a compiler, it's also a community of people who want to make development on Solana better. We're on [Twitter](https://twitter.com/seahorse\_lang). **You can also check out a bunch of other community-made resources** [**here**](https://www.seahorse.dev/introduction/seahorse-community)**!**
