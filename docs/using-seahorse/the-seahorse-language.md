---
description: >-
  Seahorse is based on Python 3, but only supports a subset of the full Python
  language. It has some additional constraints on what is and isn't allowed.
---

# The Seahorse Language

### Limitations: a brief overview <a href="#limitations-a-brief-overview" id="limitations-a-brief-overview"></a>

Seahorse tries as hard as possible to do everything that Python does. However, some things that Python can do aren't very feasibly translated to Rust. For example, Rust structs know all of their fields at compile-time, but Python objects can gain additional fields at runtime - it might be possible to support Python here, but it would come at a big runtime cost.

The most important distinction is static vs. dynamic typing, discussed [here](the-seahorse-language.md#type-hints-and-static-typing).

One feature of Python's that is (nearly) fully supported by Seahorse is the way that values are passed by alias. This basically means that if you have some value in a variable, re-assigning this value to another variable will simply make a secondary alias for the same data:

```
a = [1, 2, 3]


# Makes an alias to the list in a, now any edits to b will be reflected in a
b = a


# Passes an alias to b which is just an alias to the list in a. If f makes
# any edits to its parameter, then they will be reflected in a and b
f(b)
```

Note to program authors - this behavior is achieved by using Rust's _interior mutability_ pattern, since Rust's mutability rules are much stricter than Python's. This means that there is a small extra runtime cost associated with operations on mutable values. "Mutable values" mean any data types that are mutable by Python/Seahorse - this includes most collections (lists, arrays) and custom accounts types. Strings and integers are both immutable, so you won't see any extra cost here.

There is a lot of room for optimization on the compiler side here, and hopefully Seahorse will be able to take advantage of it in the future. Until then, just know that _Seahorse programs will always output slightly less efficient code, in order to adhere to Python's programming model._

### The Seahorse prelude <a href="#the-seahorse-prelude" id="the-seahorse-prelude"></a>

When you first create a Seahorse project, a Python file called **prelude.py** is imported via `from seahorse.prelude import *`. This file contains class/function definitions for everything built in to Seahorse, and is used to provide editors with autocompletion and serve as documentation for the things you can do with Seahorse. The following table briefly summarizes the classes/functions that are made available - check **prelude.py** for more details:

| Type                                               | Description                                                                                                                                                                                                                                                                                                        |
| -------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| `u8, u16, ... u128`, `i8, i16, ... i128`, `f64`    | Simple numeric types that map to Rust builtin types of the same names. Includes some functions to convert between types, since Seahorse cannot always do this automatically (but it tries - see [Numbers and math](the-seahorse-language.md#numbers-and-math))                                                     |
| `Array[T, N]`                                      | Fixed-length array, like a Python `list` but with a size. `N` must be an integer literal. Can be created as through the class constructor (`Array(Iter[T], u64)` where the second argument is the length) or the function constructor (`array(...T)`) Arrays can be used in any function that accepts an iterable. |
| `Pubkey`                                           | A 32-byte public key.                                                                                                                                                                                                                                                                                              |
| `Account`, `Signer`, `Empty`, `TokenAccount`, etc. | Types for supported Solana accounts. Discussed in detail [here](accounts.md).                                                                                                                                                                                                                                      |

### Python constructs and builtins <a href="#python-constructs-and-builtins" id="python-constructs-and-builtins"></a>

Only a subset of Python's language constructs and builtins are supported. Seahorse is actively trying to support as much of Python as possible.

Builtin functions all should behave how you would expect them to in Python, sometimes with some minor adjustments for typing purposes.

#### Classes and OOP <a href="#classes-and-oop" id="classes-and-oop"></a>

Classes allow you to organize your code in a more intuitive way. You've already seen classes being used to store on-chain data (by extending `Account`), but they can do much more than this!

Here's what defining a simple class with some data and methods looks like:

```
# Creates a simple class that stores some data (2 floating-point numbers).
class Point:
  x: f64
  y: f64


  # Defines the constructor for `Point`. Note the syntax - just like in
  # Python, the constructor takes self and modifies it, returning nothing.
  def __init__(self, x: f64, y: f64):
    self.x = x
    self.y = y


  # This is an instance method, since it uses self as a parameter.
  def magnitude(self) -> f64:
    return (self.x**2 + self.y**2) ** 0.5


  # This method is static, since it has no self parameter.
  def origin() -> Point:
    return Point(0, 0)
```

You can then use the class just like you would in Python:

```
p = Point(3, 4)
print(p.magnitude()) # 5.0


q = Point.origin()
print(q.magnitude()) # 0.0
```

One limitation of used-defined classes is that _they may not be stored in accounts._ [This is a known issue](https://github.com/ameliatastic/seahorse-lang/issues/33), and it's being worked on.

**Automatic constructors with `@dataclass`**

Constructors can be automatically generated for classes with the [`@dataclass` decorator](https://docs.python.org/3/library/dataclasses.html):

```
@dataclass
class Datapoint:
  x: f64
  y: f64
```

A constructor will be generated for this class that takes a parameter for each field, in the order defined in the class. So the above `Datapoint` constructor can be called like this:

```
# Creates a Datapoint with x=2, y=3.
p = Datapoint(2, 3)
```

#### Imports <a href="#imports" id="imports"></a>

You can stretch your codebase among multiple files and then use those files with imports. Like in Python, an import can refer to a precise object (class/function) in a module, a module itself, or a package which may contain modules and subpackages.

Seahorse (.py) files make up modules, and directories make up packages. You might have a codebase structured like this:

```
programs_py
|\_ seahorse (contains the Seahorse editor hints files)
|\_ program.py
|\_ util
    |\_ algorithms
    |   |\_ merge_sort.py
    |\_ data_structures
    |   |\_ merkle_tree.py
```

Then **program.py** can use **util** like this:

```
# Imports util as a package, giving access to algorithms and data_structures
import util
# Imports merge_sort as a module
from util.algorithms import merge_sort
# Imports everything in merkle_tree
from util.algorithms.merkle_tree import *


# ...


merge_sort.sort(my_list)


tree = MerkleTree(my_data)
```

Note the lack of an **\_\_init\_\_.py** file - some Python features require this, but Seahorse doesn't - directories are treated as packages and .py files are treated as modules. This also means that if you have a package with some extra random Python code nested deeply inside it, Seahorse will attempt to read and parse it, so make sure that you know what you're doing when you import a package!

#### Other constructs <a href="#other-constructs" id="other-constructs"></a>

These are some of the weirder/more Pythonic language constructs that you can use in Seahorse:

* **List comprehensions**\
  Seahorse fully supports list comprehensions! Just like in Python, you can do things like `[i**2 for i in range(10)]`. Other types of comprehension (generator, set, dict) are not supported yet, but will be in the near future.
* **F-strings**\
  Formatted strings work mostly like in Python, but the exact string you get might be unexpected and _is subject to change_. Namely, if you pass in a custom class as a parameter, Seahorse will translate this to use the class's derived Debug method under the hood, which might give you weird results. _For now, you should only really count on using f-strings for ad-hoc debugging and logging information._ The API will stabilize eventually.
* **Tuple assignment**\
  Seahorse supports tuple assignment exactly like Python does - you can iterate over lists of tuples with `for (x, y) in ...`, and you can unpack tuples with `x, y = ...`. You can even do the Pythonic one-line swap: `x, y = y, x`.
* **Functional programming and functions as first-class objects**\
  _Partially supported_. New in v2, you can do things that rely on functional programming - namely `map` and `filter` (see [Builtins for working with iterators](the-seahorse-language.md#builtins-for-working-with-iterators)). Functions are not first-class objects in Seahorse, though, so you may not assign a function to a variable and pass it around that way.

#### General builtins <a href="#general-builtins" id="general-builtins"></a>

* `print(...T) -> None`\
  Print a message. Under the hood, Seahorse uses the built-in `msg!` macro to log messages to Solana.
* `str(T) -> str`\
  Construct a string.
* `list(Iter[T]) -> T`\
  Construct a list from an iterable.

#### Builtins for working with numbers <a href="#builtins-for-working-with-numbers" id="builtins-for-working-with-numbers"></a>

* `abs({Numeric} T) -> T`\
  Get the absolute value of a number.
* `min(...{Numeric} T) -> T`\
  Get the minimum of some numbers. Seahorse does not support `min`'s [alternate iterable form](https://docs.python.org/3/library/functions.html?highlight=round#min).
* `max(...{Numeric} T) -> T`\
  Get the maximum of some numbers, also does not support the iterable form.
* `round(f64) -> i128`\
  Round a floating-point number to the nearest integer.

#### Builtins for working with iterators <a href="#builtins-for-working-with-iterators" id="builtins-for-working-with-iterators"></a>

Note that `Iter[T]` includes any type that can be iterated over, like lists and arrays.

* `len(Iter[T]) -> u64`\
  Get the length of an iterable.
* `enumerate(Iter[T]) -> Iter[(u64, T)]`\
  Obtain an iterator that also gives you the index of each item.
* `filter((T) -> bool, Iter[T]) -> Iter[T]`\
  Obtain an iterator that filters out certain elements.
* `map((T) -> U, Iter[T]) -> Iter[U]`\
  Obtain an iterator that transforms each element of the original iterable.
* `range({Numeric} T, {Numeric} T?, {Numeric} T?) -> Iter[T]`\
  Obtain an iterator over a range of numbers. Like in Python, `range(a)` counts from 0 to `a` (exclusive), `range(a, b)` counts from `a` to `b`, and `range(a, b, k)` counts from `a` to `b` in increments of `k`.
* `sorted(Iter[T]) -> List[T]`\
  Obtain a sorted list from an iterable.
* `sum(Iter[{Numeric} T]) -> T`\
  Get the sum of the elements in an iterable.
* `zip(Iter[T], Iter[U]) -> Iter[(T, U)]`\
  Obtain an iterator that traverses two iterators simultaneously. Seahorse does not support a variadic amount of iterators like in Python.

### Type hints and static typing <a href="#type-hints-and-static-typing" id="type-hints-and-static-typing"></a>

In Python, you can provide optional _type hints_ on variables assignments, class fields, and function parameters. These hints are completely ignored by Python when your code runs, but your editor might make use of them to allow autocomplete and other features that rely on knowing the types of objects.

In Seahorse, type hints are occasionally mandatory in order to allow the underlying Rust code to be _statically typed_ - that is, typed at compile time. The following table summarizes when you have to (or might want to) use type hints:

| Location            | Needs type hints?                                                                                                                                                                                                                                                               |
| ------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Class fields        | **ALWAYS**, unless the class is an Enum.                                                                                                                                                                                                                                        |
| Function parameters | **ALWAYS**.                                                                                                                                                                                                                                                                     |
| Variable assignment | **MAYBE**, Rust has a powerful type inference system that can usually fill in the type of a variable when it is declared. If this isn't enough, you can provide a type when assigning a variable (`var: Type = value`) and Seahorse will make sure the value is the right type. |

Using Seahorse, most of your variables can be automatically typed as long as your class fields and function parameters are. Sometimes it might fail and you'll have to add a manual type or two, but for the most part you can rely on it to get you the result you expect.

#### Numbers and math <a href="#numbers-and-math" id="numbers-and-math"></a>

Rust has much stricter rules for doing simple math operations than Python - you can't add two different types of numbers, even if the only difference between them is their size (e.g. u8 vs. u64).

Seahorse preserves this while maintaining some flexibility by performing _automatic numeric coercion_ in certain situations (mainly if you're performing arithmetic operations). Most mathematical operations will ensure the types of both operands are the same by coercing them to the _less strict_ of the two types. In practice, this means that small integers will coerce to big integers, which will coerce to floating point numbers, and never the other way around. (This is essentially what Python does with math between `int`s and `float`s, but safely applied to more types.)

Seahorse supports every fixed-width int size and `f64`. The automatic coercion rules are simple:

* An unsigned int can be coerced to any wider (more bits) unsigned/signed int type
* A signed int type can be coerced to any wider signed int type
* Any int type can coerce to `f64`.

Like in Rust, an untyped integer is assigned a type based on usage. If you declare a variable `x = 8`, Seahorse only knows that it belongs to some numeric type. If later, for instance, you pass `x` to a function that expects a `u64` as an argument, then Seahorse will assign the `u64` type to `x`. And if you then pass `x` to a function that expects `u16` as an argument, Seahorse will throw an error, because `x` is a `u64`.

Here are some examples to make all of that concrete:

```
a      = u32(2)  # u32, constructors always return their type
b: i64 = 2       # i64, since this is explicitly declared
c      = a + b   # i64, a will be coerced to an i64 in order to add it to
                 # b, producing an i64.


d: u64 = 3
e = 10 // d  # u64, integer division simply coerces both sides to the same type
f = 10 / d   # f64, regular division requires floating points (and coerces both
             # sides as such)


g: u8 = 2
h: i8 = 3
i = g + h  # ERROR - u8 cannot coerce to i8, and i8 cannot coerce to u8.
```

There are two special operations to remember: `/` (non-integer division) and `**` (exponentation).

As shown above, non-integer division will always coerce both sides to floating-point (`f64`), and returns an `f64`.

Exponentiation has two modes: integer and non-integer. When doing `base ** exponent`, Seahorse will decide to try either integer/non-integer exponentiation based on the type of `base`. If `base` is an integer, then `exponent` will attempt to be coerced to a `u32`. Otherwise, if `base` is an `f64`, then the `exponent` will be cast to an `f64`. You can't raise an integer to a non-`u32` power.

```
a = 2
b: u16 = 2
c: i64 = 2


x = 10    # x is some integer
y = 10.0  # y is an f64


x ** a  # This will work (and a now becomes u32 based on usage)
x ** b  # This will work - u16 can coerce to u32
x ** c  # ERROR - i64 cannot coerce to u32, even though it's technically
        # just a constant 2


y ** c  # This will work - c will get coerced to f64 to do non-integer
        # exponentiation
```

#### Scoping rules <a href="#scoping-rules" id="scoping-rules"></a>

When your code gets compiled to Rust, certain rules need to be obeyed. Each declaration of a variable must have exactly one type, and scopes delineate where variables may be accessed. Python breaks both of those rules, resulting in runnable code like this:

```
x = 'string'


if condition:
  x = 2
  y = 3


print(x + y)
```

...which will print "5" if `condition` was true. Attempting to compile this to valid Rust code would be a mess, so instead Seahorse imposes Rust's rules onto Seahorse in the following way:

* You may not reassign a variable to a new type _if the assignment happens in a deeper scope_.
* Scoping works like in Rust, variables will be dropped as soon as the scope they're declared in ends.

Note that the first rule allows you to reassign a variable to a different type while in the same scope - you can still write code like this:

```
x = 'string'
# ...
x = 0
# ...
x = False
```

### Scripts vs. modules <a href="#scripts-vs-modules" id="scripts-vs-modules"></a>

In a Python script, every top-level statement is run in sequence, and the result of the script is just whatever happens during those statements. If you import the script as a module, then the code just runs as usual and exposes all the new names to the importer.

In Seahorse, there is no concept of a script - the code you write is used to generate a Rust library, which is analagous to a Python module with some extra limitations. Statements can not be run unless they are part of a function that gets called. The following table summarizes what can do in your Seahorse file as a top-level statement:

| Statement               | Description                                                                                                                                   |
| ----------------------- | --------------------------------------------------------------------------------------------------------------------------------------------- |
| Imports                 | Imports for Seahorse builtin libraries and local files                                                                                        |
| Class definitions       | Arbitrary classes                                                                                                                             |
| Function definitions    | Arbitrary functions                                                                                                                           |
| Instruction definitions | Functions decorated with `@instruction`                                                                                                       |
| `declare_id('...')`     | A special statement that tells Anchor what your program's ID is - more on this [here](https://seahorse-lang.org/docs/seahorse-lang#declareid) |

### Directives <a href="#directives" id="directives"></a>

Although you can't put arbitrary top-level statements in your program, there are some special statements, known as _directives,_ that allow you to control the compiler more than just generated code. (Right now the only directive is `declare_id`, but more will be added in a future release!)

#### declare\_id <a href="#declare-id" id="declare-id"></a>

Anchor has a Rust macro called `declare_id!` that is needed to make sure your program knows its own key. When you `seahorse init` a new project, the resulting **.py** file includes a `declare_id` at the top:

```
# Default ID - every Anchor program starts with this ID, this is not your program's unique ID yet!
declare_id('Fg6PaFpoGXkYsidMpWTK6W2BeZ7FEfcYkg476zPFsLnS')
```

However, this ID might change when you recompile with Anchor. This is an especially common hangup when testing your code for the first time.

All you need to do is fetch the new ID from `/target/idl/<program>.json`:

```
{
  ...


  "metadata": {
    "address": "55bK1XrRae9iWca1i5CJQqH9nxSD1faBbvx6qViLmoRs"
  }
}
```

...and paste it into the Seahorse `declare_id` statement:

```
# New ID taken from the IDL file
declare_id('55bK1XrRae9iWca1i5CJQqH9nxSD1faBbvx6qViLmoRs')
```
