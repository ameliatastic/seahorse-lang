# The Seahorse Compiler: Overview

(Writing this readme for my own clarity. And posterity, I guess.)

Translating Python source to Rust is not as simple as just turning Python syntax into Rust syntax. Python and Rust have different module (import) systems and very different type systems. Furthermore, Anchor has its own quirks that are translated into a clean Pythonic syntax, but this introduces more things to deal with during compilation.

The compiler is split into 5 main stages:
1. Parse: turn a Python source file into a _syntax tree_ (a.k.a. AST - the A stands for abstract, which might be a technical misnomer). This is a structure that lets us more easily view the Python source as a tree of rich information, rather than a blob of text. This stage is entirely handled by the `rustpython_parser` library.
2. Clean: turn the Python AST into a slightly different AST. This new AST contains no invalid Python constructs (i.e., async expressions), and makes things a bit easier to handle later down the line.
3. Preprocess: check the cleaned AST for import statements, and load + parse + clean the corresponding Python modules. All the modules are placed in a "registry", which basically just mimics the file system hierarchy that the modules come from.
4. Compile: rest of the owl - turn our registry of modules into a Rust-like AST. This stage is split into 4 sub-stages:
    1. Namespace: turn each module into a _namespace_ that contains a mapping from symbols to top-level objects, which includes both imports and objects defined in the module. So if you `import seahorse`, `import other_package as op` and then make a `class MyAccount`, the resulting namespace will contain entries for "seahorse" -> builtin seahorse module, "op" -> other_package in the module hierarcy, and "MyAccount" -> class definition.
    2. Sign: give each (locally defined) object a type signature. Function parameters and return type is checked and turned into a type, and the same happens for class fields + methods.
    3. Check: likely the biggest and most important stage is typechecking every function. Essentially, the code is evaluated to make sure that there will be no errors in the resulting Rust code - this stage will reject statements like `x = 'string' + 5` since that doesn't make any sense. The Seahorse compiler type system includes some room for "compiler magic" to happen, more on that later.
    4. Build: the typechecked functions are turned into the Rust-like AST mentioned earlier. Having the typecheck results makes relatively easy (compared to v1 lol), but there's still some intricacy due to the way that Anchor works.
5. Generate: turn the Rust-like AST into source code. One of the simpler stages.

Of these stages, parse and clean are simple enough that they don't need any discussion. Everything after that is complex and warrants some discussion.

## Preprocess

The output of this stage is a module registry, which lets us find imports later.

Python modules and packages can be imported from different sources, which are specified in an environment variable (PYTHONPATH). When you `import module`, the interpreter checks each source (a directory in your filesystem) for a package/module named `module`, returning the first match it finds.

Seahorse mimics this by putting all code in the same module tree, giving each source in the (virtual) PYTHONPATH a unique name - for instance, the folder that your source code comes from is called `dot`. The tree might eventually looks like this:

- (root) /
    - dot/
        - program.py
        - util.py
        - seahorse/
            - prelude.py
    - (another package source) ext/
        - (imported packages)

This can be easily translated to a Rust module tree - `lib.rs` lives in the root folder, and contains modules `dot` and `ext`. Then, your program's code lives in the module `dot::program`, and your util code lives in `dot::util`. Since everything is now part of the same module tree, as long as we know the absolute path of an exported object, we can find it and reference it in code.

## Namespace

The output of this stage is a new tree that contains a mapping from symbol name to either an import or a definition, which lets us find the absolute path of objects later.

Each time you reference a variable in Python, it can come from either the module's global namespace or the local scope of whatever function you're in. So there's a few different types of variables:
- Local variables
- Global defined objects (like classes and functions)
- Imports from another module
- Imports of another module/package

Seahorse also relies on having many different builtin (compiler-aware) types and functions.

By building a module's namespace, we find all of the names of objects in the module's global scope and map them to their location in the module tree - whether that be an object defined in the same module, or an import from another module. Seahorse also includes builtin objects by placing a pre-made namespace in the module tree at the location of the prelude.py file - so when you use from this file, the compiler knows exactly what you can import.

Builtins are very special to the compiler, since they are responsible for a lot of the magic that happens when converting Python to Rust. More detail is provided in the check stage. You can look at **core/compile/builtin/mod.rs** for the `BuiltinSource` trait, a trait that outlines everything needed to make a compiler builtin object.

## Sign

The output of this stage is a newtree that contains a mapping from symbol name to its type signature.

"Type signature" is a bit of a misnomer, but I couldn't think of another name. Basically, each defined type/function in a module needs some information so that we know how to use it. For regular classes, this means its fields, methods, and whether the class is an account. For functions, this means its parameters and return type.

Accounts are special because of the way that code gets generated for them, so they're sort of separated from "regular" types in the type system.

## Check

This is the biggest and probably most important stage. The output of this stage is, for each function in a module, a mapping from the module's expressions to the types that they resolve to + optionally, whether the expression needs to be syntactically transformed.

Rust has a very strict type system, and Seahorse needs to match that strictness while also allowing for Python-like flexibility. The basic rules are:
- Each expression must have a single known type at compile time
- Variables may change type only if it happens in the same scope that they're defined in

Now I'll try to explain how typechecking works.

Basic types take the form of a generic type with a list of zero or more type parameters.

```
Ty:
    Generic(TyName, [Ty])
```

There also needs to be a type for functions, which just contains a list of parameters and return type:

```
Ty:
    Generic(TyName, [Ty])
    Function([Ty], Ty)
```

(The actual function type is a bit more complex than this because of how you can call things in Python, but this should work for illustrative purposes.)

So if we have a `Function([u64], str)` and in Seahorse we call it with args `(u64)`, then the expression should successfully evaluate to a `str`. If we instead called it with args `(str)`, or args `(u64, u64, u64)`, it should fail and the typechecker should throw an error.

This works as a very simple type system, but it's extremely inflexible. For example, what would the type of the `List.append` method be? You should be able to append a `T` to a `List[T]`, but we don't have anything that allows that yet. So let's fix that by adding the "Param" type, which can act as a free type or a reference to a known type:

```
Ty:
    Param(int)
    Generic(TyName, [Ty])
    Function([Ty], Ty)
```

Now we can use generics! The typechecker just needs a mechanism to say "when you try to call `.append` on a `List[T]`, it should give back a `Function([T], None)` (a function that takes in one parameter - a `T` - and returns nothing)". The int is essentially the name of the Param type - under the hood, it just references its index in a list of types that the typechecker references.

However, this doesn't do everything for us yet - what if we create an empty list in Seahorse then try to append to it?

```py
a = []
a.append(x) # Say `x` is a u64
```

There's a problem here - all we know is that `a` is a list, but we don't know what type its elements have. Besides, what if later we tried to append a `str` to the list? It should obviously fail, but we don't have anything to know that yet.

This is where _unification_ comes in. I might be using this term wrong (type theory is way more academic than I am), but basically it means finding a way of substituting types for each other in a way that is cohesive with the usage of each expression. So once we append a `u64` to a `List[T]`, we need to substitute `u64` for `T` everywhere `T` is used. In the example above, here's roughly what would happen:

```py
a = []
# Create a new free type T, assign `a` : T
# Since the right-hand side is an empty list:
#     Create a new free type U
#     Unify T with List[U] - now we know T = List[U], so `a` : List[U]
a.append(x)
# Create a new free type V (for the whole expression)
# Find the type of `a` : List[U]
# Find the type of List[U].append : Function([U], None)
# Find the parameters of the function call : [u64]
# Unify the function call with its parameters, and the return type:
#     Unify u64 with U - now we know U = u64, so `a` : List[u64]
#     Unify V with None.
a.append(s)
# If `s` is a `str`, this will fail - we'll try to unify `str` with `u64`, which doesn't work
```

Unification allows us to do more complex things than just assign free variables to absolute types. For example, what type should integer literals have? We want them to become Rust integers, but we don't want to have to write out their types every time. But, by adding a variant of the param type - IntParam - we can make a new type that will act as a free type but only unify with Rust ints:

```py
x = 1
# x: {Numeric} T
a: List[u64] = []
# a: List[u64]
a.append(x)
# Unify u64 with {Numeric} T - now we know x: u64.

# If we had done something like this instead...
a: List[str] = []
a.append(x)
# ...then this would error, since str can't unify with a {Numeric} type
```

Another magic type that can be added is a type that allows multiple similar types in. For example, the seeds list in an `Empty.init` call allows strings, ints, and even accounts as members. This is important for ergonomics, and impossible with the type system I've described so far. But, we can add a new magic type called Cast that allows any type that can _cast_ to a different type. The type signature is `Empty[T].init : Function([Signer, List[Cast<Seed>]], T)` - the `init` function takes in a `Signer` and a list of anything that can be cast to a `Seed`. Then we simply right a rule saying that strings, ints, and accounts can all be cast to `Seed`.

```py
@instruction
def init_protocol(signer: Signer, seed: u64, empty_protocol: Empty[Protocol]):
    empty_protocol.init(Signer, ['protocol', signer, seed])
```

So we can typecheck expressions while having both flexibility and type strictness. But the compiler still has a lot of work to do - Rust doesn't care about our `Cast<Seed>` rule, and we still need to insert code to transform each seed into bytes under the hood. This is where the other important magic type - `Transform` - comes into play.

`Transform(ty, transformation)` defines a type that needs to have a specific syntactic `transformation` applied to it, while operating as type `ty`. As an example, the (internally-used) type of `floor` looks something like this: `floor(f64) -> Transform(f64, transformation)`. The `transformation` then gets applied to the syntax element that has the return type, and turns it into the proper Rust conversion - making `floor(x)` into `x.floor()`.

A transformation is simply a function that takes an element of our Rust-like AST and converts it into a different form of the same AST, and maybe including some extra information. Extra information covers extra effects outside of the AST element we're given - for example, `Empty.init` needs to extract the payer and seeds arguments and let the build stage pass them to the Anchor accounts context for the instruction that we're in. Most transformations are defined somewhere in **core/compile/builtin**.

The typechecker eventually touches every single expression and sub-expression in the function. The order in which expressions are visited is a well-defined depth-first traversal. While it finds the type of each expression, the typechecker also outputs a list containing each expression's type, in the same order that they get visited. This allows the next stage to reconstruct the type info of each expression, which crucially includes transformations.

## Aside - Uniting the programming models of Python and Rust

Before we get into the build stage, now might be a good time to talk about exactly how Python and Rust differ, and how we can minimize their differences by generating extra boilerplate code.

In Python, every variable you make is actually just an alias for a value. When you reassign something, you're just rebinding the variable name to a new value. Python can then destroy the old variable once it knows it's no longer being used. Each value has a type, and types can either be mutable or immutable - that is, you can either change the data inside them or you can't. For example, integers are immutable but lists are mutable.

What happens when you pass a variable to a function? Well, the variable is just an alias for a value, so it's like you've just given a _reference_ to that value. If you pass a list to a function and the function modifies the list, the modification actually happens to the original list.

Variables are also dynamically typed. You can have some code like this:

```py
a = 0
if condition:
    a = [1, 2, 3]

a.append('value')
```

...and Python will only ever complain if `condition` happens to be false, because ints don't have an `append` method.

Rust has none of this flexibility by default, so we need to make some adjustments to the code we generate.

The first big breakthrough is to go through and type everything, which we just did. That immediately lets us throw out user code that does something which would be impossible in Rust (like the example above). However, since Rust allows you to _shadow_ variables (redeclare them with a different type without changing the original variable), we can grant a little more flexibility - if a variable changes its type, we can allow it if it happens in the same scope that it was declared in. This will just correspond to a new `let` statement, and everything will proceed as usual. (If we allowed variables to change their type from within a different scope, we'd accidentally introduce cases like the example above.)

The second big breakthrough is to use the hell out of the "interior mutability" pattern. This is a Rust pattern where, instead of holding onto a mutable piece of data (like a `Vec<u64>`), we hold onto a smart reference to it that allows us to mutate it, in this case an `Rc<RefCell<Vec<u64>>>`.

Normal Rust references are strict - you can either have many immutable references (`&value`) to a piece of data, or a single mutable reference (`&mut value`). The interior mutability pattern allows us to have many mutable refrences to some underlying data. And in case you're worried that this kind of "breaks Rust" (or its memory safety guarantees), it doesn't - `RefCell` is a standard-library solution that still only allows you to mutably borrow the data once at a time.

Last thing - what about on-chain accounts? These can't store complex/heap stored data types like `Rc<RefCell<...>>`, so how can we use this like we can in Python? This brings us to our third breakthrough - loading and storing data from the chain into our "modified" programming model.

For each account, we need to generate a secondary struct that has 2 things:
- The same fields as in the original account, maybe modified to be `Rc<RefCell<...>>`s if the type is mutable
- A mutable reference to the account itself

Then, right before an instruction is called (or rather, right before its underlying code is called) we take each account and load its data into the alternate struct. This gives us a shareable, mutable reference to an account that also has its own shareable and mutable data. Then, once the instruction finishes, we destroy the alternate struct and store the data back into the account.

So that's it, 3 big changes to otherwise "normal" Rust code that lets us do almost anything we can in Python. On to the build stage!

## Build

The output of this stage is a Rust-like AST with some constructs tailored for Anchor.

When building a function, the build step examines every syntax element defined in the parsed + cleaned Python AST and transforms it into something that can directly generate Rust code. Like the typechecker did, we iterate over each statement/expression of each function in the same depth-first order. Each expression gets its type information back and is 1. turned into a `TypedExpression` that has the type + direct-to-Rust AST conversion and 2. transformed if the type demands it.

Each other syntax element is simpler - classes are turned into structs, enums, and accounts, imports are turned into their Rust-equivalent `use` statement.

One final thing needs to happen before kicking this stage to generate - the `seahorse` node is cut from the source tree so that we don't accidentally try to generate code for our builtin libraries.

## Generate

The output of this stage is a tree of strings, each of which maps to a Rust source file.

Finally we're here! This stage is actually pretty simple, each element in our Rust source AST needs to get translated to a string. To do this, we use the **quote** crate to generate a token stream (which is much more friendly to work with than raw strings) then turn that into a string for each file. You can think of this stage as just templating on steroids.

Besides just translating each source file, we also need to generate:
- A **mod.rs** for each directory. This is just a file in every (non-root) node of the tree that contains `mod` statements for the submodules being defined.
- An appropriate **lib.rs** for the program as a whole.

**lib.rs** will look roughly like this:

```
import some std:: things, anchor_lang:: things
import the contents of the origin module

add mod statements for each sub-module in the source tree

define a module "seahorse_util" that includes some constructs to help us clean up the generated code a bit:
    - for example, a type def for Mutable<T> = Rc<RefCell<T>>

define the entrypoint module for the whole program:
    define the Accounts context for each instruction:
        - this just involves converting each account-type param to the appropriate Rust definition (along with any inferred accounts)

    define each instruction:
        1. load each account:
            - for program accounts, this means translating them to the appropriate Loaded* struct
            - for other (Anchor-defined) accounts, this means putting the account in a Seahorse-friendly struct so their data can be shared easily
        2. call the instruction handler, which is just the original function defined as an @instruction in Seahorse
        3. store each program account - take the data out of the Loaded* struct and put it back into the original account
```

Once we've generated everything, the generated tree of an entire project might looks something like this:

```
(root)
|\_ lib.rs
|\_ dot
    |\_ mod.rs
    |\_ program.rs
    |\_ util
        |\_ mod.rs
        |\_ data_structures.rs
        |\_ algorithms.rs
```

The generate stage then passes this tree back to its caller (probably the `seahorse build` command), all of the files get written to the filesystem, and `anchor build` runs to compile the code for real!