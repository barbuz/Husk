# Husk

Husk is a functional [golfing language](https://en.wikipedia.org/wiki/Code_golf), inspired by (and implemented in) [Haskell](https://haskell.org).
Documentation can be found on the [Wiki](https://github.com/barbuz/Husk/wiki).

## Language concepts

Many of the fundaments of this language are derived from Haskell, so if you are familiar with Haskell it will be easier for you to learn Husk. The main difference is given by a type inferencing step done at compilation time which can guess the desired function among different functions with the same name, basing on how their types unify with the types of other functions and the input. In practice, this means that most functions in Husk are overloaded to do different things based on the type of their arguments.

## Running Husk

The Husk interpreter here provided requires a Haskell compiler. Download all the source files, compile `Husk.hs`, and you're set.

An Husk program may be launched with the command `Husk`, passing as first argument the source of the program, and as following arguments the inputs to the program. When run, Husk will infer the possible types of the program and the inputs, unify them, and then produce an Haskell program with fixed typing which is executed right away.
Husk reads Unicode source with the flag `-u`, byte-encoded source with the flag `-b`, and verbose ASCII source with the flag `-v`.
For more information on using the interpreter, please check the Wiki.
