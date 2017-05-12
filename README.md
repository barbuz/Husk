# Husk

Husk is a functional [golfing language](en.wikipedia.org/wiki/Code_golf), inspired by (and implemented in) [Haskell](www.haskell.org).

This language is still in an early development phase, so anything may be subject to modification without prior notice.

## Language concepts

Many of the fundaments of this language are derived from Haskell, so if you are familiar with Haskell it will be easier for you to learn Husk. The main difference is given by a type inferencing step done at compilation time which can guess the desired function among different functions with the same name, basing on how their types unify with the types of other functions and the input. In practice, this means that most functions in Husk are overloaded to do different things based on the type of their arguments.

### Types

### Program syntax

### Builtins

---

## Running Husk

The Husk interpreter here provided requires an Haskell compiler. Download all the source files, compile *Husk.hs*, and you're set.

An Husk program may be launched with the command `Husk`, passing as first argument the source of the program, and as following arguments the inputs to the program. When run, Husk will infer the possible types of the program and the inputs, unify them, and then produce an Haskell program with fixed typing which is executed right away.

The following flags are recognized by Husk:

- `-i` : only infer the possibly types of the program and print them out. Don't execute the program.
- `-f filename` : instead of using the first argument as source, load the source from the file `filename`.
- `-o filename` : save the transpiled Haskell code into the file `filename`.