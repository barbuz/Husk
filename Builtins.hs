module Builtins (bins, cmd, commands) where

import Expr

-- Utilities for writing types
[x,y,z,u,v,w,n,m] = map (TVar . pure) "xyzuvwnm"

int :: Type
int = TConc TInt

dbl :: Type
dbl = TConc TDouble

chr :: Type
chr = TConc TChar

lst :: Type -> Type
lst = TList

tup :: Type -> Type -> Type
tup = TPair

con :: Type -> (TClass, Type)
con typ = (Concrete, typ)

num :: Type -> (TClass, Type)
num typ = (Number, typ)

forall :: String -> [(TClass, Type)] -> Type -> Scheme
forall vars cons typ = Scheme (map pure vars) $ CType cons typ

simply :: Type -> Scheme
simply typ = forall "" [] typ

-- Compute command from char
cmd :: Char -> Exp [Lit Scheme]
cmd char | Just exp <- lookup char commandsList = exp
cmd char = error $ "No builtin bound to character " ++ [char]

-- List of commands
commands :: String
commands = map fst commandsList

-- Assoc list of commands that can occur in source
commandsList :: [(Char, Exp [Lit Scheme])]
commandsList = [
  ('+', bins "add addDI addID cat"),
  ('-', bins "sub subDI subID"),
  ('*', bins "mul mulDI mulID"),
  ('/', bins "div"),
  ('÷', bins "idiv"),
  ('%', bins "mod modDI modID"),
  ('_', bins "neg"),
  ('i', bins "inv"),
  (';', bins "pure"),
  (',', bins "pair"),
  (':', bins "cons snoc"),
  ('m', bins "map mapr"),
  ('z', bins "zip"),
  ('«', bins "foldl foldl1"),
  ('»', bins "foldr foldr1"),
  ('◄', bins "scanl scanl1"),
  ('►', bins "scanr scanr1"),
  ('f', bins "filter select"),
  ('L', bins "len nlen"),
  ('#', bins "countf count"),
  ('r', bins "range"),
  ('N', bins "nats"),
  ('‼', bins "index"),
  ('↑', bins "take takew"),
  ('↓', bins "drop dropw"),
  ('←', bins "head fst"),
  ('→', bins "last snd"),
  ('↔', bins "swap rev flip"),
  ('h', bins "init"),
  ('t', bins "tail"),
  ('ƒ', bins "fix"),
  ('F', bins "fixp"),
  ('<', bins "lt"),
  ('>', bins "gt"),
  ('≤', bins "le"),
  ('≥', bins "ge"),
  ('=', bins "eq"),
  ('≠', bins "neq"),
  ('?', bins "if if2"),
  ('¿', bins "fif"),
  ('¬', bins "not"),
  ('|', bins "or or'"),
  ('&', bins "and and'"),
  ('S', bins "hook"),
  ('K', bins "const"),
  ('I', bins "id"),
  ('Λ', bins "list listN listF listNF"),
  ('Σ', bins "sum trianI trianD concat"),
  ('Π', bins "prod fact cartes"),
  ('§', bins "fork fork2"),
  ('‡', bins "argdup")
  ]

-- Compute builtins from space-delimited list
bins :: String -> Exp [Lit Scheme]
bins names = ELit $ map getBuiltin $ words names
  where getBuiltin name | Just typ <- lookup name builtinsList = Lit "func_" name typ
        getBuiltin name = error $ "No builtin named " ++ name

-- Assoc list of builtins
builtinsList :: [(String, Scheme)]
builtinsList = [

  -- Arithmetic
  ("add",   forall "n" [num n] $ n ~> n ~> n),
  ("addID", simply $ int ~> dbl ~> dbl),
  ("addDI", simply $ dbl ~> int ~> dbl),
  ("sub",   forall "n" [num n] $ n ~> n ~> n),
  ("subID", simply $ int ~> dbl ~> dbl),
  ("subDI", simply $ dbl ~> int ~> dbl),
  ("mul",   forall "n" [num n] $ n ~> n ~> n),
  ("mulID", simply $ int ~> dbl ~> dbl),
  ("mulDI", simply $ dbl ~> int ~> dbl),
  ("div",   forall "mn" [num m,num n] $ m ~> n ~> dbl),
  ("idiv",  forall "mn" [num m,num n] $ m ~> n ~> int),
  ("mod",   forall "n" [num n] $ n ~> n ~> n),
  ("modID", simply $ int ~> dbl ~> dbl),
  ("modDI", simply $ dbl ~> int ~> dbl),
  ("neg",   forall "n" [num n] $ n ~> n),
  ("inv",   forall "n" [num n] $ n ~> dbl),
  ("trianI",simply $ int ~> int),
  ("trianD",simply $ dbl ~> dbl),
  ("fact",  forall "n" [num n] $ n ~> n),

  -- List and pair manipulation
  ("pure",  forall "x" [] $ x ~> lst x),
  ("pair",  forall "xy" [] $ x ~> y ~> tup x y),
  ("swap",  forall "xy" [] $ tup x y ~> tup y x),
  ("cons",  forall "x" [] $ x ~> lst x ~> lst x),
  ("cat",   forall "x" [] $ lst x ~> lst x ~> lst x),
  ("snoc",  forall "x" [] $ lst x ~> x ~> lst x),
  ("len",   forall "x" [] $ lst x ~> int),
  ("nlen",  forall "n" [num n] $ n ~> int),
  ("countf",forall "xy" [con y] $ (x ~> y) ~> lst x ~> int),
  ("count", forall "x" [con x] $ x ~> lst x ~> int),
  ("head",  forall "x" [] $ lst x ~> x),
  ("last",  forall "x" [] $ lst x ~> x),
  ("init",  forall "x" [] $ lst x ~> lst x),
  ("tail",  forall "x" [] $ lst x ~> lst x),
  ("fst",   forall "xy" [] $ tup x y ~> x),
  ("snd",   forall "xy" [] $ tup x y ~> y),
  ("index", forall "x" [] $ int ~> lst x ~> x),
  ("take",  forall "x" [] $ int ~> lst x ~> lst x),
  ("takew", forall "xy" [con y] $ (x ~> y) ~> lst x ~> lst x),
  ("drop",  forall "x" [] $ int ~> lst x ~> lst x),
  ("dropw", forall "xy" [con y] $ (x ~> y) ~> lst x ~> lst x),
  ("rev",   forall "x" [] $ lst x ~> lst x),
  ("range", forall "mn" [num m,num n] $ m ~> lst n),
  ("nats",  forall "n" [num n] $ lst n),
  ("concat",forall "x" [] $ lst (lst x) ~> lst x),
  ("sum",   forall "n" [num n] $ lst n ~> n),
  ("prod",  forall "n" [num n] $ lst n ~> n),
  ("cartes",forall "x" [] $ lst (lst x) ~> lst (lst x)),

  -- Higher order functions
  ("map",   forall "xy" [] $ (x ~> y) ~> (lst x ~> lst y)),
  ("mapr",  forall "xy" [] $ lst (x ~> y) ~> x ~> lst y),
  ("zip",   forall "xyz" [] $ (x ~> y ~> z) ~> (lst x ~> lst y ~> lst z)),
  ("fixp",  forall "x" [con x] $ (x ~> x) ~> x ~> x),
  ("filter",forall "xy" [con y] $ (x ~> y) ~> lst x ~> lst x),
  ("select",forall "xy" [con x] $ lst x ~> lst y ~> lst y),
  ("foldl", forall "xy" [] $ (y ~> x ~> y) ~> y ~> lst x ~> y),
  ("foldl1",forall "x" [] $ (x ~> x ~> x) ~> lst x ~> x),
  ("foldr", forall "xy" [] $ (x ~> y ~> y) ~> y ~> lst x ~> y),
  ("foldr1",forall "x" [] $ (x ~> x ~> x) ~> lst x ~> x),
  ("scanl", forall "xy" [] $ (y ~> x ~> y) ~> y ~> lst x ~> lst y),
  ("scanl1",forall "x" [] $ (x ~> x ~> x) ~> lst x ~> lst x),
  ("scanr", forall "xy" [] $ (x ~> y ~> y) ~> y ~> lst x ~> lst y),
  ("scanr1",forall "x" [] $ (x ~> x ~> x) ~> lst x ~> lst x),
  ("list",  forall "xy" [] $ y ~> (x ~> lst x ~> y) ~> lst x ~> y),
  ("listN", forall "xy" [] $ (x ~> lst x ~> y) ~> lst x ~> y),
  ("listF", forall "xy" [] $ y ~> ((lst x ~> y) ~> (x ~> lst x ~> y)) ~> lst x ~> y),
  ("listNF",forall "xy" [] $ ((lst x ~> y) ~> (x ~> lst x ~> y)) ~> lst x ~> y),
  
  -- Combinators
  ("hook",  forall "xyz" [] $ (x ~> y ~> z) ~> (x ~> y) ~> x ~> z),
  ("const", forall "xy" [] $ x ~> y ~> x),
  ("id",    forall "x" [] $ x ~> x),
  ("fix",   forall "x" [] $ (x ~> x) ~> x),
  ("flip",  forall "xyz" [] $ (x ~> y ~> z) ~> (y ~> x ~> z)),
  ("com3",  forall "xyzuv" [] $ (u ~> v) ~> (x ~> y ~> z ~> u) ~> (x ~> y ~> z ~> v)),
  ("com2",  forall "xyzu" [] $ (z ~> u) ~> (x ~> y ~> z) ~> (x ~> y ~> u)),
  ("com",   forall "xyz" [] $ (y ~> z) ~> (x ~> y) ~> (x ~> z)),
  ("app",   forall "xy" [] $ (x ~> y) ~> x ~> y),
  ("fork",  forall "xyzu" [] $ (x ~> y ~> z) ~> (u ~> x) ~> (u ~> y) ~> u ~> z),
  ("fork2", forall "xyzuv" [] $ (x ~> y ~> z) ~> (u ~> v ~> x) ~> (u ~> v ~> y) ~> u ~> v ~> z),
  ("argdup",forall "xy" [] $ (x ~> x ~> y) ~> x ~> y),

  -- Boolean functions and comparisons
  ("lt",    forall "x" [con x] $ x ~> x ~> int),
  ("gt",    forall "x" [con x] $ x ~> x ~> int),
  ("le",    forall "x" [con x] $ x ~> x ~> int),
  ("ge",    forall "x" [con x] $ x ~> x ~> int),
  ("eq",    forall "x" [con x] $ x ~> x ~> int),
  ("neq",   forall "x" [con x] $ x ~> x ~> int),
  ("if",    forall "xy" [con x] $ y ~> y ~> x ~> y),
  ("if2",   forall "xy" [con x] $ (x ~> y) ~> y ~> x ~> y),
  ("fif",   forall "xyz" [con x] $ (z ~> x) ~> (z ~> y) ~> (z ~> y) ~> z ~> y),
  ("not",   forall "x" [con x] $ x ~> int),
  ("or",    forall "x" [con x] $ x ~> x ~> x),
  ("or'",   forall "x" [con x, num n] $ x ~> x ~> n),
  ("and",   forall "x" [con x] $ x ~> x ~> x),
  ("and'",  forall "x" [con x, num n] $ x ~> x ~> n)
  ]
