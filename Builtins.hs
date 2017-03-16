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
  ('+', bins "add addDI addID"),
  ('-', bins "sub subDI subID"),
  ('*', bins "mul mulDI mulID"),
  ('_', bins "neg"),
  (';', bins "pure"),
  (':', bins "cat cons snoc pair"),
  ('m', bins "map"),
  ('z', bins "zip"),
  ('F', bins "fixp"),
  ('<', bins "lt"),
  ('>', bins "gt"),
  ('=', bins "eq"),
  ('?', bins "if"),
  ('S', bins "hook"),
  ('K', bins "const"),
  ('I', bins "id")
  ]

-- Compute builtins from space-delimited list
bins :: String -> Exp [Lit Scheme]
bins names = ELit $ map getBuiltin $ words names
  where getBuiltin name | Just typ <- lookup name builtinsList = Lit name typ
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
  ("neg",   forall "n" [num n] $ n ~> n),

  -- List manipulation
  ("pure",  forall "x" [] $ x ~> lst x),
  ("pair",  forall "x" [] $ x ~> x ~> lst x),
  ("cons",  forall "x" [] $ x ~> lst x ~> lst x),
  ("cat",   forall "x" [] $ lst x ~> lst x ~> lst x),
  ("snoc",  forall "x" [] $ lst x ~> x ~> lst x),

  -- Higher order functions
  ("com3",  forall "xyzuv" [] $ (u ~> v) ~> (x ~> y ~> z ~> u) ~> (x ~> y ~> z ~> v)),
  ("com2",  forall "xyzu" [] $ (z ~> u) ~> (x ~> y ~> z) ~> (x ~> y ~> u)),
  ("com",   forall "xyz" [] $ (y ~> z) ~> (x ~> y) ~> (x ~> z)),
  ("app",   forall "xy" [] $ (x ~> y) ~> (x ~> y)),
  ("map",   forall "xy" [] $ (x ~> y) ~> (lst x ~> lst y)),
  ("zip",   forall "xyz" [] $ (x ~> y ~> z) ~> (lst x ~> lst y ~> lst z)),
  ("fix",   forall "x" [] $ (x ~> x) ~> x),
  ("fixp",  forall "x" [con x] $ (x ~> x) ~> x ~> x),
  
  -- Combinators
  ("hook",  forall "xyz" [] $ (x ~> y ~> z) ~> (x ~> y) ~> x ~> z),
  ("const", forall "xy" [] $ x ~> y ~> x),
  ("id",    forall "x" [] $ x ~> x),

  -- Boolean functions and comparisons
  ("lt",    forall "x" [con x] $ x ~> x ~> int),
  ("gt",    forall "x" [con x] $ x ~> x ~> int),
  ("eq",    forall "x" [con x] $ x ~> x ~> int),
  ("if",    forall "xy" [con x] $ x ~> y ~> y ~> y)
  ]
