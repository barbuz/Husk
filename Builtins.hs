module Builtins where

import Expr

-- Compute command from char
cmd :: Char -> Exp [Lit]
cmd char | Just exp <- lookup char commandsList = exp
cmd char = error $ "No builtin bound to character " ++ [char]

-- List of commands
commands :: String
commands = map fst commandsList

-- Assoc list of commands that can occur in source
commandsList :: [(Char, Exp [Lit])]
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
  ('?', bins "if")
  ]

-- Compute builtins from space-delimited list
bins :: String -> Exp [Lit]
bins names = ELit $ map getBuiltin $ words names
  where getBuiltin name | Just typ <- lookup name builtinsList = Lit name typ
        getBuiltin name = error $ "No builtin named " ++ name

-- Assoc list of builtins
builtinsList :: [(String, Scheme)]
builtinsList = [

  -- Arithmetic
  ("add",   Scheme ["n"] $ CType [(Number, TVar "n")] $ TVar "n" ~> TVar "n" ~> TVar "n"),
  ("addID", Scheme [] $ CType [] $ TConc TInt ~> TConc TDouble ~> TConc TDouble),
  ("addDI", Scheme [] $ CType [] $ TConc TDouble ~> TConc TInt ~> TConc TDouble),
  ("sub",   Scheme ["n"] $ CType [(Number, TVar "n")] $ TVar "n" ~> TVar "n" ~> TVar "n"),
  ("subID", Scheme [] $ CType [] $ TConc TInt ~> TConc TDouble ~> TConc TDouble),
  ("subDI", Scheme [] $ CType [] $ TConc TDouble ~> TConc TInt ~> TConc TDouble),
  ("mul",   Scheme ["n"] $ CType [(Number, TVar "n")] $ TVar "n" ~> TVar "n" ~> TVar "n"),
  ("mulID", Scheme [] $ CType [] $ TConc TInt ~> TConc TDouble ~> TConc TDouble),
  ("mulDI", Scheme [] $ CType [] $ TConc TDouble ~> TConc TInt ~> TConc TDouble),
  ("neg",   Scheme ["n"] $ CType [(Number, TVar "n")] $ TVar "n" ~> TVar "n"),

  -- List manipulation
  ("pure",  Scheme ["x"] $ CType [] $ TVar "x" ~> TList (TVar "x")),
  ("pair",  Scheme ["x"] $ CType [] $ TVar "x" ~> TVar "x" ~> TList (TVar "x")),
  ("cons",  Scheme ["x"] $ CType [] $ TVar "x" ~> TList (TVar "x") ~> TList (TVar "x")),
  ("cat",   Scheme ["x"] $ CType [] $ TList (TVar "x") ~> TList (TVar "x") ~> TList (TVar "x")),
  ("snoc",  Scheme ["x"] $ CType [] $ TList (TVar "x") ~> TVar "x" ~> TList (TVar "x")),

  -- Higher order functions
  ("com3",  Scheme ["x", "y", "z", "u", "v"] $ CType [] $
            (TVar "u" ~> TVar "v") ~>
            (TVar "x" ~> TVar "y" ~> TVar "z" ~> TVar "u") ~>
            (TVar "x" ~> TVar "y" ~> TVar "z" ~> TVar "v")),
  ("com2",  Scheme ["x", "y", "z", "u"] $ CType [] $
            (TVar "z" ~> TVar "u") ~>
            (TVar "x" ~> TVar "y" ~> TVar "z") ~>
            (TVar "x" ~> TVar "y" ~> TVar "u")),
  ("com",   Scheme ["x", "y", "z"] $ CType [] $
            (TVar "y" ~> TVar "z") ~>
            (TVar "x" ~> TVar "y") ~>
            (TVar "x" ~> TVar "z")),
  ("app",   Scheme ["x", "y"] $ CType [] $
            (TVar "x" ~> TVar "y") ~>
            (TVar "x" ~> TVar "y")),
  ("map",   Scheme ["x", "y"] $ CType [] $
            (TVar "x" ~> TVar "y") ~>
            (TList (TVar "x") ~> TList (TVar "y"))),
  ("zip",   Scheme ["x", "y", "z"] $ CType [] $
            (TVar "x" ~> TVar "y" ~> TVar "z") ~>
            (TList (TVar "x") ~> TList (TVar "y") ~> TList (TVar "z"))),
  ("fix",   Scheme ["x"] $ CType [] $ (TVar "x" ~> TVar "x") ~> TVar "x"),
  ("fixp",  Scheme ["x"] $ CType [(Concrete, TVar "x")] $
            (TVar "x" ~> TVar "x") ~> TVar "x" ~> TVar "x"),

  -- Boolean functions and comparisons
  ("lt",    Scheme ["x"] $ CType [(Concrete, TVar "x")] $
            TVar "x" ~> TVar "x" ~> TConc TInt),
  ("gt",    Scheme ["x"] $ CType [(Concrete, TVar "x")] $
            TVar "x" ~> TVar "x" ~> TConc TInt),
  ("eq",    Scheme ["x"] $ CType [(Concrete, TVar "x")] $
            TVar "x" ~> TVar "x" ~> TConc TInt),
  ("if",    Scheme ["x", "y"] $ CType [(Concrete, TVar "x")] $
            TVar "x" ~> TVar "y" ~> TVar "y" ~> TVar "y")
  ]
