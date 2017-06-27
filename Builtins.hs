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

con :: Type -> TClass
con = Concrete

num :: Type -> TClass
num = Number

vec :: Type -> Type -> Type -> Type -> TClass
vec = Vect

forall :: String -> [TClass] -> Type -> Scheme
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
  ('-', bins "sub subDI subID diffl del"),
  ('*', bins "mul mulDI mulID"),
  ('/', bins "div"),
  ('÷', bins "idiv"),
  ('%', bins "mod modDI modID"),
  ('_', bins "neg"),
  ('\\', bins "inv"),
  (';', bins "pure"),
  (',', bins "pair"),
  (':', bins "cons snoc"),
  ('m', bins "map mapr"),
  ('z', bins "zip"),
  ('F', bins "foldr foldr1"),
  ('Ḟ', bins "foldl foldl1"),
  ('G', bins "scanr scanr1"),
  ('Ġ', bins "scanl scanl1"),
  ('f', bins "filter select"),
  ('L', bins "len nlen"),
  ('#', bins "countf count"),
  ('N', bins "nats"),
  ('!', bins "index index2"),
  ('↑', bins "take take2 takew"),
  ('↓', bins "drop drop2 dropw"),
  ('↕', bins "span"),
  ('←', bins "head fst predN predC"),
  ('→', bins "last snd succN succC"),
  ('↔', bins "swap rev"),
  ('h', bins "init"),
  ('t', bins "tail"),
  ('ƒ', bins "fix"),
  ('ω', bins "fixp"),
  ('<', bins "lt"),
  ('>', bins "gt"),
  ('≤', bins "le"),
  ('≥', bins "ge"),
  ('=', bins "eq"),
  ('≠', bins "neq"),
  ('?', bins "if if2 fif"),
  ('¬', bins "not"),
  ('|', bins "or or'"),
  ('&', bins "and and'"),
  ('S', bins "hook"),
  ('Ṡ', bins "hookf"),
  ('K', bins "const"),
  ('I', bins "id"),
  ('`', bins "flip"),
  ('Γ', bins "list listN listF listNF"),
  ('Σ', bins "sum trianI trianD concat"),
  ('Π', bins "prod fact cartes"),
  ('§', bins "fork fork2"),
  ('‡', bins "argdup"),
  ('∞', bins "rep"),
  ('¡', bins "iter"),
  ('c', bins "chr ord"),
  ('s', bins "show"),
  ('r', bins "read"),
  ('ø', bins "empty"),
  ('€', bins "elem subl"),
  ('o', bins "com com2 com3 com4"),
  ('ȯ', EAbs "x" $ EAbs "y" $ EAbs "z" $
        EOp (bins "com com2 com3 com4") (EVar "x") $
        EOp (bins "com com2 com3 com4") (EVar "y") (EVar "z")),
  ('ö', EAbs "x" $ EAbs "y" $ EAbs "z" $ EAbs "u" $
        EOp (bins "com com2 com3 com4") (EVar "x") $
        EOp (bins "com com2 com3 com4") (EVar "y") $
        EOp (bins "com com2 com3 com4") (EVar "z") (EVar "u")),
  ('¨', bins "vec"),
  ('O', bins "sort"),
  ('Ö', bins "sorton sortby"),
  ('▲', bins "max maxl"),
  ('▼', bins "min minl"),
  ('u', bins "nub"),
  ('ü', bins "nubon nubby"),
  ('U', bins "nubw"),
  ('w', bins "words unwords"),
  ('¶', bins "lines unlines"),
  ('p', bins "pfac"),
  ('~', bins "subs subs2"),
  ('g', bins "group"),
  ('ġ', bins "groupOn groupBy"),
  ('ḣ', bins "heads"),
  ('ṫ', bins "tails"),
  ('¦', bins "divds"),
  ('P', bins "perms"),
  ('V', bins "any"),
  ('Λ', bins "all"),
  ('T', bins "trsp trspw"),
  ('ż', bins "zip'"),
  ('ṁ', bins "cmap cmapr smap smapr"),
  ('≡', bins "congr"),
  ('¤', bins "combin"),
  ('i', bins "d2i c2i s2i"),
  ('e', bins "list2"),
  ('ė', bins "list3"),
  ('ë', bins "list4"),
  ('Ṫ', bins "table"),
  ('Ṁ', bins "rmap"),
  ('M', bins "lmap"),
  ('«', bins "mapacL"),
  ('»', bins "mapacR"),
  ('R', bins "replic")
  ]

-- Compute builtins from space-delimited list
bins :: String -> Exp [Lit Scheme]
bins names = ELit $ map getBuiltin $ words names
  where getBuiltin "vec" = Vec $ forall "xyuv" [vec x y u v] $ (x ~> y) ~> (u ~> v)
        getBuiltin name | Just typ <- lookup name builtinsList = Builtin name typ
        getBuiltin name = error $ "No builtin named " ++ name

-- Assoc list of builtins
builtinsList :: [(String, Scheme)]
builtinsList = [

  ("intseq", simply $ chr ~> lst int),

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
  ("predN", forall "n" [num n] $ n ~> n),
  ("succN", forall "n" [num n] $ n ~> n),
  ("pfac",  simply $ int ~> lst int),
  ("divds", forall "n" [num n] $ n ~> n ~> int),

  -- List and pair manipulation
  ("empty", forall "x" [] $ lst x),
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
  ("indexC",forall "x" [con x] $ int ~> lst x ~> x),
  ("indexC2",forall "x" [con x] $ lst x ~> int ~> x),
  ("index", forall "x" [] $ int ~> lst x ~> x),
  ("index2",forall "x" [] $ lst x ~> int ~> x),
  ("take",  forall "x" [] $ int ~> lst x ~> lst x),
  ("take2",  forall "x" [] $ lst x ~> int ~> lst x),
  ("takew", forall "xy" [con y] $ (x ~> y) ~> lst x ~> lst x),
  ("drop",  forall "x" [] $ int ~> lst x ~> lst x),
  ("drop2",  forall "x" [] $ lst x ~> int ~> lst x),
  ("dropw", forall "xy" [con y] $ (x ~> y) ~> lst x ~> lst x),
  ("span",  forall "xy" [con y] $ (x ~> y) ~> lst x ~> tup (lst x) (lst x)),
  ("rev",   forall "x" [] $ lst x ~> lst x),
  ("heads", forall "x" [con x] $ x ~> lst x),
  ("tails", forall "x" [con x] $ x ~> lst x),
  ("nats",  forall "n" [num n] $ lst n),
  ("concat",forall "x" [] $ lst (lst x) ~> lst x),
  ("sum",   forall "n" [num n] $ lst n ~> n),
  ("prod",  forall "n" [num n] $ lst n ~> n),
  ("cartes",forall "x" [] $ lst (lst x) ~> lst (lst x)),
  ("elem",  forall "x" [con x] $ lst x ~> x ~> int),
  ("sort",  forall "x" [con x] $ lst x ~> lst x),
  ("sorton",forall "xy" [con y] $ (x ~> y) ~> lst x ~> lst x),
  ("sortby",forall "x" [] $ (x ~> x ~> int) ~> lst x ~> lst x),
  ("maxl",  forall "x" [con x] $ lst x ~> x),
  ("minl",  forall "x" [con x] $ lst x ~> x),
  ("diffl", forall "x" [con x] $ lst x ~> lst x ~> lst x),
  ("del",   forall "x" [con x] $ x ~> lst x ~> lst x),
  ("nub",   forall "x" [con x] $ lst x ~> lst x),
  ("nubon", forall "xy" [con y] $ (x ~> y) ~> lst x ~> lst x),
  ("nubby", forall "xy" [con y] $ (x ~> x ~> y) ~> lst x ~> lst x),
  ("nubw",  forall "x" [con x] $ lst x ~> lst x),
  ("subs",  forall "x" [con x] $ x ~> x ~> lst x ~> lst x),
  ("subs2", forall "x" [con x] $ lst x ~> lst x ~> lst x ~> lst x),
  ("group", forall "x" [con x] $ lst x ~> lst (lst x)),
  ("groupOn",forall "xy" [con y] $ (x ~> y) ~> lst x ~> lst (lst x)),
  ("groupBy",forall "xy" [con y] $ (x ~> x ~> y) ~> lst x ~> lst (lst y)), 
  ("perms", forall "x" [] $ lst x ~> lst (lst x)),
  ("trsp",  forall "x" [] $ lst (lst x) ~> lst (lst x)),
  ("trspw", forall "x" [] $ x ~> lst (lst x) ~> lst (lst x)),
  ("list2", forall "x" [] $ x ~> x ~> lst x),
  ("list3", forall "x" [] $ x ~> x ~> x ~> lst x),
  ("list4", forall "x" [] $ x ~> x ~> x ~> x ~> lst x),
  ("replic",forall "x" [] $ int ~> x ~> lst x),
  

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
  ("iter",  forall "x" [] $ (x ~> x) ~> x ~> lst x),
  ("rep",   forall "x" [] $ x ~> lst x),
  ("zip'",  forall "x" [] $ (x ~> x ~> x) ~> lst x ~> lst x ~> lst x),
  ("cmap",  forall "xy" [] $ (x ~> lst y) ~> lst x ~> lst y),
  ("smap",  forall "xn" [num n] $ (x ~> n) ~> lst x ~> n),
  ("cmapr", forall "xy" [] $ lst (x ~> lst y) ~> x ~> lst y),
  ("smapr", forall "xn" [num n] $ lst (x ~> n) ~> x ~> n),
  ("table", forall "xyz" [] $ (x ~> y ~> z) ~> lst x ~> lst y ~> lst (lst z)),
  ("rmap",  forall "xyz" [] $ (x ~> y ~> z) ~> x ~> lst y ~> lst z),
  ("lmap",  forall "xyz" [] $ (x ~> y ~> z) ~> lst x ~> y ~> lst z),
  ("mapacL",forall "xyz" [] $ (x ~> y ~> x) ~> (x ~> y ~> z) ~> x ~> lst y ~> lst z),
  ("mapacR",forall "xyz" [] $ (y ~> x ~> x) ~> (y ~> x ~> z) ~> x ~> lst y ~> lst z),
  
  -- Combinators
  ("hook",  forall "xyz" [] $ (x ~> y ~> z) ~> (x ~> y) ~> x ~> z),
  ("hookf", forall "xyz" [] $ (x ~> y ~> z) ~> (y ~> x) ~> y ~> z),
  ("const", forall "xy" [] $ x ~> y ~> x),
  ("id",    forall "x" [] $ x ~> x),
  ("fix",   forall "x" [] $ (x ~> x) ~> x),
  ("flip",  forall "xyz" [] $ (x ~> y ~> z) ~> (y ~> x ~> z)),
  ("com4",  forall "xyzuvw" [] $ (v ~> w) ~> (x ~> y ~> z ~> u ~> v) ~> (x ~> y ~> z ~> u ~> w)),
  ("com3",  forall "xyzuv" [] $ (u ~> v) ~> (x ~> y ~> z ~> u) ~> (x ~> y ~> z ~> v)),
  ("com2",  forall "xyzu" [] $ (z ~> u) ~> (x ~> y ~> z) ~> (x ~> y ~> u)),
  ("com",   forall "xyz" [] $ (y ~> z) ~> (x ~> y) ~> (x ~> z)),
  ("app",   forall "xy" [] $ (x ~> y) ~> x ~> y),
  ("fork",  forall "xyzu" [] $ (x ~> y ~> z) ~> (u ~> x) ~> (u ~> y) ~> u ~> z),
  ("fork2", forall "xyzuv" [] $ (x ~> y ~> z) ~> (u ~> v ~> x) ~> (u ~> v ~> y) ~> u ~> v ~> z),
  ("argdup",forall "xy" [] $ (x ~> x ~> y) ~> x ~> y),
  ("combin",forall "xyz" [] $ (y ~> y ~> z) ~> (x ~> y) ~> (x ~> x ~> z)),

  -- Boolean functions and comparisons
  ("lt",    forall "x" [con x] $ x ~> x ~> int),
  ("gt",    forall "x" [con x] $ x ~> x ~> int),
  ("le",    forall "x" [con x] $ x ~> x ~> int),
  ("ge",    forall "x" [con x] $ x ~> x ~> int),
  ("eq",    forall "x" [con x] $ x ~> x ~> int),
  ("neq",   forall "x" [con x] $ x ~> x ~> int),
  ("if",    forall "xy" [con x] $ y ~> y ~> x ~> y),
  ("if2",   forall "xy" [con x] $ (x ~> y) ~> y ~> x ~> y),
  ("fif",   forall "xyz" [con x] $ (z ~> y) ~> (z ~> y) ~> (z ~> x) ~> z ~> y),
  ("not",   forall "x" [con x] $ x ~> int),
  ("fnot",  forall "xy" [con y] $ (x ~> y) ~> (x ~> int)),
  ("or",    forall "x" [con x] $ x ~> x ~> x),
  ("or'",   forall "x" [con x, con y, num n] $ x ~> y ~> n),
  ("and",   forall "x" [con x] $ x ~> x ~> x),
  ("and'",  forall "x" [con x, con y, num n] $ x ~> y ~> n),
  ("max",   forall "x" [con x] $ x ~> x ~> x),
  ("min",   forall "x" [con x] $ x ~> x ~> x),
  ("any",   forall "xy" [con y] $ (x ~> y) ~> lst x ~> y),
  ("all",   forall "xy" [con y] $ (x ~> y) ~> lst x ~> y),
  ("subl",  forall "x" [con x] $ lst x ~> lst x ~> int),
  ("congr", forall "x" [con x] $ x ~> x ~> int),
  
  -- Chars and strings
  ("chr",   simply $ int ~> chr),
  ("ord",   simply $ chr ~> int),
  ("predC", simply $ chr ~> chr),
  ("succC", simply $ chr ~> chr),
  ("show",  forall "x" [con x] $ x ~> lst chr),
  ("read",  forall "x" [con x] $ lst chr ~> x),
  ("words", simply $ lst chr ~> lst (lst chr)),
  ("unwords", simply $ lst (lst chr) ~> lst chr),
  ("lines", simply $ lst chr ~> lst (lst chr)),
  ("unlines", simply $ lst (lst chr) ~> lst chr),
  
  -- Type conversions
  ("d2i",   simply $ dbl ~> int),
  ("c2i",   simply $ chr ~> int),
  ("s2i",   simply $ lst chr ~> int)
  ]
