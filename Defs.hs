{-# LANGUAGE UndecidableInstances, FlexibleInstances, FlexibleContexts, BangPatterns #-}

module Defs where
-- Built-in functions

import Data.Function (fix)
import qualified Data.Char as C
import           Data.Char (chr,ord)
import Data.List
import qualified Data.Set as S (member, insert, singleton)
import Data.Ord (comparing)
import Data.Bits ((.&.), (.|.))
import Data.Ratio ((%), numerator, denominator)

import Numeric (showFFloat)

-- Type of numeric values: integer fractions and Doubles
--  1:%0 is infinity
-- -1:%0 is negative infinity
--  0:%0 is Any
data TNum = !Integer :% !Integer
          | TDbl !Double

-- Convert to Double
doublify :: TNum -> Double
doublify (p :% q) = fromInteger p / fromInteger q
doublify (TDbl a) = a

instance Eq TNum where
  p :% 0 == q :% 0 = p == q
  p :% q == r :% s = p*s == q*r
  x == y           = doublify x == doublify y

instance Ord TNum where
  compare (p :% 0) (r :% 0) = compare p r
  compare (p :% q) (r :% s) = compare (p*s) (q*r)
  compare x y               = compare (doublify x) (doublify y)

boolToNum :: Bool -> TNum
boolToNum True = 1
boolToNum False = 0

-- Instances for TNum
instance Show TNum where
  show (p :% 1)    = show p
  show (1 :% 0)    = "Inf"
  show (0 :% 0)    = "Any"
  show ((-1) :% 0) = "-Inf"
  show (p :% q)    = show p ++ "/" ++ show q
  show (TDbl d) = showFFloat Nothing d ""

instance Read TNum where
  readsPrec n str
    | p@(_:_) <- tryInt,
      x@(_:_) <- [(k, q) | (k, '/':rest) <- p, q <- readsPrec n rest]
    = [(cancel $ k :% m, rest2) | (k, (m, rest2)) <- x]
    | p@(_:_) <- tryInt           = [(k :% 1, rest) | (k, rest) <- p]
    | p@(_:_) <- tryDbl           = [(TDbl k, rest) | (k, rest) <- p]
    | 'I':'n':'f':rest <- str     = [(1 :% 0, rest)]
    | 'A':'n':'y':rest <- str     = [(0 :% 0, rest)]
    | '-':'I':'n':'f':rest <- str = [((-1) :% 0, rest)]
    | otherwise                   = []
    where tryInt = readsPrec n str :: [(Integer, String)]
          tryDbl = readsPrec n str :: [(Double, String)]

-- Simplify a fraction
cancel :: TNum -> TNum
cancel (p :% 0) = signum p :% 0
cancel (p :% q)
  | k <- signum q * p,
    n <- abs q,
    r <- gcd k n
  = div k r :% div n r
cancel a = a

-- Create a binary numeric operator
-- operate f (!) applies f to fractions and (!) to Doubles,
-- converting fractions to Doubles when necessary
operate :: (Integer -> Integer -> Integer -> Integer -> (Integer, Integer)) -> (Double -> Double -> TNum) -> TNum -> TNum -> TNum
operate f _ (p :% q) (r :% s) | (x, y) <- f p q r s = cancel $ x :% y
operate _ (!) a b = doublify a ! doublify b
          
instance Num TNum where
  (+) = operate (\p q r s -> (p*s + r*q, q*s)) ((TDbl .) . (+))

  (-) = operate (\p q r s -> (p*s - r*q, q*s)) ((TDbl .) . (-))
  
  (*) = operate (\p q r s -> (p*r, q*s)) ((TDbl .) . (*))

  abs (p :% q) = abs p :% q
  abs (TDbl a) = TDbl $ abs a

  signum (p :% _) = signum p :% 1
  signum (TDbl a) = round (signum a) :% 1

  negate (p :% q) = negate p :% q
  negate (TDbl a) = TDbl $ negate a

  fromInteger = (:% 1)

instance Real TNum where
  toRational (p :% q) = p % q
  toRational (TDbl a) = toRational a

instance Enum TNum where
  toEnum n = toEnum n :% 1
  
  fromEnum (p :% q) = fromEnum $ p % q
  fromEnum (TDbl n) = fromEnum n
  
  succ = (+1)
  pred = (-1+)
  
  enumFrom = iterate succ
  enumFromThen a b = iterate (+(b-a)) a
  enumFromTo a c = case compare a c of
    GT -> []
    EQ -> [a]
    LT -> takeWhile (<= c) $ iterate succ a
  enumFromThenTo a b c = case (compare a c, compare a b) of
    (GT, GT) -> takeWhile (>= c) $ iterate (+(b-a)) a
    (GT, _)  -> []
    (EQ, GT) -> [a]
    (EQ, EQ) -> repeat a
    (EQ, LT) -> [a]
    (LT, GT) -> []
    (LT, EQ) -> repeat a
    (LT, LT) -> takeWhile (<= c) $ iterate (+(b-a)) a

instance Integral TNum where
  toInteger (p :% q) = div p q
  toInteger (TDbl d) = truncate d
  
  quotRem a@(_ :% _) b@(_ :% _)
    | d@(p :% q) <- a / b,
      k <- div p q :% 1
    = if q == 0
      then (d, a * signum d)
      else (k, a - b*k)
  quotRem a b
    | x <- doublify a,
      y <- doublify b,
      r <- truncate $ x / y
    = (r :% 1, TDbl $ x - y * fromInteger r)

instance Fractional TNum where
  fromRational r = numerator r :% denominator r

  (/) = operate
          (\p q r s -> (p*s, q*r))
          (\x y -> if y == 0
                   then round (signum x) :% 0
                   else TDbl $ x/y)

-- Lift a numeric function to TNums
-- The extra arguments are results for Inf and -Inf
numeric :: (Double -> Double) -> TNum -> TNum -> (TNum -> TNum)
numeric f pinf ninf = g
  where g (1 :% 0)    = pinf
        g (0 :% 0)    = 0 :% 0
        g ((-1) :% 0) = ninf
        g x           = TDbl $ f $ doublify x
  
instance Floating TNum where
  pi = TDbl pi

  exp = numeric exp (1 :% 0) 0
  log = numeric log (1 :% 0) 0
  sqrt = numeric sqrt (1 :% 0) ((-1) :% 0)

  sin = numeric sin 0 0
  cos = numeric cos 0 0
  tan = numeric tan 0 0

  asin = numeric asin 0 0
  acos = numeric acos 0 0
  atan = numeric atan (pi/2) (-pi/2)

  sinh = numeric sinh (1 :% 0) ((-1) :% 0)
  cosh = numeric cosh (1 :% 0) (1 :% 0)
  tanh = numeric tanh 1 (-1)

  asinh = numeric asinh (1 :% 0) ((-1) :% 0)
  acosh = numeric acosh (1 :% 0) 0
  atanh = numeric atanh 0 0

instance RealFrac TNum where
  properFraction a@(_ :% 0) = (0, a)
  properFraction (p :% q) | r <- quot p q = (fromInteger r, (p - r) :% q)
  properFraction (TDbl a) | (n, r) <- properFraction a = (n, TDbl r)

-- Class of all Husk types (used for defaulting)

class Husky a where
  defVal :: a

instance Husky TNum where
  defVal = 0

instance Husky Char where
  defVal = ' '

instance (Husky a) => Husky [a] where
  defVal = []

instance (Husky a, Husky b) => Husky (a,b) where
  defVal = (defVal, defVal)

instance (Husky b) => Husky (a -> b) where
  defVal = const defVal

-- String conversion
class ToString a where
  toString :: a -> String

instance {-# OVERLAPPING #-} ToString String where
  toString = id

instance {-# OVERLAPPING #-} ToString [String] where
  toString = unlines

instance {-# OVERLAPPING #-} ToString [[String]] where
  toString = unlines.map unwords

instance Concrete a => ToString a where
  toString = show


-- Class of concrete values
class (Husky a, Show a, Read a, Eq a, Ord a, ToString a) => Concrete a where
  isTruthy :: a -> Bool
  toTruthy :: a -> TNum
  func_false :: a
  func_false = defVal
  func_true :: a
  func_lt :: a -> a -> TNum
  func_gt :: a -> a -> TNum
  func_le :: a -> a -> TNum
  func_ge :: a -> a -> TNum
  func_neq :: a -> a -> TNum
  func_congr :: a -> a -> TNum
  func_simil :: a -> a -> TNum
  
  func_maxval :: a
  func_minval :: a
  
  func_heads :: a -> [a]
  func_tails :: a -> [a]
  
  func_eq :: a -> a -> TNum
  func_eq x y = boolToNum $ x == y
  
  func_or :: a -> a -> a
  func_or y x = if isTruthy x then x else y
  
  func_and :: a -> a -> a
  func_and y x = if isTruthy x then y else x
  
  func_read :: [Char] -> a
  func_read x | ((val, _):_) <- reads x = val
              | otherwise = defVal

func_or' :: (Concrete a, Concrete b) => a -> b -> TNum
func_or' x y = func_or (toTruthy x) (toTruthy y)

func_and' :: (Concrete a, Concrete b) => a -> b -> TNum
func_and' x y = func_and (toTruthy x) (toTruthy y)

instance Concrete TNum where
  isTruthy = (/= 0)
  toTruthy (TDbl d) = roundAway d :% 1
  toTruthy n = n
  func_true = 1
  func_lt y x = max 0 $ toTruthy (y-x)
  func_gt y x = max 0 $ toTruthy (x-y)
  func_le y x | x > y = 0
              | otherwise = toTruthy $ y-x+1
  func_ge y x | x < y = 0
              | otherwise = toTruthy $ x-y+1
  func_neq y x = abs $ toTruthy (x-y)
  
  func_maxval = 1 :% 0
  func_minval = (-1) :% 0
  
  func_congr 0 0 = 1
  func_congr 0 _ = 0
  func_congr _ 0 = 0
  func_congr _ _ = 1
  
  func_simil x y | abs (x-y) <= 1 = 1
                 | otherwise      = 0
  
  func_heads x | x >= 0    = [1 .. x]
               | otherwise = [x .. -1]
  func_tails x | x >= 0    = [x, x-1 .. 1]
               | otherwise = [-1, -2 .. x]

instance Concrete Char where
  isTruthy = not . C.isSpace
  toTruthy = boolToNum.isTruthy
  func_true = '!'
  func_lt y x = fromIntegral $ max 0 (ord y - ord x)
  func_gt y x = fromIntegral $ max 0 (ord x - ord y)
  func_le y x = fromIntegral $ max 0 (ord y - ord x + 1)
  func_ge y x = fromIntegral $ max 0 (ord x - ord y + 1)
  func_neq y x = abs.fromIntegral $ (ord x)-(ord y)
  
  func_maxval = maxBound
  func_minval = minBound
      
  func_congr x y | isTruthy x == isTruthy y = 1
                 | otherwise                = 0
  
  func_simil x y | x==y || x == succ y || y == succ x = 1
                 | otherwise                  = 0
  
  func_heads x = ['\0'..x]
  func_tails x = [x, pred x..'\0']

instance Concrete a => Concrete [a] where
  isTruthy = (/= [])
  toTruthy = genericLength
  func_true = [func_true]
  func_lt = go 1
    where go n (_:_) [] = n
          go n (y:ys) (x:xs) | x < y = n
                             | x > y = 0
                             | otherwise = go (n+1) ys xs
          go _ _ _ = 0
  func_gt x y = func_lt y x
  func_le = go 1
    where go n _ [] = n
          go n (y:ys) (x:xs) | x < y = n
                             | x > y = 0
                             | otherwise = go (n+1) ys xs
          go _ _ _ = 0
  func_ge x y = func_le y x
  func_neq = go 1
    where go n [] [] = 0
          go n (x:xs) (y:ys) | x /= y = n
                             | otherwise = go (n+1) xs ys
          go n _ _ = n
  
  func_maxval = repeat func_maxval
  func_minval = []
  
  func_congr [] [] = 1
  func_congr [] _  = 0
  func_congr _  [] = 0
  func_congr (x:xs) (y:ys) = if func_congr x y == 0 then 0 else func_congr xs ys
  
  func_simil (x:xs) (y:ys) = func_simil xs ys
  func_simil []     []     = 1
  func_simil _      _      = 0
  
  func_heads=tail.inits
  func_tails=init.tails

instance (Concrete a, Concrete b) => Concrete (a, b) where
  isTruthy (x, y) = isTruthy x && isTruthy y
  toTruthy (x, y) = toTruthy x * toTruthy y
  func_true = (func_true, func_true)
  func_lt (x, y) (x', y') = if x == x' then func_lt y y' else func_lt x x'
  func_gt (x, y) (x', y') = if x == x' then func_gt y y' else func_gt x x'
  func_le (x, y) (x', y') = if x > x' then func_lt y y' else func_le x x'
  func_ge (x, y) (x', y') = if x < x' then func_gt y y' else func_ge x x'
  func_neq (x, y) (x', y') = if x == x' then func_neq y y' else func_neq x x'
  
  func_maxval = (func_maxval, func_maxval)
  func_minval = (func_minval, func_minval)
  
  func_congr (a,b) (c,d) = if func_congr a c + func_congr b d == 2 then 1 else 0
  func_simil (a,b) (c,d) = if func_simil a c + func_simil b d == 2 then 1 else 0
  
  func_heads (a,b) = [(c,d)|c<-func_heads a,d<-func_heads b]
  func_tails (a,b) = [(c,d)|c<-func_tails a,d<-func_tails b]

roundAway :: Double -> Integer
roundAway d = if d<0 then floor d else ceiling d

--Primes (quite efficient implementation, but not the most efficient)
primes_list = 2 : oddprimes
  where
    oddprimes = sieve [3,5..] 9 oddprimes
    sieve (x:xs) q ps@ ~(p:t)
      | x < q     = x : sieve xs q ps
      | otherwise =     sieve (xs `minus` [q, q+2*p..]) (head t^2) t
    minus (x:xs) (y:ys) = case (compare x y) of
           LT -> x : minus  xs  (y:ys)
           EQ ->     minus  xs     ys
           GT ->     minus (x:xs)  ys
    minus  xs     _     = xs


-- Built-in functions

func_fix :: (a -> a) -> a
func_fix = fix

func_fixp :: Concrete a => (a -> a) -> a -> a
func_fixp f a = go (S.singleton a) $ f a
  where go xs x | x `S.member` xs = x
                | otherwise       = go (S.insert x xs) $ f x

func_fixpL :: Concrete a => (a -> [a]) -> a -> [a]
func_fixpL f a = cs
  where f' = concatMap f
        b = func_fixp (take 1 . f') [a]
        n = succ $ length $ takeWhile (/= b) $ tail $ iterate (take 1 . f') b
        f'' = foldr1 (.) $ replicate n f'
        cs = b ++ tail (f'' cs)

func_app :: (a -> b) -> a -> b
func_app = id

func_com :: (b -> c) -> (a -> b) -> a -> c
func_com = (.)

func_com2 :: (c -> d) -> (a -> b -> c) -> a -> b -> d
func_com2 f g x = f . g x

func_com3 :: (d -> e) -> (a -> b -> c -> d) -> a -> b -> c -> e
func_com3 f g x y = f . g x y

func_com4 :: (e -> f) -> (a -> b -> c -> d -> e) -> a -> b -> c -> d -> f
func_com4 f g x y z = f . g x y z

func_add :: TNum -> TNum -> TNum
func_add = (+)

func_sub :: TNum -> TNum -> TNum
func_sub b a = a - b

func_mul :: TNum -> TNum -> TNum
func_mul = (*)

func_div :: TNum -> TNum -> TNum
func_div b a = a / b

func_idiv :: TNum -> TNum -> TNum
func_idiv b a = a `div` b

func_mod :: TNum -> TNum -> TNum
func_mod b a = a `mod` b

func_divds :: TNum -> TNum -> TNum
func_divds b a =
  if func_mod b a == 0
  then if a == 0
       then 1
       else func_idiv b a
  else 0

func_neg :: TNum -> TNum
func_neg x = -x

func_inv :: TNum -> TNum
func_inv = recip

-- Triangular numbers: sum of all numbers in [1..n]
func_trian :: TNum -> TNum
func_trian (p :% 1) = div (p*(p+1)) 2 :% 1
func_trian r        = r*(r+1)/2

func_fact :: TNum -> TNum
func_fact n = product [1..n]

func_pure :: a -> [a]
func_pure = (: [])

func_cons :: a -> [a] -> [a]
func_cons = (:)

func_snoc :: [a] -> a -> [a]
func_snoc x y = x ++ [y]

func_cat :: [a] -> [a] -> [a]
func_cat = (++)

func_head :: (Husky a) => [a] -> a
func_head [] = defVal
func_head (x:_) = x

func_last :: (Husky a) => [a] -> a
func_last [] = defVal
func_last xs = last xs

func_tail :: [a] -> [a]
func_tail [] = []
func_tail (_:xs) = xs

func_init :: [a] -> [a]
func_init [] = []
func_init xs = init xs

func_pair :: a -> b -> (a, b)
func_pair = (,)

func_swap :: (a,b) -> (b,a)
func_swap (x,y) = (y,x)

func_fst :: (a,b) -> a
func_fst = fst

func_snd :: (a,b) -> b
func_snd = snd

func_map :: (a -> b) -> [a] -> [b]
func_map = map

func_mapr :: [(a -> b)] -> a -> [b]
func_mapr fs a = [f a | f<-fs]

func_zip :: (a -> b -> c) -> [a] -> [b] -> [c]
func_zip = zipWith

func_filter :: Concrete b => (a -> b) -> [a] -> [a]
func_filter f = filter $ isTruthy . f

func_select :: Concrete a => [a] -> [b] -> [b]
func_select x y = [b | (a, b) <- zip x y, isTruthy a]

func_scanl :: (b -> a -> b) -> b -> [a] -> [b]
func_scanl = scanl

func_scanl1 :: (a -> a -> a) -> [a] -> [a]
func_scanl1 = scanl1

func_scanr :: (a -> b -> b) -> b -> [a] -> [b]
func_scanr = scanr

func_scanr1 :: (a -> a -> a) -> [a] -> [a]
func_scanr1 = scanr1

func_len :: [a] -> TNum
func_len = genericLength

func_nlen :: TNum -> TNum
func_nlen = genericLength.show

func_countf :: Concrete b => (a -> b) -> [a] -> TNum
func_countf c = genericLength . filter (isTruthy . c)

func_count :: Concrete a => a -> [a] -> TNum
func_count x = genericLength . filter (== x)

func_count' :: Concrete a => [a] -> a -> TNum
func_count' = flip func_count

func_index :: (Husky a) => TNum -> [a] -> a
func_index _ [] = defVal
func_index i xs
  | (1 :% 0) <- i    = last xs
  | (0 :% 0) <- i    = defVal
  | ((-1) :% 0) <- i = head xs
  | toInteger i>0    = genericIndex (cycle xs) $ toInteger i-1
  | otherwise        = genericIndex (cycle $ reverse xs) $ -toInteger i

func_index2 :: (Husky a) => [a] -> TNum -> a
func_index2 = flip func_index

func_rev :: [a] -> [a]
func_rev = reverse

func_nats :: [TNum]
func_nats = [1, 2 ..]

func_sum :: [TNum] -> TNum
func_sum = sum

func_prod :: [TNum] -> TNum
func_prod = product

func_concat :: [[a]] -> [a]
func_concat = concat

func_cartes :: [[a]] -> [[a]]
func_cartes [] = [[]]
func_cartes (x:xs) = func_mix (:) x $ func_cartes xs

func_mix :: (a -> b -> c) -> [a] -> [b] -> [c]
func_mix f xs ys = concat $ func_adiags $ func_table f xs ys

-- Lazy merges
merge3 :: [a] -> [a] -> [a] -> [a]
merge3 (x:xs) ys zs = x : next ys
  where next (b:bs) = b : next' zs
          where next' (c:cs) = c : merge3 xs bs cs
                next' [] = merge2 xs ys
        next [] = merge2 xs zs
merge3 [] ys zs = merge2 ys zs

merge2 :: [a] -> [a] -> [a]
merge2 (x:xs) ys = x : next ys
  where next (y:ys) = y : merge2 xs ys
        next [] = xs
merge2 [] ys = ys

func_if :: Concrete a => b -> b -> a -> b
func_if b c a = if isTruthy a then b else c

func_if2 :: Concrete a => (a->b) -> b -> a -> b
func_if2 g c a = if isTruthy a then g a else c

func_fif :: Concrete a => (x->b) -> (x->b) -> (x->a) -> x -> b
func_fif g h f x = if isTruthy (f x) then g x else h x

func_not :: Concrete a => a -> TNum
func_not a = if isTruthy a then 0 else 1

func_fnot :: Concrete b => (a -> b) -> a -> TNum
func_fnot f = func_not . f

func_hook :: (a -> b -> c) -> (a -> b) -> a -> c
func_hook x y z = x z (y z)

func_hookf :: (a -> b -> c) -> (b -> a) -> b -> c
func_hookf x y z = x (y z) z

func_const :: a -> b -> a
func_const x _ = x

func_id :: a -> a
func_id x = x

func_flip :: (a -> b -> c) -> (b -> a -> c)
func_flip = flip

func_foldl :: (b -> a -> b) -> b -> [a] -> b
func_foldl = foldl

func_foldl1 :: (Husky a) => (a -> a -> a) -> [a] -> a
func_foldl1 _ [] = defVal
func_foldl1 f xs = foldl1 f xs

func_foldr :: (a -> b -> b) -> b -> [a] -> b
func_foldr = foldr

func_foldr1 :: (Husky a) => (a -> a -> a) -> [a] -> a
func_foldr1 _ [] = defVal
func_foldr1 f xs = foldr1 f xs


func_take :: TNum -> [a] -> [a]
func_take n
  | n >= 0    = genericTake n
  | otherwise = reverse . genericTake (-n) . reverse

func_take2 :: [a] -> TNum -> [a]
func_take2 = flip func_take

func_takew :: Concrete b => (a -> b) -> [a] -> [a]
func_takew _ [] = []
func_takew f (x:xs)
  | isTruthy(f x) = x : func_takew f xs
  | otherwise     = []

func_drop :: TNum -> [a] -> [a]
func_drop n
  | n >= 0    = genericDrop n
  | otherwise = reverse . genericDrop (-n) . reverse

func_drop2 :: [a] -> TNum -> [a]
func_drop2 = flip func_drop

func_dropw :: Concrete b => (a -> b) -> [a] -> [a]
func_dropw _ [] = []
func_dropw f (x:xs)
  | isTruthy(f x) = func_dropw f xs
  | otherwise     = x:xs

func_span :: Concrete b => (a -> b) -> [a] -> ([a],[a])
func_span f xs = go f ([],xs)
  where go f result@(hs,(t:ts)) | isTruthy(f t) = go f (hs++[t],ts)
                                | otherwise     = result
        go f (hs,[]) = (hs,[])

func_list :: b -> (a -> [a] -> b) -> [a] -> b
func_list c _ [] = c
func_list _ f (x:xs) = f x xs

func_listN :: (Husky b) => (a -> [a] -> b) -> [a] -> b
func_listN _ []     = defVal
func_listN f (x:xs) = f x xs

func_listF :: b -> (([a] -> b) -> (a -> [a] -> b)) -> [a] -> b
func_listF c f = go
  where go []     = c
        go (x:xs) = f go x xs

func_listNF :: (Husky b) => (([a] -> b) -> (a -> [a] -> b)) -> [a] -> b
func_listNF f = go
  where go []     = defVal
        go (x:xs) = f go x xs

func_fork :: (a -> b -> c) -> (x -> a) -> (x -> b) -> x -> c
func_fork f g h x = f (g x) (h x)

func_fork2 :: (a -> b -> c) -> (x -> y -> a) -> (x -> y -> b) -> x -> y -> c
func_fork2 f g h x y = f (g x y) (h x y)

func_argdup :: (a -> a -> b) -> a -> b
func_argdup f x = f x x

func_iter :: (a -> a) -> a -> [a]
func_iter = iterate

func_iterL :: (a -> [a]) -> [a] -> [a]
func_iterL f = go
  where go [] = []
        go xs = xs ++ go (concatMap f xs)

func_iterP :: ([a] -> a) -> [a] -> [a]
func_iterP f = \as -> as ++ go as
  where go xs | x <- f xs = x : go (xs ++ [x])

func_rep :: a -> [a]
func_rep = repeat

func_ord :: Char -> TNum
func_ord = fromIntegral.ord

func_chr :: TNum -> Char
func_chr = chr.fromInteger.toInteger

func_show :: Concrete a => a -> String
func_show = show

func_empty :: [a]
func_empty = []

func_predN :: TNum -> TNum
func_predN n = n-1

func_succN :: TNum -> TNum
func_succN n = n+1

func_predC :: Char -> Char
func_predC = pred

func_succC :: Char -> Char
func_succC = succ

func_elem :: Concrete a => [a] -> a -> TNum
func_elem xs x | Just i <- elemIndex x xs = fromIntegral i + 1
               | otherwise                = 0

func_elem' :: Concrete a => a -> [a] -> TNum
func_elem' = flip func_elem

func_sort :: Concrete a => [a] -> [a]
func_sort = sort

func_sorton :: Concrete b => (a -> b) -> [a] -> [a]
func_sorton = sortOn

-- f x y means x is greater then y
func_sortby :: Concrete b => (a -> a -> b) -> [a] -> [a]
func_sortby f xs = sortOn (\x -> length [y | y <- xs, isTruthy $ f x y]) xs

func_max :: Concrete a => a -> a -> a
func_max = max

func_min :: Concrete a => a -> a -> a
func_min = min

func_maxl :: Concrete a => [a] -> a
func_maxl = foldr max func_minval

func_minl :: Concrete a => [a] -> a
func_minl = foldr min func_maxval

func_del :: Concrete a => a -> [a] -> [a]
func_del = delete

func_diffl :: Concrete a => [a] -> [a] -> [a]
func_diffl [] xs = xs
--first option: multiset difference
func_diffl (y:ys) xs = func_diffl ys $ func_del y xs
--second option: filter elements (disregards multiciplities)
--func_diffl (y:ys) xs = func_diffl ys $ filter (/=y) xs

func_nub :: Concrete a => [a] -> [a]
func_nub = nub

func_nubon :: Concrete b => (a -> b) -> [a] -> [a]
func_nubon f = nubBy (\x y -> f x == f y)

func_nubby :: Concrete b => (a -> a -> b) -> [a] -> [a]
func_nubby f = nubBy (\x y -> isTruthy $ f x y)

func_words :: [Char] -> [[Char]]
func_words = words

func_unwords :: [[Char]] -> [Char]
func_unwords = unwords

func_lines :: [Char] -> [[Char]]
func_lines = lines

func_unlines :: [[Char]] -> [Char]
func_unlines = unlines

func_pfac :: TNum -> [TNum]
func_pfac = factorize 2
  where factorize _ 1 = [] 
        factorize d n 
            | d * d > n = [n]
            | n `mod` d == 0 = d : factorize d (n `div` d)
            | otherwise = factorize (d + 1) n

func_subs :: Concrete a => a -> a -> [a] -> [a]
func_subs x y = map (\z -> if z == x then y else z)

func_subs2 :: Concrete a => [a] -> [a] -> [a] -> [a]
func_subs2 _ _ [] = []
func_subs2 x y s@(h:t) | Just s2 <- stripPrefix x s = y++func_subs2 x y s2
                       | otherwise = h : func_subs2 x y t

func_group :: Concrete a => [a] -> [[a]]
func_group = group

func_groupOn :: Concrete b => (a -> b) -> [a] -> [[a]]
func_groupOn f = groupBy (\x y -> f x == f y)

func_groupBy :: Concrete b => (a -> a -> b) -> [a] -> [[a]]
func_groupBy _ [] = []
func_groupBy p (a:as) = consHead a $ go a as
  where go _ [] = [[]]
        go a (x:xs) | isTruthy $ p a x = consHead x $ go x xs
                    | otherwise        = [] : consHead x (go x xs)
        consHead x ~(xs:xss) = (x:xs):xss

func_perms :: [a] -> [[a]]
func_perms = permutations

func_subl :: Concrete a => [a] -> [a] -> TNum
func_subl super sub = subindex 1 super sub
  where subindex i _ [] = i
        subindex i super@(_:xs) sub = if sub`isPrefixOf`super then i else subindex (i+1) xs sub
        subindex _ [] _  = 0

-- Index of first truthy, or 0
func_any :: Concrete b => (a->b) -> [a] -> TNum
func_any f = go 1
  where go _ [] = 0
        go n (x:xs)
          | isTruthy $ f x = n
          | otherwise      = go (n+1) xs

-- Length + 1 or 0
func_all :: Concrete b => (a->b) -> [a] -> TNum
func_all f = go 1
  where go n [] = n
        go n (x:xs)
          | isTruthy $ f x = go (n+1) xs
          | otherwise      = 0

func_trsp :: [[a]] -> [[a]]
func_trsp = transpose

--Transpose with -> turns into a square matrix before transposing, padding with the given element
func_trspw :: a -> [[a]] -> [[a]]
func_trspw padding rows | all null rows = []
                        | otherwise     = map (headwith padding) rows : func_trspw padding (map (drop 1) rows)
   where headwith _       (x:_) = x
         headwith padding  _    = padding

--Zip, but keep extra elements from the longer list unaltered
func_zip' :: (a -> a -> a) -> [a] -> [a] -> [a]
func_zip' _ [] ys = ys
func_zip' _ xs [] = xs
func_zip' f (x:xs) (y:ys) = f x y : func_zip' f xs ys

func_cmap :: (a -> [b]) -> [a] -> [b]
func_cmap = concatMap

func_smap :: (a -> TNum) -> [a] -> TNum
func_smap f = sum . map f

func_cmapr :: [(a -> [b])] -> a -> [b]
func_cmapr fs = concat . func_mapr fs

func_smapr :: [(a -> TNum)] -> a -> TNum
func_smapr fs = sum . func_mapr fs

func_combin :: (b -> b -> c) -> (a -> b) -> a -> a -> c
func_combin f g x y = f (g x) (g y)

func_n2i :: TNum -> TNum
func_n2i x = func_floor $ x+1/2 --round halves towards positive infinity

func_c2i :: Char -> TNum
func_c2i c | Just i <- elemIndex c "0123456789" = fromIntegral i
           | otherwise                          = 0

-- Read the first number found in the string, or 0 if nothing found
func_s2i :: String -> TNum
func_s2i s = case takeWhile C.isDigit $ dropWhile (not . C.isDigit) s of
               "" -> 0
               x -> read x

func_list2 :: a -> a -> [a]
func_list2 x y = [x,y]

func_list3 :: a -> a -> a -> [a]
func_list3 x y z = [x,y,z]

func_list4 :: a -> a -> a -> a -> [a]
func_list4 x y z t = [x,y,z,t]

func_nubw :: Concrete a => [a] -> [a]
func_nubw xs = go [] xs
  where go ys (x:xs) | elem x ys = []
                     | otherwise = x:go (x:ys) xs
        go _ []                  = []

func_table :: (a -> b -> c) -> [a] -> [b] -> [[c]]
func_table f as bs = [[f a b | b <- bs] | a <- as]

func_lmap :: (a -> b -> c) -> [a] -> b -> [c]
func_lmap f as b = map (flip f b) as

func_rmap :: (a -> b -> c) -> a -> [b] -> [c]
func_rmap f a = map (f a)

func_mapacL :: (a -> b -> a) -> (a -> b -> c) -> a -> [b] -> [c]
func_mapacL _ _ _ []     = []
func_mapacL f g x (y:ys) = g x y : func_mapacL f g (f x y) ys

func_mapacR :: (b -> a -> a) -> (b -> a -> c) -> a -> [b] -> [c]
func_mapacR _ _ _ []     = []
func_mapacR f g x (y:ys) = g y (foldr f x ys) : func_mapacR f g x ys

func_replic :: TNum -> a -> [a]
func_replic n = replicate $ fromInteger $ toInteger n

func_replif :: a -> TNum -> [a]
func_replif = flip func_replic

func_abs :: TNum -> TNum
func_abs = abs

func_sign :: TNum -> TNum
func_sign = signum

-- x y -> base-x digits of y
func_base :: TNum -> TNum -> [TNum]
func_base 0 n = [n]
func_base 1 n
  | (d, m) <- divMod n 1, m /= 0 = func_base 1 d ++ [m]
  | otherwise                    = replicate (fromInteger $ toInteger $ abs n) $ signum n
func_base (-1) n
  | (d, m) <- divMod n 1, m /= 0 = go (func_base (-1) d) m
  | n > 0     = func_take (2*abs n - 1) $ cycle [1, 0]
  | otherwise = func_take (2*abs n) $ cycle [1, 0]
  where go [] m     = [m]
        go [0] m    = [m]
        go [k] m    = [k,0,m]
        go (k:ks) m = k : go ks m
func_base b n = reverse $ go n
  where go m | m >= 0 || b > 0, abs m < abs b = [m]
             | (d, r) <- divMod m b =
                 if r >= 0 || b > 0
                 then r : go d
                 else r-b : go (d+1)

func_base2 :: TNum -> [TNum]
func_base2 = func_base 2

func_base10 :: TNum -> [TNum]
func_base10 = func_base 10

-- x y -> y interpreted in base x
func_abase :: TNum -> [TNum] -> TNum
func_abase b ds = sum [b^n * d | (n, d) <- zip [0..] $ reverse ds]

func_abase2 :: [TNum] -> TNum
func_abase2 = func_abase 2

func_abas10 :: [TNum] -> TNum
func_abas10 = func_abase 10

func_double :: TNum -> TNum
func_double = (* 2)

func_halve :: TNum -> TNum
func_halve n = n / 2

-- a b -> b^a
func_power :: TNum -> TNum -> TNum
func_power (m :% 1) n
  | m >= 0    = n^m
  | otherwise = n^^m
func_power m n = n**m

func_square :: TNum -> TNum
func_square n = n * n

-- Should return a rational if input is a perfect square
func_sqrt :: TNum -> TNum
func_sqrt n | n < 0 = -func_sqrt (-n)
func_sqrt (p :% q) | Just r <- isqrt p,
                     Just s <- isqrt q
                   = r :% s
func_sqrt d = d**(1/2)

isqrt :: Integer -> Maybe Integer
isqrt n = go n $ div (n+1) 2
  where go a b | a <= b,
                 a*a == n  = Just a
               | a <= b    = Nothing
               | otherwise = go b $ div (b + div n b) 2

hasLength m [] = m <= 0
hasLength m (x:xs) = m <= 0 || hasLength (m-1) xs

func_slice :: TNum -> [a] -> [[a]]
func_slice n | n < 0 = map (genericTake (-n)) . init . tails
func_slice n = takeWhile ((>= n) . genericLength) . map (genericTake n) . tails

func_cuts :: [TNum] -> [a] -> [[a]]
func_cuts [] _ = []
func_cuts _ [] = []
func_cuts (m:ms) xs
  | m < 0, (ys, zs) <- genericSplitAt (-m) $ reverse xs
  = reverse ys : func_cuts ms (reverse zs)
  | (ys, zs) <- genericSplitAt m xs
  = ys : func_cuts ms zs

func_cut :: TNum -> [a] -> [[a]]
func_cut n | n < 0     = map reverse . reverse . func_cuts (repeat (-n)) . reverse
           | otherwise = func_cuts $ repeat n

func_mapad2 :: (a -> a -> b) -> [a] -> [b]
func_mapad2 _ [] = []
func_mapad2 f xs = zipWith f xs $ tail xs

func_mapad3 :: (a -> a -> a -> b) -> [a] ->[b]
func_mapad3 _ [] = []
func_mapad3 _ [_] = []
func_mapad3 f xs = zipWith3 f xs (tail xs) $ tail $ tail xs

func_join :: [a] -> [[a]] -> [a]
func_join x = concat . go
  where go [] = []
        go [y] = [y]
        go (y:ys) = y : x : go ys

func_join' :: a -> [[a]] -> [a]
func_join' = func_join . pure

func_powset :: [a] -> [[a]]
func_powset = subsequences

func_oelem :: Concrete a => [a] -> a -> TNum
func_oelem = go 1
  where go _ [] _ = 0
        go n (x:xs) y | y>x = go (n+1) xs y
                      | y==x = n
                      | otherwise = 0
                      
func_oelem' :: Concrete a => a -> [a] -> TNum
func_oelem' = flip func_oelem

func_isprime :: TNum -> TNum
func_isprime p | n :% 1 <- p,
                 n >= 2,
                 probablePrime n
               = func_oelem primes_list p
               | otherwise = 0
  where
    probablePrime :: Integer -> Bool
    probablePrime n
      | elem n [2,3,5,7] = True
      | any (\p -> rem n p == 0) [2,3,5,7] = False
      | not $ fermatProbPrime 2 n = False
      | Nothing <- isqrt n,
        d <- [d | d <- zipWith (*) [5,7..] $ cycle [1, -1], jacobi d n == -1]!!0
      = lucasProbPrime d n
      | otherwise = True
    oddify :: Integer -> (Integer, Integer)
    oddify k
      | even k, (d, s) <- oddify (div k 2) = (d, s+1)
      | otherwise = (k, 0)
    fermatProbPrime :: Integer -> Integer -> Bool
    fermatProbPrime a n
      | (d, s) <- oddify $ n-1
      = rem (a^d) n == 1 || or [rem (a^(d*(2^r))) n == n-1 | r <- [0..s-1]]
      | otherwise = False
    jacobi :: Integer -> Integer -> Integer
    jacobi _ 1 = 1
    jacobi 1 _ = 1
    jacobi 0 _ = 0
    jacobi a k
      | a < 0 || a >= k = jacobi (mod a k) k
      | even a          = (-1)^(div(k*k-1)8) * jacobi (div a 2) k
      | gcd a k > 1     = 0
      | otherwise       = (-1)^(div((a-1)*(k-1))4) * jacobi k a
    -- For Lucas sequences, we choose P = 1, Q = (1-D)/4
    lucasUVQ :: Integer -> Integer -> Integer -> (Integer, Integer, Integer)
    lucasUVQ n d = go
      where q = div (1-d) 4
            toEven r | odd r = r + n
                     | otherwise = r
            go 0 = (0, 2, 1)
            go 1 = (1, 1, q)
            go k | even k, (u, v, qk) <- go $ div k 2 = (rem (u*v) n, rem (v*v - 2*qk) n, rem (qk*qk) n)
                 | (u, v, qk) <- go $ k-1 = (div (toEven $ u + v) 2, div (toEven $ d*u + v) 2, rem (qk*q) n)
    -- For the Lucas test, we choose P = 1
    lucasProbPrime :: Integer -> Integer -> Bool
    lucasProbPrime d n
      | (u, _, _) <- lucasUVQ n d (n+1)
      = mod u n == 0

func_slices :: [a] -> [[a]]
func_slices xs = reverse . init . tails =<< inits xs

func_clone :: TNum -> [a] -> [a]
func_clone n = concatMap $ func_replic n

func_clone' :: [a] -> TNum -> [a]
func_clone' = flip func_clone

func_clones :: [TNum] -> [a] -> [a]
func_clones ns xs = concat $ zipWith func_replic ns xs

func_cycle :: [a] -> [a]
func_cycle [] = []
func_cycle xs = cycle xs

func_cumsum :: [TNum] -> [TNum]
func_cumsum = tail . scanl (+) 0

func_cumcat :: [[a]] -> [[a]]
func_cumcat = tail . scanl (++) []

func_isanum :: Char -> TNum
func_isanum c =
  if C.isAlphaNum c
  then func_ord c
  else 0

func_isalph :: Char -> TNum
func_isalph c =
  if C.isAlpha c
  then func_ord c
  else 0

func_isuppr :: Char -> TNum
func_isuppr c =
  if C.isUpper c
  then func_ord c
  else 0

func_islowr :: Char -> TNum
func_islowr c =
  if C.isLower c
  then func_ord c
  else 0

func_isdigt :: Char -> TNum
func_isdigt c =
  if C.isDigit c
  then func_ord c
  else 0

func_touppr :: Char -> Char
func_touppr = C.toUpper

func_tolowr :: Char -> Char
func_tolowr = C.toLower

func_swcase :: Char -> Char
func_swcase c = if C.isUpper c then C.toLower c else C.toUpper c

func_ceil :: TNum -> TNum
func_ceil a@(_ :% 0) = a
func_ceil x = ceiling x

func_floor :: TNum -> TNum
func_floor a@(_ :% 0) = a
func_floor x = floor x

func_gcd :: TNum -> TNum -> TNum
func_gcd = gcd

func_lcm :: TNum -> TNum -> TNum
func_lcm = lcm

func_small :: TNum -> TNum
func_small = boolToNum . (<= 1) . abs

func_twice :: (a -> a) -> (a -> a)
func_twice f = \x -> f (f x)

func_divmod :: TNum -> TNum -> [TNum]
func_divmod m n = [func_idiv m n, func_mod m n]

func_powstN :: TNum -> [a] -> [[a]]
func_powstN n
  | n < 0     = \xs -> foldl merge2 [] $ map (flip only xs) [0 .. -n]
  | otherwise = only n
  where only n [] = [[] | 0 <= n && n < 1]
        only n (x:xs)
          | 0 <= n && n < 1 = [[]]
          | otherwise = map (x:) (only (n-1) xs) `merge2` only n xs

func_rangeN :: TNum -> TNum -> [TNum]
func_rangeN a b | a <= b    = [a .. b]
                | otherwise = [a, a-1 .. b]

func_rangeC :: Char -> Char -> [Char]
func_rangeC a b | a <= b    = [a .. b]
                | otherwise = [a, pred a .. b]

func_find :: (Husky a, Concrete b) => (a -> b) -> [a] -> a
func_find f = func_head . func_filter f

func_findN :: (Concrete a) => (TNum -> a) -> TNum -> TNum
func_findN f n = func_find f [n..]

func_same :: (Concrete a) => [a] -> TNum
func_same [] = 1
func_same (x:xs) =
  if all (==x) xs
  then func_len xs + 2
  else 0

func_single :: [a] -> TNum
func_single [_] = 1
func_single _ = 0

func_iter2 :: (x -> (x,y)) -> x -> [y]
func_iter2 f = map (snd . f) . iterate (fst . f)

rangify :: (Enum a, Ord a) => [a] -> [a]
rangify (m:ns@(n:_)) = range ++ rangify ns
  where range | m < n  = [m .. pred n]
              | m > n  = [m, pred m .. succ n]
              | m == n = [m]
rangify ns = ns

func_rangeL :: [TNum] -> [TNum]
func_rangeL = rangify

func_rangeS :: [Char] -> [Char]
func_rangeS = rangify

func_joinE :: [a] -> [a] -> [a]
func_joinE xs ys = func_join xs $ map pure ys

func_branch :: (x -> y -> z) -> (a -> x) -> (b -> y) -> a -> b -> z
func_branch f g h x y = f (g x) (h y)

func_rotate :: TNum -> [a] -> [a]
func_rotate n xs | n >= 0    = iterate lrot xs !! fromIntegral n
                 | otherwise = iterate rrot xs !! fromIntegral (-n)
  where lrot [] = []
        lrot (x:xs) = xs ++ [x]
        rrot [] = []
        rrot xs = last xs : init xs

func_rotatf :: [a] -> TNum -> [a]
func_rotatf = flip func_rotate

func_bhook :: (x -> y -> z) -> (x -> u -> y) -> x -> u -> z
func_bhook f g a b = f a (g a b)

func_bhookf :: (x -> y -> z) -> (u -> y -> x) -> u -> y -> z
func_bhookf f g a b = f (g a b) b

func_until :: Concrete b => (a -> b) -> (a -> a) -> a -> a
func_until p = until (isTruthy . p)

func_divs :: TNum -> [TNum]
func_divs n = [d | d <- [1..n], isTruthy $ func_divds d n]

func_uwshow :: Concrete a => [a] -> [Char]
func_uwshow = unwords . map show

func_ulshow :: Concrete a => [a] -> [Char]
func_ulshow = unlines . map show

func_decorM :: (((a, b) -> c) -> [(a,b)] -> [[(a, b')]]) -> (a -> b -> c) -> [a] -> [b] -> [[b']]
func_decorM f g xs ys = map (map snd) $ f (uncurry g) $ zip xs ys

func_decorL :: (((a, b) -> c) -> [(a,b)] -> [(a, b')]) -> (a -> b -> c) -> [a] -> [b] -> [b']
func_decorL f g xs ys = map snd $ f (uncurry g) $ zip xs ys

func_decorV :: (((a, b) -> c) -> [(a,b)] -> (a, b')) -> (a -> b -> c) -> [a] -> [b] -> b'
func_decorV f g xs ys = snd $ f (uncurry g) $ zip xs ys

func_decorN :: (((a, b) -> c) -> [(a,b)] -> d) -> (a -> b -> c) -> [a] -> [b] -> d
func_decorN f g xs ys = f (uncurry g) $ zip xs ys

func_prep0 :: Husky a => [a] -> [a]
func_prep0 xs = defVal:xs

func_doubL :: [a] -> [a]
func_doubL xs = xs++xs

func_halfL :: [a] -> [[a]]
func_halfL xs = go xs xs where
        go (y:_:ys) (z:zs) = let [zs',zs''] = go ys zs in [z:zs',zs'']
        go (y:ys)   (z:zs) = [[z],zs]
        go    _      zs    = [[],zs]

-- Merge a potentially infinite number of potentially infinite lists
-- Each list is assumed to be sorted
-- If there are more than two lists, the heads are assumed to be sorted as well
func_merge :: (Concrete a) => [[a]] -> [a]
func_merge [] = []
func_merge [xs] = xs
func_merge ([]:xss) = func_merge xss
func_merge [xs, []] = xs
func_merge [xs@(x:_), ys@(y:_)] | x > y = func_merge [ys, xs]
func_merge ((x:xs):yss) = x : func_merge (put xs yss)
  where put [] yss = yss
        put xs [] = [xs]
        put xs ([]:yss) = put xs yss
        put xs@(x:_) yss@(ys@(y:_):zss)
         | x <= y    = xs : yss
         | otherwise = ys : put xs zss

-- Minima and maxima with custom comparison
-- p x y means x is greater then y

func_minby :: Concrete b => (a -> a -> b) -> a -> a -> a
func_minby p x y = if isTruthy $ p x y then y else x

func_maxby :: Concrete b => (a -> a -> b) -> a -> a -> a
func_maxby p x y = if isTruthy $ p x y then x else y

func_minon :: Concrete b => (a -> b) -> a -> a -> a
func_minon f x y = if f x <= f y then x else y

func_maxon :: Concrete b => (a -> b) -> a -> a -> a
func_maxon f x y = if f x <= f y then y else x

func_minlby :: (Husky a, Concrete b) => (a -> a -> b) -> [a] -> a
func_minlby _ [] = defVal
func_minlby p xs = snd $ minimumBy (comparing fst) $ map (\x -> (length [y | y <- xs, isTruthy $ p x y], x)) xs

func_maxlby :: (Husky a, Concrete b) => (a -> a -> b) -> [a] -> a
func_maxlby _ [] = defVal
func_maxlby p xs = snd $ maximumBy (comparing fst) $ map (\x -> (length [y | y <- xs, isTruthy $ p x y], x)) xs

func_minlon :: (Husky a, Concrete b) => (a -> b) -> [a] -> a
func_minlon _ [] = defVal
func_minlon f xs = foldr1 (func_minon f) xs

func_maxlon :: (Husky a, Concrete b) => (a -> b) -> [a] -> a
func_maxlon _ [] = defVal
func_maxlon f xs = foldr1 (func_maxon f) xs


func_aptp :: (a -> b -> c) -> (a, b) -> c
func_aptp f (x, y) = f x y

func_apftp :: (a -> b -> c) -> (b, a) -> c
func_apftp f (x, y) = f y x

func_scltp :: (a -> b -> c) -> (a, b) -> (c, b)
func_scltp f (x, y) = (f x y, y)

func_scrtp :: (a -> b -> c) -> (a, b) -> (a, c)
func_scrtp f (x, y) = (x, f x y)

func_maptp :: (a -> b) -> (a, a) -> (b, b)
func_maptp f (x, y) = (f x, f y)

func_lmaptp :: (a -> c) -> (a, b) -> (c, b)
func_lmaptp f (x, y) = (f x, y)

func_rmaptp :: (b -> c) -> (a, b) -> (a, c)
func_rmaptp f (x, y) = (x, f y)


func_adiags :: [[a]] -> [[a]]
func_adiags = func_init . go 1
  where go _ [] = []
        go n xss =
          let (prefix, suffix) = splitAt n xss
              (diag, rests) = unzip [(x, xs) | (x:xs) <- prefix]
          in diag : go (length diag + 1) (rests ++ suffix)

func_lrange :: TNum -> [TNum]
func_lrange n | n >= 0    = [0 .. n-1]
              | otherwise = [n+1 .. 0]

func_ixes :: [x] -> [TNum]
func_ixes = zipWith const [1..]

func_srange :: TNum -> [TNum]
func_srange n | n >= 0    = [-n .. n]
              | otherwise = [-n, -n-1 .. n]

func_rvixes :: [x] -> [TNum]
func_rvixes xs = reverse [1 .. genericLength xs]

func_cpow :: TNum -> [x] -> [[x]]
func_cpow n xs = func_cartes $ func_replic n xs

func_cpow' :: [x] -> TNum -> [[x]]
func_cpow' = flip func_cpow

func_cpowN :: TNum -> TNum -> [[TNum]]
func_cpowN n = func_cpow n . func_heads

func_toadjM :: (((a, a) -> c) -> [(a,a)] -> [[(a, a)]]) -> (a -> a -> c) -> [a] -> [[a]]
func_toadjM f g xs = func_decorM f g (func_tail xs) xs

func_toadjL :: (((a, a) -> c) -> [(a,a)] -> [(a, a)]) -> (a -> a -> c) -> [a] -> [a]
func_toadjL f g xs = func_decorL f g (func_tail xs) xs

func_toadjV :: (((a, a) -> c) -> [(a,a)] -> (a, a)) -> (a -> a -> c) -> [a] -> a
func_toadjV f g xs = func_decorV f g (func_tail xs) xs

func_toadjN :: (((a, a) -> c) -> [(a,a)] -> b) -> (a -> a -> c) -> [a] -> b
func_toadjN f g xs = func_decorN f g (func_tail xs) xs

func_all2 :: Concrete y => (x -> x -> y) -> [x] -> TNum
func_all2 _ [] = 1
func_all2 pred xs =
  case func_toadjN func_all pred xs of
    0 -> 0
    n -> n + 1

func_any2 :: Concrete y => (x -> x -> y) -> [x] -> TNum
func_any2 = func_toadjN func_any

func_count2 :: Concrete y => (x -> x -> y) -> [x] -> TNum
func_count2 = func_toadjN func_countf

func_sameon :: Concrete y => (x -> y) -> [x] -> TNum
func_sameon f = func_same . map f

func_sameby :: Concrete y => (x -> x -> y) -> [x] -> TNum
func_sameby p xs =
  if and [isTruthy $ p x y | (x:ys) <- tails xs, y <- ys]
  then func_len xs + 1
  else 0

func_keyon :: Concrete y => (x -> y) -> [x] -> [[x]]
func_keyon f = func_groupOn f . func_sorton f

func_keyby :: Concrete y => (x -> x -> y) -> [x] -> [[x]]
func_keyby p = go []
  where go yss [] = yss
        go yss (x:xs) = go (put x yss) xs
        put x [] = [[x]]
        put x (ys:yss) | and [isTruthy $ p y x | y <- ys] = (ys++[x]) : yss
                       | otherwise                        = ys : put x yss

func_unzip :: [(x, y)] -> ([x], [y])
func_unzip = unzip

func_split :: Concrete x => x -> [x] -> [[x]]
func_split x = go
  where go [] = [[]]
        go (y:ys) | x == y            = [] : go ys
                  | (zs:zss) <- go ys = (y:zs) : zss
                  | otherwise         = [[y]]

func_split' :: Concrete x => [x] -> x -> [[x]]
func_split' = flip func_split

func_splitL :: Concrete x => [x] -> [x] -> [[x]]
func_splitL xs = go
  where go [] = [[]]
        go ys     | Just zs <- stripPrefix xs ys = [] : go zs
        go (y:ys) | (zs:zss) <- go ys            = (y:zs) : zss
                  | otherwise                    = [[y]]

func_joinV :: x -> [x] -> [x]
func_joinV = intersperse

lenBelow xs n | n <= 0 = False
lenBelow [] n = True
lenBelow (_:xs) n = lenBelow xs (n-1)

func_replen :: [x] -> TNum -> [x]
func_replen [] _ = []
func_replen xs n | n < 0     = func_replen (reverse xs) (-n)
                 | n == 0    = []
                 | otherwise = go (cycle xs) xs 0
  where go _ [] _ = []
        go as (_:bs) m | mn <- func_floor $ m+n = func_take mn as ++ go (func_drop mn as) bs (func_mod 1 $ m+n)

func_repln' :: TNum -> [x] -> [x]
func_repln' = flip func_replen

func_isect :: Concrete x => [x] -> [x] -> [x]
func_isect [] _ = []
func_isect _ [] = []
func_isect (x:xs) ys | Just zs <- del ys = x : func_isect xs zs
                     | otherwise         = func_isect xs ys
  where del [] = Nothing
        del (y:ys) | y == x    = Just ys
                   | otherwise = (y:) <$> del ys

func_mean :: [TNum] -> TNum
func_mean [] = 0
func_mean xs = sum xs / func_len xs

func_cart2 :: [a] -> [a] -> [[a]]
func_cart2 xs ys = [[x,y] | x <- xs, y <- ys]

func_ccons :: [a] -> [[a]] -> [[a]]
func_ccons xs yss = [x:ys | x <- xs, ys <- yss]

func_csnoc :: [[a]] -> [a] -> [[a]]
func_csnoc xss ys = [xs++[y] | xs <- xss, y <- ys]

func_bwand :: TNum -> TNum -> TNum
func_bwand r s
  | (m, a) <- properFraction r,
    (n, b) <- properFraction s = go (1/2) ((m .&. n) :% 1) a b
  where go _ t 0 _ = t
        go _ t _ 0 = t
        go d t x y | t == t+d     = t
                   | x < d, y < d = go (d/2) t x y
                   | x < d        = go (d/2) t x (y-d)
                   | y < d        = go (d/2) t (x-d) y
                   | otherwise    = go (d/2) (t+d) (x-d) (y-d)

func_bwor :: TNum -> TNum -> TNum
func_bwor r s
  | (m, a) <- properFraction r,
    (n, b) <- properFraction s = go (1/2) ((m .|. n) :% 1) a b
  where go _ t 0 0 = t
        go d t x y | t == t+d     = t
                   | x < d, y < d = go (d/2) t x y
                   | x < d        = go (d/2) (t+d) x (y-d)
                   | y < d        = go (d/2) (t+d) (x-d) y
                   | otherwise    = go (d/2) (t+d) (x-d) (y-d)

func_union :: Concrete x => [x] -> [x] -> [x]
func_union xs ys = (filter (`notElem` ys) $ nub xs) ++ ys

func_ucons :: Concrete x => x -> [x] -> [x]
func_ucons x ys = if x `elem` ys then ys else x:ys

func_usnoc :: Concrete x => [x] -> x -> [x]
func_usnoc xs y = if y `elem` xs then xs else xs++[y]


func_uwpshw :: (Concrete a, Concrete b) => (a, b) -> [Char]
func_uwpshw (a, b) = unwords [show a, show b]

func_ulpshw :: (Concrete a, Concrete b) => (a, b) -> [Char]
func_ulpshw (a, b) = unlines [show a, show b]

func_subset :: (Concrete a) => [a] -> [a] -> TNum
func_subset _ [] = 1
func_subset [] _ = 0
func_subset xs (y:ys) | Just zs <- del y xs = func_subset zs ys
                      | otherwise           = 0
  where del a [] = Nothing
        del a (b:bs) | a == b    = Just bs
                     | otherwise = (b:) <$> del a bs

func_comf :: (x -> y -> z) -> (u -> y) -> x -> u -> z
func_comf f g = \x y -> f x $ g y

func_comf2 :: (x -> y -> z) -> (u -> v -> y) -> x -> u -> v -> z
func_comf2 f g = \x y z -> f x $ g y z

func_comf3 :: (x -> y -> z) -> (u -> v -> w -> y) -> x -> u -> v -> w -> z
func_comf3 f g = \x y z u -> f x $ g y z u

func_comf4 :: (x -> y -> z) -> (u -> v -> w -> t -> y) -> x -> u -> v -> w -> t -> z
func_comf4 f g = \x y z u v -> f x $ g y z u v

func_gaps :: (Husky a) => TNum -> [a] -> [a]
func_gaps n = func_gapsL $ repeat n

func_gaps2 :: (Husky a) => [a] -> TNum -> [a]
func_gaps2 = flip func_gaps

func_gapsL :: (Husky a) => [TNum] -> [a] -> [a]
func_gapsL ns = concat . zipWith go ns . func_cuts (abs <$> ns)
  where go n | n < 0     = func_drop (-n-1)
             | otherwise = func_take 1

func_cut2 :: [a] -> TNum -> [[a]]
func_cut2 = flip func_cut

func_chrsum :: [Char] -> TNum
func_chrsum = func_sum . map func_ord

func_nubwN :: Concrete a => TNum -> [a] -> [a]
func_nubwN k = go [] . func_slice (abs k)
  where go ys (xs:xss) | elem xs ys = if k < 0 then [] else func_init xs
                       | null xss   = xs
                       | otherwise  = func_head xs : go (xs:ys) xss
        go _ []                     = []
        
func_revnum :: TNum -> TNum
func_revnum = func_abas10 . func_rev . func_base10


func_onixes :: Husky a => ((TNum -> a) -> [TNum] -> b) -> [a] -> b
func_onixes f xs = f (func_index2 xs) $ func_ixes xs

func_flipap :: b -> (a -> b -> c) -> a -> c
func_flipap = flip flip

func_cutL :: [[a]] -> [b] -> [[b]]
func_cutL ((_:xs):xss) (y:ys) | (zs:zss) <- func_cutL (xs:xss) ys = (y:zs):zss
func_cutL ([]:xss) ys = [] : func_cutL xss ys
func_cutL [] _ = []
func_cutL _ [] = []

func_ixsof :: (Concrete a) => a -> [a] -> [TNum]
func_ixsof x ys = [i | (i, y) <- zip [1..] ys, y == x]

func_ixsof2 :: (Concrete a) => [a] -> a -> [TNum]
func_ixsof2 = flip func_ixsof

func_where :: (Concrete b) => (a -> b) -> [a] -> [TNum]
func_where f xs = [i | (i, x) <- zip [1..] xs, isTruthy $ f x]

func_where2 :: (Concrete b) => (a -> a -> b) -> [a] -> [TNum]
func_where2 = func_toadjN func_where

func_idx2d :: Husky a => (TNum, TNum) -> [[a]] -> a
func_idx2d (x, y) = func_index y . func_index x

func_idx2d2 :: Husky a => [[a]] -> (TNum, TNum) -> a
func_idx2d2 = flip func_idx2d
