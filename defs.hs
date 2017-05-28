{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, UndecidableInstances, FlexibleInstances, FlexibleContexts #-}

import IntSeq

import Data.Function (fix)
import System.Environment (getArgs)
import Data.Char (ord,chr)
import Data.List

class Vect a b x y | a b x -> y where func_vec :: (a -> b) -> (x -> y)

instance Vect a b a b where func_vec = id
instance (Vect a b x y) => Vect a b [x] [y] where func_vec = map . func_vec

class ToString a where
  toString :: a -> String

instance {-# OVERLAPPING #-} ToString [Char] where
  toString = id

instance Concrete a => ToString a where
  toString = show

class (Show a, Read a, Eq a, Ord a, ToString a) => Concrete a where
  isTruthy :: a -> Bool
  toTruthy :: a -> Integer
  func_false :: a
  func_true :: a
  func_lt :: a -> a -> Integer
  func_gt :: a -> a -> Integer
  func_le :: a -> a -> Integer
  func_ge :: a -> a -> Integer
  
  func_heads :: a -> [a]
  func_tails :: a -> [a]
  
  func_eq :: a -> a -> Integer
  func_eq x y = boolToNum $ x == y
  
  func_neq :: a -> a -> Integer
  
  func_or :: a -> a -> a
  func_or x y = if isTruthy x then x else y
  
  func_and :: a -> a -> a
  func_and x y = if isTruthy x then y else x
  
  func_read :: [Char] -> a
  func_read = read

func_or' :: (Concrete a, Concrete b) => a -> b -> Integer
func_or' x y = func_or (toTruthy x) (toTruthy y)

func_and' :: (Concrete a, Concrete b) => a -> b -> Integer
func_and' x y = func_and (toTruthy x) (toTruthy y)

instance Concrete Integer where
  isTruthy = (/= 0)
  toTruthy = id
  func_false = 0
  func_true = 1
  func_lt y x = max 0 (y-x)
  func_gt y x = max 0 (x-y)
  func_le y x = max 0 (y-x+1)
  func_ge y x = max 0 (x-y+1)
  func_neq y x = abs $ x-y
  func_heads x = [1..x]
  func_tails x = [x,x-1..1]

instance Concrete Double where
  isTruthy = (/= 0)
  toTruthy = roundAway
  func_false = 0
  func_true = 1
  func_lt y x = max 0 $ roundAway(y-x)
  func_gt y x = max 0 $ roundAway(x-y)
  func_le y x = max 0 $ roundAway(y-x+1)
  func_ge y x = max 0 $ roundAway(x-y+1)
  func_neq y x = abs.roundAway $ x-y
  func_heads x = [1..x]
  func_tails x = [x,x-1..1]

instance Concrete Char where
  isTruthy = (/= 0).ord
  toTruthy = fromIntegral.ord
  func_false = '\0'
  func_true = '\n'
  func_lt y x = fromIntegral $ max 0 (ord y - ord x)
  func_gt y x = fromIntegral $ max 0 (ord x - ord y)
  func_le y x = fromIntegral $ max 0 (ord y - ord x + 1)
  func_ge y x = fromIntegral $ max 0 (ord x - ord y + 1)
  func_neq y x = abs.fromIntegral $ (ord x)-(ord y)
  func_heads x = ['\0'..x]
  func_tails x = [x, pred x..'\0']

instance Concrete a => Concrete [a] where
  isTruthy = (/= [])
  toTruthy = genericLength
  func_false = []
  func_true = [func_true]
  func_lt = go 1
    where go n [] (_:_) = n
          go n (y:ys) (x:xs) | x < y = n
                             | x > y = 0
                             | otherwise = go (n+1) ys xs
          go _ _ _ = 0
  func_gt x y = func_lt y x
  func_le = go 1
    where go n [] _ = n
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
  
  func_heads=inits
  func_tails=tails

instance (Concrete a, Concrete b) => Concrete (a, b) where
  isTruthy (x, y) = isTruthy x && isTruthy y
  toTruthy (x, y) = toTruthy x * toTruthy y
  func_false = (func_false, func_false)
  func_true = (func_true, func_true)
  func_lt (x, y) (x', y') = if x == x' then func_lt y y' else func_lt x x'
  func_gt (x, y) (x', y') = if x == x' then func_gt y y' else func_gt x x'
  func_le (x, y) (x', y') = if x > x' then func_lt y y' else func_le x x'
  func_ge (x, y) (x', y') = if x < x' then func_gt y y' else func_ge x x'
  func_neq (x, y) (x', y') = if x == x' then func_neq y y' else func_neq x x'
  
  func_heads (a,b) = [(c,d)|c<-func_heads a,d<-func_heads b]
  func_tails (a,b) = [(c,d)|c<-func_tails a,d<-func_tails b]

class (Num n, Concrete n, Enum n, Real n) => Number n where
  valueOf :: n -> Either Integer Double

instance Number Integer where
  valueOf = Left

instance Number Double where
  valueOf = Right

boolToNum :: (Num a) => Bool -> a
boolToNum = fromInteger . toInteger . fromEnum

roundAway :: Double -> Integer
roundAway d = if d<0 then floor d else ceiling d

func_fix :: (a -> a) -> a
func_fix = fix

func_fixp :: Concrete a => (a -> a) -> a -> a
func_fixp f a = go a $ f a
  where go x y | x == y    = y
               | otherwise = go y $ f y

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

func_add :: Number n => n -> n -> n
func_add = (+)

func_addID :: Integer -> Double -> Double
func_addID a b = fromInteger a + b

func_addDI :: Double -> Integer -> Double
func_addDI a b = a + fromInteger b

func_sub :: Number n => n -> n -> n
func_sub b a = a - b

func_subID :: Integer -> Double -> Double
func_subID b a = a - fromInteger b

func_subDI :: Double -> Integer -> Double
func_subDI b a = fromInteger a - b

func_mul :: Number n => n -> n -> n
func_mul = (*)

func_mulID :: Integer -> Double -> Double
func_mulID a b = fromInteger a * b

func_mulDI :: Double -> Integer -> Double
func_mulDI a b = a * fromInteger b

func_div :: (Number m, Number n) => m -> n -> Double
func_div b a = x / y
  where x | Left n  <- valueOf a = fromInteger n
          | Right r <- valueOf a = r
        y | Left n  <- valueOf b = fromInteger n
          | Right r <- valueOf b = r

func_idiv :: (Number m, Number n) => m -> n -> Integer
func_idiv b a | Left m <- valueOf a,
                Left n <- valueOf b  = m `div` n
              | Left m <- valueOf a  = func_idiv b (fromInteger m :: Double)
              | Left n <- valueOf b  = func_idiv (fromInteger n :: Double) a
              | Right r <- valueOf a,
                Right s <- valueOf b = floor $ r / s

func_mod :: Number n => n -> n -> n
func_mod b a = a - fromInteger (func_idiv b a) * b

func_modID :: Integer -> Double -> Double
func_modID b a = a - fromInteger (func_idiv b a * b)

func_modDI :: Double -> Integer -> Double
func_modDI b a = fromInteger a - fromInteger (func_idiv b a) * b

func_divds :: Number n => n -> n -> Integer
func_divds b a = func_not $ func_mod b a

func_neg :: Number n => n -> n
func_neg x = -x

func_inv :: Number n => n -> Double
func_inv x | Left n  <- valueOf x = recip $ fromInteger n
           | Right r <- valueOf x = recip r

-- Triangular numbers: sum of all numbers in [1..n]
func_trianI :: Integer -> Integer
func_trianI n =  div (n*(n+1)) 2

func_trianD :: Double -> Double
func_trianD r = r*(r+1)/2

func_fact :: Number n => n -> n
func_fact n = product [1..n]

func_pure :: a -> [a]
func_pure = (: [])

func_cons :: a -> [a] -> [a]
func_cons = (:)

func_snoc :: [a] -> a -> [a]
func_snoc x y = x ++ [y]

func_cat :: [a] -> [a] -> [a]
func_cat = (++)

func_head :: [a] -> a
func_head = head

func_last :: [a] -> a
func_last = last

func_tail :: [a] -> [a]
func_tail = tail

func_init :: [a] -> [a]
func_init = init

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

func_len :: [a] -> Integer
func_len = genericLength

func_nlen :: Number a => a -> Integer
func_nlen = genericLength.show

func_countf :: Concrete b => (a -> b) -> [a] -> Integer
func_countf c = genericLength . filter (isTruthy . c)

func_count :: Concrete a => a -> [a] -> Integer
func_count x = genericLength . filter (== x)

func_index :: Integer -> [a] -> a
func_index i
  | i>0       = flip genericIndex (i-1).cycle
  | otherwise = flip genericIndex (-i).cycle.reverse
  
func_index2 :: [a] -> Integer -> a
func_index2 = flip func_index

func_rev :: [a] -> [a]
func_rev = reverse

func_nats :: Number n => [n]
func_nats = [1, 2 ..]

func_sum :: Number n => [n] -> n
func_sum = sum

func_prod :: Number n => [n] -> n
func_prod = product

func_concat :: [[a]] -> [a]
func_concat = concat

func_cartes :: [[a]] -> [[a]]
func_cartes = mapM id

func_if :: Concrete a => b -> b -> a -> b
func_if b c a = if isTruthy a then b else c

func_if2 :: Concrete a => (a->b) -> b -> a -> b
func_if2 g c a = if isTruthy a then g a else c

func_fif :: Concrete a => (x->b) -> (x->b) -> (x->a) -> x -> b
func_fif g h f x = if isTruthy (f x) then g x else h x

func_not :: Concrete a => a -> Integer
func_not a = if isTruthy a then 0 else 1

func_fnot :: Concrete b => (a -> b) -> a -> Integer
func_fnot f = func_not . f

func_hook :: (a -> b -> c) -> (a -> b) -> a -> c
func_hook x y z = x z (y z)

func_const :: a -> b -> a
func_const x _ = x

func_id :: a -> a
func_id x = x

func_flip :: (a -> b -> c) -> (b -> a -> c)
func_flip = flip

func_foldl :: (b -> a -> b) -> b -> [a] -> b
func_foldl = foldl

func_foldl1 :: (a -> a -> a) -> [a] -> a
func_foldl1 = foldl1

func_foldr :: (a -> b -> b) -> b -> [a] -> b
func_foldr = foldr

func_foldr1 :: (a -> a -> a) -> [a] -> a
func_foldr1 = foldr1


func_take :: Integer -> [a] -> [a]
func_take n
  | n >= 0    = genericTake n
  | otherwise = reverse . genericTake (-n) . reverse

func_takew :: Concrete b => (a -> b) -> [a] -> [a]
func_takew _ [] = []
func_takew f (x:xs)
  | isTruthy(f x) = x : func_takew f xs
  | otherwise     = []

func_drop :: Integer -> [a] -> [a]
func_drop n
  | n >= 0    = genericDrop n
  | otherwise = reverse . genericDrop (-n) . reverse

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

func_listN :: (a -> [a] -> b) -> [a] -> b
func_listN _ []      = error "listN only supports nonempty lists."
func_listN f (x:xs) = f x xs

func_listF :: b -> (([a] -> b) -> (a -> [a] -> b)) -> [a] -> b
func_listF c f = go
  where go [] = c
        go (x:xs) = f go x xs

func_listNF :: (([a] -> b) -> (a -> [a] -> b)) -> [a] -> b
func_listNF f = go
  where go [] = error "listNF only supports nonempty lists."
        go (x:xs) = f go x xs

func_fork :: (a -> b -> c) -> (x -> a) -> (x -> b) -> x -> c
func_fork f g h x = f (g x) (h x)

func_fork2 :: (a -> b -> c) -> (x -> y -> a) -> (x -> y -> b) -> x -> y -> c
func_fork2 f g h x y = f (g x y) (h x y)

func_argdup :: (a -> a -> b) -> a -> b
func_argdup f x = f x x

func_iter :: (a -> a) -> a -> [a]
func_iter = iterate

func_rep :: a -> [a]
func_rep = repeat

func_ord :: Char -> Integer
func_ord = fromIntegral.ord

func_chr :: Integer -> Char
func_chr = chr.fromInteger

func_show :: Concrete a => a -> String
func_show = show

func_empty :: [a]
func_empty = []

func_predN :: Number n => n -> n
func_predN n = n-1

func_succN :: Number n => n -> n
func_succN n = n+1

func_predC :: Char -> Char
func_predC = pred

func_succC :: Char -> Char
func_succC = succ

func_elem :: Concrete a => [a] -> a -> Integer
func_elem xs x | Just i <- elemIndex x xs = fromIntegral i + 1
               | otherwise                = 0

func_sort :: Concrete a => [a] -> [a]
func_sort = sort

func_sorton :: Concrete b => (a -> b) -> [a] -> [a]
func_sorton = sortOn

func_sortby :: (a -> a -> Integer) -> [a] -> [a]
func_sortby f = sortBy $ \x y -> compare (f x y) 0

func_max :: Concrete a => a -> a -> a
func_max = max

func_min :: Concrete a => a -> a -> a
func_min = min

func_maxl :: Concrete a => [a] -> a
func_maxl = maximum

func_minl :: Concrete a => [a] -> a
func_minl = minimum

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

func_pfac :: Integer -> [Integer]
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
func_groupBy f = groupBy (\x y -> isTruthy $ f x y)

func_perms :: [a] -> [[a]]
func_perms = permutations

func_subl :: Concrete a => [a] -> [a] -> Integer
func_subl super sub = subindex 1 super sub
  where subindex i _ [] = i
        subindex i super@(_:xs) sub = if sub`isPrefixOf`super then i else subindex (i+1) xs sub
        subindex _ [] _  = 0
        
func_any :: Concrete b => (a->b) -> [a] -> b
func_any f = foldl func_or func_false . map f

func_all :: Concrete b => (a->b) -> [a] -> b
func_all f = foldl func_and func_true . map f

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