{-# LANGUAGE FlexibleContexts #-}

import Data.Function (fix)
import System.Environment (getArgs)
import Data.Char (ord)
import Data.List (genericLength,genericIndex,findIndex)

class (Show a, Read a, Eq a, Ord a) => Concrete a where
  isTruthy :: a -> Bool
  toTruthy :: a -> Integer
  func_lt :: a -> a -> Integer
  func_gt :: a -> a -> Integer
  
  func_eq :: a -> a -> Integer
  func_eq x y = boolToNum $ x == y
  
  func_neq :: a -> a -> Integer
  
  func_or :: a -> a -> a
  func_or x y = if isTruthy x then x else y
  
  func_and :: a -> a -> a
  func_and x y = if isTruthy x then y else x

func_or' :: (Concrete a, Concrete b) => a -> b -> Integer
func_or' x y = func_or (toTruthy x) (toTruthy y)

func_and' :: (Concrete a, Concrete b) => a -> b -> Integer
func_and' x y = func_and (toTruthy x) (toTruthy y)

instance Concrete Integer where
  isTruthy = (/= 0)
  toTruthy = id
  func_lt x y = max 0 (y-x)
  func_gt x y = max 0 (x-y)
  func_neq x y = x-y

instance Concrete Double where
  isTruthy = (/= 0)
  toTruthy = roundAway
  func_lt x y = max 0 $ roundAway(y-x)
  func_gt x y = max 0 $ roundAway(x-y)
  func_neq x y = roundAway $ x-y

instance Concrete Char where
  isTruthy = (/= 0).ord
  toTruthy = fromIntegral.ord
  func_lt x y = fromIntegral $ max 0 (ord y - ord x)
  func_gt x y = fromIntegral $ max 0 (ord x - ord y)
  func_neq x y = fromIntegral $ (ord x)-(ord y)

instance Concrete a => Concrete [a] where
  isTruthy = (/= [])
  toTruthy = genericLength
  func_lt x y = case findIndex (uncurry (/=)) (zip x y) of
                  Just i  ->  if x!!i < y!!i then fromIntegral i+1 else 0
                  Nothing ->  0
  func_gt x y = case findIndex (uncurry (/=)) (zip x y) of
                  Just i  ->  if x!!i > y!!i then fromIntegral i+1 else 0
                  Nothing ->  0
  func_neq x y = case findIndex (uncurry (/=)) (zip x y) of
                  Just i  ->  fromIntegral i+1
                  Nothing ->  0

class (Num n, Ord n, Show n, Read n, Concrete n) => Number n where
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

func_add :: Number n => n -> n -> n
func_add = (+)

func_addID :: Integer -> Double -> Double
func_addID a b = fromInteger a + b

func_addDI :: Double -> Integer -> Double
func_addDI a b = a + fromInteger b

func_sub :: Number n => n -> n -> n
func_sub = (-)

func_subID :: Integer -> Double -> Double
func_subID a b = fromInteger a - b

func_subDI :: Double -> Integer -> Double
func_subDI a b = a - fromInteger b

func_mul :: Number n => n -> n -> n
func_mul = (*)

func_mulID :: Integer -> Double -> Double
func_mulID a b = fromInteger a * b

func_mulDI :: Double -> Integer -> Double
func_mulDI a b = a * fromInteger b

func_div :: (Number m, Number n) => m -> n -> Double
func_div a b = x / y
  where x | Left n  <- valueOf a = fromInteger n
          | Right r <- valueOf a = r
        y | Left n  <- valueOf b = fromInteger n
          | Right r <- valueOf b = r

func_idiv :: (Number m, Number n) => m -> n -> Integer
func_idiv a b | Left m <- valueOf a,
                Left n <- valueOf b  = m `div` n
              | Left m <- valueOf a  = func_idiv (fromInteger m :: Double) b
              | Left n <- valueOf b  = func_idiv a (fromInteger n :: Double)
              | Right r <- valueOf a,
                Right s <- valueOf b = floor $ r / s

func_mod :: Number n => n -> n -> n
func_mod a b = a - fromInteger (func_idiv a b) * b

func_modID :: Integer -> Double -> Double
func_modID a b = fromInteger a - fromInteger (func_idiv a b) * b

func_modDI :: Double -> Integer -> Double
func_modDI a b = a - fromInteger (func_idiv a b * b)

func_neg :: Number n => n -> n
func_neg x = -x

func_inv :: Number n => n -> Double
func_inv x | Left n  <- valueOf x = recip $ fromInteger n
           | Right r <- valueOf x = recip r

func_pure :: a -> [a]
func_pure = (: [])

func_cons :: a -> [a] -> [a]
func_cons = (:)

func_snoc :: [a] -> a -> [a]
func_snoc x y = x ++ [y]

func_cat :: [a] -> [a] -> [a]
func_cat = (++)

func_pair :: a -> a -> [a]
func_pair x y = [x, y]

func_map :: (a -> b) -> [a] -> [b]
func_map = map

func_zip :: (a -> b -> c) -> [a] -> [b] -> [c]
func_zip = zipWith

func_filter :: Concrete b => (a -> b) -> [a] -> [a]
func_filter f = filter $ isTruthy . f

func_select :: Concrete a => [a] -> [b] -> [b]
func_select x y = [b | (a, b) <- zip x y, isTruthy a]

func_scanl :: (b -> a -> b) -> b -> [a] -> [b]
func_scanl = scanl

func_scanr :: (a -> b -> b) -> b -> [a] -> [b]
func_scanr = scanr

func_len :: [a] -> Integer
func_len = genericLength

func_nlen :: Number a => a -> Integer
func_nlen = genericLength.show

func_index :: [a] -> Integer -> a
func_index = genericIndex

func_if :: Concrete a => a -> b -> b -> b
func_if a b c = if isTruthy a then b else c

func_not :: (Concrete a, Number n) => a -> n
func_not a = if isTruthy a then 0 else 1

func_hook :: (a -> b -> c) -> (a -> b) -> a -> c
func_hook x y z = x z (y z)

func_const :: a -> b -> a
func_const x _ = x

func_id :: a -> a
func_id x = x

func_foldl :: (b -> a -> b) -> b -> [a] -> b
func_foldl = foldl

func_foldr :: (a -> b -> b) -> b -> [a] -> b
func_foldr = foldr
