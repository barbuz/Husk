import Data.Function (fix)
import System.Environment (getArgs)

func_fix :: (a -> a) -> a
func_fix = fix

func_app :: (a -> b) -> a -> b
func_app = id

func_com :: (b -> c) -> (a -> b) -> a -> c
func_com = (.)

func_com2 :: (c -> d) -> (a -> b -> c) -> a -> b -> d
func_com2 f g x = f . g x

func_add :: Integer -> Integer -> Integer
func_add = (+)

func_sub :: Integer -> Integer -> Integer
func_sub  = (-)

func_neg :: Integer -> Integer
func_neg x = -x

func_mul :: Integer -> Integer -> Integer
func_mul = (*)

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

