-- Built-in functions

-- Type of numeric values
data TNum = TInt Integer
          | TDbl Double

instance Eq TNum where
  TInt a == TInt b = a == b
  TInt a == TDbl b = fromInteger a == b
  TDbl a == TInt b = a == fromInteger b
  TDbl a == TDbl b = a == b

instance Ord TNum where
  compare (TInt a) (TInt b) = compare a b
  compare (TInt a) (TDbl b) = compare (fromInteger a) b
  compare (TDbl a) (TInt b) = compare a (fromInteger b)
  compare (TDbl a) (TDbl b) = compare a b

doublify :: TNum -> TNum
doublify (TInt n) = TDbl $ fromInteger n
doublify d = d

boolToNum :: Bool -> TNum
boolToNum True = 1
boolToNum False = 0

-- Instances for TNum
instance Show TNum where
  show (TInt n) = show n
  show (TDbl d) = show d

instance Read TNum where
  readsPrec n str = case readsPrec n str :: [(Integer, String)] of
    [] -> [(TDbl d, rest) | (d, rest) <- readsPrec n str :: [(Double, String)]]
    p  -> [(TInt k, rest) | (k, rest) <- p]

instance Num TNum where
  TInt a + TInt b = TInt $ a + b
  TInt a + TDbl b = TDbl $ fromInteger a + b
  TDbl a + TInt b = TDbl $ a + fromInteger b
  TDbl a + TDbl b = TDbl $ a + b

  TInt a - TInt b = TInt $ a - b
  TInt a - TDbl b = TDbl $ fromInteger a - b
  TDbl a - TInt b = TDbl $ a - fromInteger b
  TDbl a - TDbl b = TDbl $ a - b
  
  TInt a * TInt b = TInt $ a * b
  TInt a * TDbl b = TDbl $ fromInteger a * b
  TDbl a * TInt b = TDbl $ a * fromInteger b
  TDbl a * TDbl b = TDbl $ a * b

  abs (TInt a) = TInt $ abs a
  abs (TDbl a) = TDbl $ abs a

  signum (TInt a) = TInt $ signum a
  signum (TDbl a) = TDbl $ signum a

  negate (TInt a) = TInt $ negate a
  negate (TDbl a) = TDbl $ negate a

  fromInteger = TInt

instance Real TNum where
  toRational (TInt a) = toRational a
  toRational (TDbl a) = toRational a

instance Enum TNum where
  toEnum n = TInt $ toEnum n
  
  fromEnum (TInt n) = fromEnum n
  fromEnum (TDbl n) = fromEnum n

instance Integral TNum where
  toInteger (TInt n) = n
  toInteger (TDbl d) = truncate d
  
  quotRem (TInt a) (TInt b) | (x, y) <- quotRem a b = (TInt x, TInt y)
  quotRem (TInt a) b        = quotRem (TDbl $ fromInteger a) b
  quotRem a        (TInt b) = quotRem a (TDbl $ fromInteger b)
  quotRem (TDbl a) (TDbl b) = (TInt $ truncate $ a / b, TDbl $ a - b * fromInteger (truncate $ a / b))

instance Fractional TNum where
  fromRational r = TDbl $ fromRational r

  TInt a / TInt b = TDbl $ fromInteger a / fromInteger b
  TInt a / TDbl b = TDbl $ fromInteger a / b
  TDbl a / TInt b = TDbl $ a / fromInteger b
  TDbl a / TDbl b = TDbl $ a / b

instance Floating TNum where
  pi = TDbl pi

  exp (TDbl a) = TDbl $ exp a
  exp (TInt a) = TDbl $ exp $ fromInteger a

  log (TDbl a) = TDbl $ log a
  log (TInt a) = TDbl $ log $ fromInteger a

  sqrt (TDbl a) = TDbl $ sqrt a
  sqrt (TInt a) = TDbl $ sqrt $ fromInteger a

  sin (TDbl a) = TDbl $ sin a
  sin (TInt a) = TDbl $ sin $ fromInteger a

  cos (TDbl a) = TDbl $ cos a
  cos (TInt a) = TDbl $ cos $ fromInteger a

  tan (TDbl a) = TDbl $ tan a
  tan (TInt a) = TDbl $ tan $ fromInteger a

  asin (TDbl a) = TDbl $ asin a
  asin (TInt a) = TDbl $ asin $ fromInteger a

  acos (TDbl a) = TDbl $ acos a
  acos (TInt a) = TDbl $ acos $ fromInteger a

  atan (TDbl a) = TDbl $ atan a
  atan (TInt a) = TDbl $ atan $ fromInteger a

  sinh (TDbl a) = TDbl $ sinh a
  sinh (TInt a) = TDbl $ sinh $ fromInteger a

  cosh (TDbl a) = TDbl $ cosh a
  cosh (TInt a) = TDbl $ cosh $ fromInteger a

  asinh (TDbl a) = TDbl $ asinh a
  asinh (TInt a) = TDbl $ asinh $ fromInteger a

  acosh (TDbl a) = TDbl $ acosh a
  acosh (TInt a) = TDbl $ acosh $ fromInteger a

  atanh (TDbl a) = TDbl $ atanh a
  atanh (TInt a) = TDbl $ atanh $ fromInteger a

instance RealFrac TNum where
  properFraction (TInt a) = (fromInteger a, TInt 0)
  properFraction (TDbl a) | (n, r) <- properFraction a = (n, TDbl r)
  

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
class (Show a, Read a, Eq a, Ord a, ToString a) => Concrete a where
  isTruthy :: a -> Bool
  toTruthy :: a -> TNum
  func_false :: a
  func_true :: a
  func_lt :: a -> a -> TNum
  func_gt :: a -> a -> TNum
  func_le :: a -> a -> TNum
  func_ge :: a -> a -> TNum
  func_neq :: a -> a -> TNum
  func_congr :: a -> a -> TNum
  
  func_heads :: a -> [a]
  func_tails :: a -> [a]
  
  func_eq :: a -> a -> TNum
  func_eq x y = boolToNum $ x == y
  
  func_or :: a -> a -> a
  func_or y x = if isTruthy x then x else y
  
  func_and :: a -> a -> a
  func_and y x = if isTruthy x then y else x
  
  func_read :: [Char] -> a
  func_read = read

func_or' :: (Concrete a, Concrete b) => a -> b -> TNum
func_or' x y = func_or (toTruthy x) (toTruthy y)

func_and' :: (Concrete a, Concrete b) => a -> b -> TNum
func_and' x y = func_and (toTruthy x) (toTruthy y)

instance Concrete TNum where
  isTruthy = (/= 0)
  toTruthy (TDbl d) = TInt $ roundAway d
  toTruthy n = n
  func_false = 0
  func_true = 1
  func_lt y x = max 0 $ toTruthy (y-x)
  func_gt y x = max 0 $ toTruthy (x-y)
  func_le y x = max 0 $ toTruthy (y-x+1)
  func_ge y x = max 0 $ toTruthy (x-y+1)
  func_neq y x = abs $ toTruthy (x-y)
  
  func_congr 0 0 = 1
  func_congr 0 _ = 0
  func_congr _ 0 = 0
  func_congr _ _ = 1
  
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
      
  func_congr '\0' '\0' = 1
  func_congr '\0' _    = 0
  func_congr _    '\0' = 0
  func_congr _    _    = 1
  
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
  
  func_congr [] [] = 1
  func_congr [] _  = 0
  func_congr _  [] = 0
  func_congr (x:xs) (y:ys) = if func_congr x y == 0 then 0 else func_congr xs ys
  
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
  
  func_congr (a,b) (c,d) = if func_congr a c + func_congr b d == 2 then 1 else 0
  
  func_heads (a,b) = [(c,d)|c<-func_heads a,d<-func_heads b]
  func_tails (a,b) = [(c,d)|c<-func_tails a,d<-func_tails b]

roundAway :: Double -> Integer
roundAway d = if d<0 then floor d else ceiling d


-- Built-in functions

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
func_divds b a = func_not $ func_mod b a

func_neg :: TNum -> TNum
func_neg x = -x

func_inv :: TNum -> TNum
func_inv = recip

-- Triangular numbers: sum of all numbers in [1..n]
func_trian :: TNum -> TNum
func_trian (TInt n) = TInt $ div (n*(n+1)) 2
func_trian (TDbl r) = TDbl $ r*(r+1)/2

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

func_head :: [a] -> a
func_head = head

func_last :: [a] -> a
func_last = last

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

func_index :: TNum -> [a] -> a
func_index i
  | toInteger i>0 = flip genericIndex (toInteger i-1).cycle
  | otherwise     = flip genericIndex (-toInteger i).cycle.reverse

func_index2 :: [a] -> TNum -> a
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
func_cartes = mapM id

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

func_foldl1 :: (a -> a -> a) -> [a] -> a
func_foldl1 = foldl1

func_foldr :: (a -> b -> b) -> b -> [a] -> b
func_foldr = foldr

func_foldr1 :: (a -> a -> a) -> [a] -> a
func_foldr1 = foldr1


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

func_sortby :: (a -> a -> TNum) -> [a] -> [a]
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
func_groupBy f = groupBy (\x y -> isTruthy $ f x y)

func_perms :: [a] -> [[a]]
func_perms = permutations

func_subl :: Concrete a => [a] -> [a] -> TNum
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
func_n2i n@(TInt _) = n
func_n2i (TDbl d) = TInt $ floor $ d+0.5 --round halves towards positive infinity

func_c2i :: Char -> TNum
func_c2i c | Just i <- elemIndex c "0123456789" = fromIntegral i
           | otherwise                          = 0

-- Read the first number found in the string
func_s2i :: String -> TNum
func_s2i = read . takeWhile C.isDigit . dropWhile (not . C.isDigit)

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
