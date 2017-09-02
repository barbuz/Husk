
-- Built-in integer sequences

func_intseq :: Char -> [TNum]
--Even
func_intseq '0' = [2,4..]
--Odd
func_intseq '1' = [1,3..]
--Powers of 2
func_intseq '2' = map (2^) [1..]
--Powers of 3
func_intseq '3' = map (3^) [1..]
--Powers of 5
func_intseq '5' = map (5^) [1..]
--Powers of 7
func_intseq '7' = map (7^) [1..]
--Fibonacci
func_intseq 'f' = fibs
  where
    fibs = 1 : 1 : zipWith (+) fibs (tail fibs)
--Primes (quite efficient implementation, but not the most efficient)
func_intseq 'p' = 2 : oddprimes
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
--Powers of 10
func_intseq '⁰' = map (10^) [1..]
--Squares
func_intseq '□' = map (^2) [1..]


func_intseq c = error $ "Unimplemented integer sequence for character '"++c:"'"
