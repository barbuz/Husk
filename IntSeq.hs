{-# LANGUAGE UndecidableInstances, FlexibleInstances, FlexibleContexts, BangPatterns #-}

module IntSeq where
-- Built-in integer sequences

import Data.List
import Defs

func_intseq :: Char -> [TNum]

--Alternate signs
func_intseq '±' = concat $ transpose [[1..],[-1,-2..]]

--All integers
func_intseq 'Z' = 0 : func_intseq '±'

--Yes/no sequence
func_intseq '¬' = cycle [1,0]

--Negatives
func_intseq '-' = [-1,-2..]

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

--Primes (defined in Defs.hs)
func_intseq 'p' = primes_list

--Ruler sequence (exponent of highest power of 2 dividing n), OEIS A007814
func_intseq 'r' = 0:concatMap(\x->[x+1,0])(func_intseq 'r')

--Money values (1,2,5 and their multiples by 10)
func_intseq '₅' = concat $ iterate (map (*10)) [1,2,5]

--Digits of Pi. Algorithm taken from: Gibbons, Jeremy. "Unbounded spigot algorithms for the digits of pi." The American Mathematical Monthly 113.4 (2006): 318-328.
func_intseq 'π' = g(1,180,60,2) where
   g(q,r,t,i) = let (u,y)=(3*(3*i+1)*(3*i+2),div(q*(27*i-12)+5*r)(5*t))
                in y : g(10*q*i*(2*i-1),10*u*(q*(5*i-2)+r-y*t),t*u,i+1)

--Powers of 10
func_intseq '⁰' = map (10^) [1..]

--Euro coins and notes values
func_intseq '€' = [0.01,0.02,0.05,0.1,0.2,0.5,1,2,5,10,20,50,100,200,500]

--Squares
func_intseq '□' = map (^2) [1..]

--Palindromic numbers in base 10
func_intseq '↔' = filter (((==) =<< reverse) . func_base10) [1..]

--Palindromic numbers in base 2
func_intseq '↕' = filter (((==) =<< reverse) . func_base2) [1..]

--Powers of -1
func_intseq '_' = cycle [-1,1]

--Inverses of positive integers
func_intseq '\\' = map recip [1..]

--Powers of 1/2
func_intseq '½' = map (recip . (2^)) [1..]


func_intseq c = error $ "Unimplemented integer sequence for character '"++c:"'"
