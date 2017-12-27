module Debug where

import Debug.Trace

-- Debug level (0/1/2)
debug = 0

-- Conditional debug functions
trace' :: Int -> String -> b -> b
trace' level = if debug >= level then trace else flip const

traceShow' :: (Show a) => Int -> a -> b -> b
traceShow' level = if debug >= level then traceShow else flip const
