module Debug where

import Debug.Trace

-- Debug flag
debug = False

-- Conditional debug functions
trace' :: String -> b -> b
trace' = if debug then trace else flip const

traceShow' :: (Show a) => a -> b -> b
traceShow' = if debug then traceShow else flip const
