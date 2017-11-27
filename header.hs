-- Header file for built-in functions and integer sequences

{-# LANGUAGE UndecidableInstances, FlexibleInstances, FlexibleContexts, BangPatterns #-}

import Data.Function (fix)
import System.Environment (getArgs)
import qualified Data.Char as C
import           Data.Char (chr,ord)
import Data.List
import qualified Data.Set as S (member, insert, singleton)
import Data.Ord (comparing)
import Data.Bits ((.&.), (.|.))
import Data.Ratio ((%), numerator, denominator)