-- Header file for built-in functions and integer sequences

{-# LANGUAGE UndecidableInstances, FlexibleInstances, FlexibleContexts #-}

import Data.Function (fix)
import System.Environment (getArgs)
import qualified Data.Char as C
import           Data.Char (chr,ord)
import Data.List
import qualified Data.Set as S (member, insert, singleton)
