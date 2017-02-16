-- Simplified Pratt parser for expressions, adapted from http://stackoverflow.com/a/33534426

module PrattParser where

import Text.Parsec (Parsec, choice, (<|>))
import Data.List (tails)
import Control.Applicative (pure, (<*>))

data Operator u t = Postfix (Parsec String u (t -> t))
                    | InfixL (Parsec String u (t -> t -> t)) -- Left associative
                    | InfixR (Parsec String u (t -> t -> t)) -- Right associative

-- Make a Pratt parser from a precedence table and a term parser
-- Precedence table is given from highest to lowest precedence
mkPrattParser :: [[Operator u t]] -> Parsec String u t -> Parsec String u t
mkPrattParser precTable parseTerm = parseExpr precs
  where precs = reverse precTable                                                              -- We go from lowest to highest precedence
        parseExpr operators = do
          term <- parseTerm
          parseOper operators term
        parseOper operators lhs = choice stepParsers <|> return lhs                            -- Choose an operator; if fails, return lhs
          where stepParsers = do
                  newPrecs@(precLev : higherPrecs) <- tails operators                          -- Choose a precedence level and all higher levels
                  operator <- precLev                                                          -- Choose an operator from the level
                  stepParser <- case operator of                                               -- Make a "next step" parser
                    Postfix parseOp -> return $ parseOp <*> pure lhs                           -- For postfix, just grab that
                    InfixL parseOp -> return $ parseOp <*> pure lhs <*> parseExpr higherPrecs  -- For left infix, grab everything with higher precedence
                    InfixR parseOp -> return $ parseOp <*> pure lhs <*> parseExpr newPrecs     -- For right infix, grab everything with same or higher precedence
                  return $ stepParser >>= parseOper operators                                  -- Parse with "next step", then with all operators
