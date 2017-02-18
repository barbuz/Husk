
-- Parser for recognizing types of inputs

module InputParser where

import Expr
import Infer
import Text.Parsec
import Data.List (intercalate,nub)
import Control.Monad (foldM)

type InputParser = Parsec String () (Maybe (String, Type))

unifyInputs :: Type -> Type -> Maybe Type
unifyInputs (TList t1) (TList t2) = unifyInputs t1 t2 >>= return . TList
unifyInputs t1@(TConc _) t2 | t1 == t2 = Just t1
unifyInputs (TVar _) t = Just t
unifyInputs t (TVar _) = Just t
unifyInputs _ _ = Nothing

integer :: InputParser
integer = do
  minus <- optionMaybe $ char '-'
  digits <- many1 digit
  let number = case minus of
        Just _  -> '-' : digits
        Nothing -> digits
  return $ Just (number, TConc TInt)

list :: InputParser
list = do
  char '['
  maybeElems <- sepBy inputVal (char ',')
  char ']'
  return $ do
    elems <- sequence maybeElems
    let outStr = "[" ++ intercalate "," (map fst elems) ++ "]"
    outType <- foldM unifyInputs (TVar "x") $ map snd elems
    return (outStr, TList outType)

inputVal :: InputParser
inputVal = try integer <|> list

input :: InputParser
input = do
  maybeInputVal <- inputVal
  maybeTyp <- optionMaybe $ char ':' >> inputType
  return $ case (maybeInputVal, maybeTyp) of
             (Nothing, _) -> Nothing
             (val@(Just _), Nothing) -> val
             (val, Just typ) -> do
               (str, infTyp) <- val
               newTyp <- unifyInputs infTyp typ
               return $ (str, newTyp)

inputType :: Parsec String () Type
inputType = integerT <|> varT <|> listT
  where integerT = char 'I' >> return (TConc TInt)
        varT = lower >>= \c-> return (TVar [c])
        listT = char 'L' >> fmap TList inputType

parseInput :: Int -> String -> Either String (Maybe (String, Type))
parseInput inputIndex str =
  case parse input ("input" ++ show inputIndex) str of
    Left err -> Left $ show err
    Right val -> Right val
