
-- Parser for recognizing types of inputs

module InputParser where

import Expr
import Infer
import Debug
import Text.Parsec
import Data.List (intercalate,nub)
import Control.Monad (foldM)

type InputParser = Parsec String () (Maybe (String, Type))

unifyInputs :: Type -> Type -> Maybe Type
unifyInputs t1 t2 | trace' 2 ("unifying input types " ++ show t1 ++ " and " ++ show t2) False = undefined
unifyInputs (TPair t1 t2) (TPair s1 s2) = do
  r1 <- unifyInputs t1 s1
  r2 <- unifyInputs t2 s2
  return $ TPair r1 r2
unifyInputs (TList t1) (TList t2) = unifyInputs t1 t2 >>= return . TList
unifyInputs t1@(TConc _) t2 | t1 == t2 = Just t1
unifyInputs (TVar _) t = Just t
unifyInputs t (TVar _) = Just t
unifyInputs _ _ = Nothing

number :: InputParser
number = do
  minus <- optionMaybe $ char '-'
  prefix <- many1 digit
  suffix <- optionMaybe $ do
    sep <- oneOf "./"
    rest <- many1 digit
    return (sep:rest)
  let number = case (minus, suffix) of
        (Just _, Just suffix)  -> '-' : prefix ++ suffix
        (Just _, Nothing)      -> '-' : prefix
        (Nothing, Just suffix) -> prefix ++ suffix
        (Nothing, Nothing)     -> prefix
  return $ Just (number, TConc TNum)

character :: InputParser
character = do
  char '\''
  c <- noneOf "\\'" <|> (fmap (\c -> if c == 'n' then '\n' else c) $ char '\\' >> oneOf "\\'n")
  char '\''
  return $ Just (show c, TConc TChar)

plainStr :: InputParser
plainStr = do
  chars <- many $ noneOf "\\\"" <|> (fmap (\c -> if c == 'n' then '\n' else c) $ char '\\' >> oneOf "\\\"n")
  return $ Just (show chars, TList (TConc TChar))

str :: InputParser
str = do
  char '"'
  contents <- plainStr
  char '"'
  return contents

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

pair :: InputParser
pair = do
  char '('
  elem1 <- inputVal
  char ','
  elem2 <- inputVal
  char ')'
  return $ do
    (str1, typ1) <- elem1
    (str2, typ2) <- elem2
    return ("(" ++ str1 ++ "," ++ str2 ++ ")", TPair typ1 typ2)

inputVal :: InputParser
inputVal = try number <|> try character <|> try list <|> try pair <|> str

input :: InputParser
input = do
  maybeInputVal <- inputVal
  maybeTyp <- optionMaybe $ char ':' >> inputType
  eof
  return $ case (maybeInputVal, maybeTyp) of
             (Nothing, _) -> Nothing
             (val@(Just _), Nothing) -> val
             (val, Just typ) -> do
               (str, infTyp) <- val
               newTyp <- unifyInputs infTyp typ
               return $ (str, newTyp)

inputType :: Parsec String () Type
inputType = numT <|> charT <|> varT <|> listT <|> pairT
  where numT  = char 'N' >> return (TConc TNum)
        charT = char 'C' >> return (TConc TChar)
        varT  = lower >>= \c-> return (TVar [c])
        listT = char 'L' >> fmap TList inputType
        pairT = char 'P' >> do
          first <- inputType
          second <- inputType
          return $ TPair first second

parseInput :: Int -> String -> Either String (Maybe (String, Type))
parseInput inputIndex str =
  case parse (try input <|> plainStr) ("input" ++ show inputIndex) str of
    Left err -> Left $ show err
    Right val -> Right $ trace' 1 ("input " ++ show inputIndex ++ ", " ++ str ++ ", is " ++ show val) val
