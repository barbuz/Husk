
module Parser where

import Debug
import Expr
import Builtins
import PrattParser
import DecompressString
import Codepage
import Text.Parsec
import Text.Parsec.Char
import Control.Monad (forM, foldM)
import qualified Data.Map as Map
import Data.List (elemIndex)
import Data.Maybe (catMaybes)

-- Parser state
data PState = PState {varStack :: [(Maybe ELabel, Maybe ELabel)],
                      varSupply :: Int,
                      numLines :: Int,
                      unmarked :: [Int]}

-- Parser type
type Parser = Parsec String PState

-- Unwrapped parser, giving strings for errors
parseExpr :: String -> Either String [Exp [Lit Scheme]]
parseExpr str = case runParser multiline initState "" str of
  Left err -> Left $ show err
  Right val -> Right val
  where initState = PState [] 0 0 []

-- Generate a new expression variable
genVar :: String -> Parser ELabel
genVar s = do
  stat <- getState
  let var = s ++ show (varSupply stat)
  putState stat{varSupply = varSupply stat + 1}
  return $ trace' 2 ("created " ++ var) var

-- Push a block, possibly with generated expression variables
pushBlock :: (Bool, Bool) -> Parser ()
pushBlock (gen1, gen2) = do
  var1 <- if gen1
          then Just <$> genVar "x"
          else return Nothing
  var2 <- if gen2
          then Just <$> genVar "x"
          else return Nothing
  stat <- getState
  putState stat{varStack = trace' 2 ("pushed " ++ show var1 ++ ", " ++ show var2) $ (var1, var2) : varStack stat}

-- Append a block, possibly with generated expression variables
appendBlock :: (Bool, Bool) -> Parser (Maybe ELabel, Maybe ELabel)
appendBlock (gen1, gen2) = do
  var1 <- if gen1
          then Just <$> genVar "y"
          else return Nothing
  var2 <- if gen2
          then Just <$> genVar "y"
          else return Nothing
  stat <- getState
  putState stat{varStack = varStack stat ++ [(var1, var2)]}
  return $ trace' 2 ("appended " ++ show var1 ++ ", " ++ show var2) (var1, var2)

-- Create a variable at a specific stack index
putVarAt :: Int -> Parser ELabel
putVarAt ix = do
  newVar <- genVar "z"
  stat <- getState
  let (before, (var1, var2) : after) = splitAt (div ix 2) $ varStack stat
  putState $ if even ix
             then stat{varStack = before ++ (Just newVar, var2) : after}
             else stat{varStack = before ++ (var1, Just newVar) : after}
  return newVar

-- Peek at a variable from the stack; extend stack if necessary
peekVar :: Int -> Parser ELabel
peekVar ix = do
  stack <- varStack <$> getState
  let len = length stack
  if ix >= 2 * len
    then do
      vars <- forM [0 .. div ix 2 - len - 1] $ const $ appendBlock (False, False)
      newVar <-
        if even ix
        then do
          (Just var, Nothing) <- appendBlock (True, False)
          return var
        else do
          (Nothing, Just var) <- appendBlock (False, True)
          return var
      return $ message stack newVar
    else
      let (var1, var2) = stack !! div ix 2
          maybeVar = if even ix then var1 else var2
      in case maybeVar of
           Just var -> return $ message stack var
           Nothing -> do
             var <- putVarAt ix
             return $ message stack var
  where message stack var = trace' 2 ("peeked " ++ show ix ++ " from " ++ show stack ++ ", got " ++ show var) var

-- Pop a 2-variable block off the stack
popBlock :: Parser (Maybe ELabel, Maybe ELabel)
popBlock = do
  stat <- getState
  let stack = varStack stat
  putState stat{varStack = trace' 2 ("popping from " ++ show stack) $ tail stack}
  return $ head stack

-- Parse a right paren or be at end of line
rParen :: Parser ()
rParen = (char ')' >> return ()) <|> (lookAhead endOfLine >> return ()) <|> lookAhead eof

-- Eat a lone space character (not followed by sub- or superscipt)
soleSpace :: Parser ()
soleSpace = try $ char ' ' >> notFollowedBy (oneOf "⁰¹²³⁴⁵⁶⁷⁸⁹₀₁₂₃₄₅₆₇₈₉")

-- List of builtins applied to overflowing line numbers
lineFuncs :: [Exp [Lit Scheme]]
lineFuncs = [bins "argdup",
             bins "flip",
             bins "map",
             bins "zip",
             bins "hook"]

-- Parse a multiline expression, where some lines are marked with a leading space
multiline :: Parser [Exp [Lit Scheme]]
multiline = do
  lineExprs <- sepBy1 (try markedLine <|> unmarkedLine) endOfLine
  numLn <- numLines <$> getState
  unmarkeds <- unmarked <$> getState
  return $ map (updateLineNums numLn unmarkeds) lineExprs
  where markedLine = do
          soleSpace
          expr <- lineExpr
          stat <- getState
          putState stat{numLines = numLines stat + 1}
          return (True, expr)
        unmarkedLine = do
          expr <- lineExpr
          stat <- getState
          putState stat{unmarked = unmarked stat ++ [numLines stat],
                        numLines = numLines stat + 1}
          return (False, expr)
        updateLineNums numLn unmark (marked, expr) = go expr
          where numUnmark = length unmark
                go (ELine n)
                  | marked, lNum <- mod n numLn, rounds <- div n numLn =
                      case rounds of 0 -> ELine lNum
                                     k -> EApp (lineFuncs !! (k-1)) $ ELine lNum
                  | lNum <- mod n numUnmark, rounds <- div n numUnmark =
                      case rounds of 0 -> ELine (unmark !! lNum)
                                     k -> EApp (lineFuncs !! (k-1)) $ ELine (unmark !! lNum)
                go (EApp e1 e2) = EApp (go e1) (go e2)
                go (EOp e1 e2 e3) = EOp (go e1) (go e2) (go e3)
                go (EAbs name exp) = EAbs name (go exp)
                go (ELet name exp body) = ELet name (go exp) (go body)
                go e = e
                
-- Parse a line of Husk code
lineExpr :: Parser (Exp [Lit Scheme])
lineExpr = do
  state <- getState
  putState state{varStack = []}
  expr <- expression
  overflowVars <- varStack <$> getState
  lambdified <- lambdify expr $ trace' 2 ("lambdifying " ++ show expr ++ " with " ++ show overflowVars) overflowVars
  return $ trace' 2 (show lambdified) lambdified

-- Add blocks of lambdas to an expression
lambdify :: Exp [Lit Scheme] -> [(Maybe String, Maybe String)] -> Parser (Exp [Lit Scheme])
lambdify expr pairs = go expr (reverse pairs) []
  where go expr ((Just var1, Just var2) : rest) vars = do
          innerExpr <- go expr rest (vars ++ [EVar var2])
          return $ EAbs var2 $ ELet var1 (EApp (bins "flipap") $ EVar var2) innerExpr
        go expr ((Just var1, Nothing)   : rest) vars = do
          innerExpr <- go expr rest vars
          return $ EAbs var1 innerExpr
        go expr ((Nothing,   Just var2) : rest) vars = do
          innerExpr <- go expr rest (vars ++ [EVar var2])
          return $ EAbs var2 innerExpr
        go expr ((Nothing,   Nothing)   : rest) vars = do
          var <- genVar "c"
          innerExpr <- go expr rest vars
          return $ EAbs (var ++ "_") innerExpr
        go expr [] vars = return $ foldl EApp expr vars

-- Parse an expression
expression :: Parser (Exp [Lit Scheme])
expression = mkPrattParser opTable term
  where term = between (char '(') rParen expression <|> builtin <|> try float <|> integer <|> character <|> str <|> comprstr <|> intseq <|> lambda <|> try lambdaArg <|> subscript
        opTable = [[InfixL $ optional soleSpace >> return (EOp invisibleOp)]]
        invisibleOp = bins "com4 com3 com2 com app"

-- Parse a builtin
builtin :: Parser (Exp [Lit Scheme])
builtin = do
  label <- oneOf commands
  return $ cmd label

-- Parse an integer
integer :: Parser (Exp [Lit Scheme])
integer = do
  digits <- many1 digit
  return $ ELit [Value digits numType]
  where numType = Scheme [] $ CType [] $ TConc TNum

-- Parse a float
float :: Parser (Exp [Lit Scheme])
float = do
  prefix <- many digit
  char '.'
  suffix <- many digit
  case (prefix,suffix) of
    ("","") -> return $ ELit [Value "0.5" numType]
    (_ ,"") -> return $ ELit [Value (prefix ++ ".5") numType]
    ("", _) -> return $ ELit [Value ("0." ++ suffix) numType]
    (_ , _) -> return $ ELit [Value (prefix ++ "." ++ suffix) numType]
  where numType = Scheme [] $ CType [] $ TConc TNum
 
-- Parse a character
character :: Parser (Exp [Lit Scheme])
character = do
  quote <- char '\''
  coded <- anyChar
  let c :: Char
      c = toEnum $ findByte coded
  return $ ELit [Value (show c) $ Scheme [] $ CType [] $ TConc TChar]

-- Parse a string
str :: Parser (Exp [Lit Scheme])
str = do
  quote <- char '"'
  s <- content
  quote2 <- (char '"' >> return ()) <|> (lookAhead endOfLine >> return ()) <|> lookAhead eof
  return $ ELit [Value (show s) $ Scheme [] $ CType [] $ TList (TConc TChar)]
  where
    content = do
      codedText <- many $ noneOf "\"\n\\"
      plainText <- return $ map decode codedText
      maybeEscape <- optionMaybe $ char '\\' >> anyChar
      case maybeEscape of
        Nothing -> return plainText
        Just c -> do plainText2 <- content; return $ plainText ++ toEnum (findByte c) : plainText2
    decode '¶' = '\n'
    decode '¨' = '"'
    decode '¦' = '\\'
    decode  c  = toEnum $ findByte c

-- Parse a compressed string
comprstr :: Parser (Exp [Lit Scheme])
comprstr = do
  quote <- char '¨'
  s <- content
  quote2 <- (char '¨' >> return ()) <|> (lookAhead endOfLine >> return ()) <|> lookAhead eof
  return $ ELit [Value (show $ s) $ Scheme [] $CType [] $ TList (TConc TChar)]
  where
    content = do
      comprText <- many $ noneOf "¨\n"
      decomprText <- return $ decompressString comprText
      return $ map decode decomprText
    decode '¶' = '\n'
    decode  c  =  c

-- Parse an integer sequence
intseq :: Parser (Exp [Lit Scheme])
intseq = do
  iseqCommand <- char 'İ'
  seqId <- anyChar
  return $ EApp (bins "intseq") $ ELit [Value (show seqId) $ Scheme [] $ CType [] $ TConc TChar]

-- Parse a generalized lambda
lambda :: Parser (Exp [Lit Scheme])
lambda = do
  lam <- oneOf "λμξφψχ"
  let blocks = case lam of
        'λ' -> 1
        'μ' -> 2
        'ξ' -> 3
        'φ' -> 1
        'ψ' -> 2
        'χ' -> 3
  expr <- wrap expression blocks
  rParen
  return $ if lam `elem` "φψχ" then EApp (bins "fix") expr else expr
  where
    wrap parser blocks = do
      sequence $ replicate blocks $ pushBlock (False, False)
      expr <- parser
      vars <- sequence $ replicate blocks popBlock
      lambdify expr vars

-- Parse a lambda argument
lambdaArg :: Parser (Exp [Lit Scheme])
lambdaArg = do
  supStr <- try (pure <$> oneOf sups) <|> (char ' ' >> many1 (oneOf sups))
  let digits = catMaybes $ (`elemIndex` sups) <$> supStr
      supNum = foldl1 (\n d -> 10*n + d) digits
  var <- peekVar supNum
  return $ EVar var
  where sups = "⁰¹²³⁴⁵⁶⁷⁸⁹"

-- Parse a subscript; used as line numbers
subscript :: Parser (Exp [Lit Scheme])
subscript = do
  subStr <- try (pure <$> oneOf subs) <|> (char ' ' >> many1 (oneOf subs))
  let digits = catMaybes $ (`elemIndex` subs) <$> subStr
      subNum = foldl1 (\n d -> 10*n + d) digits
  return $ ELine subNum
  where subs  = "₀₁₂₃₄₅₆₇₈₉"
