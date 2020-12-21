
-- Implementation of the Husk code page

module Codepage where

import Data.Word
import Data.List

-- The Husk code page: 256 characters, each representing one byte
codepage :: String
codepage = "¤½↕↑↓↔←→∟¦\n¡¿‼…‰†‡√≤≥±∂∫∞≈≠≡⌐¬÷×" ++
           " !\"#$%&'()*+,-./0123456789:;<=>?" ++
           "@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_" ++
           "`abcdefghijklmnopqrstuvwxyz{|}~·" ++
           "₀₁₂₃₄₅₆₇₈₉⌈⌉⌊⌋ΓΔΘΛΞΠΣΦΨΩαβγδεζηθ" ++
           "λμξπρςστφχψω⁰¹²³⁴⁵⁶⁷⁸⁹¢£€¥ƒ´▲▼►◄" ++
           "§ȦḂĊḊĖḞĠḢİĿṀṄȮṖṘṠṪẆẊẎŻȧḃċḋėḟġḣıȷ" ++
           "ŀṁṅȯṗṙṡṫẇẋẏż¨ÄËÏÖÜŸØäëïöüÿø◊□¶«»"


-- A short ASCII alias for each non-ASCII char in the code page
aliases :: [(Char, String)]
aliases = [('¤', "cur"), ('½', "hlf"), ('↕', "ud"),  ('↑', "up"),  ('↓', "dow"), ('↔', "lr"),  ('←', "lft"), ('→', "rgt"),
           ('∟', "ang"), ('¦', "bar"), ('¡', "exc"), ('¿', "que"), ('‼', "dex"), ('…', "ell"), ('‰', "ppm"),
           ('†', "dag"), ('‡', "ddg"), ('√', "srd"), ('≤', "leq"), ('≥', "geq"), ('±', "pm"),  ('∂', "ptl"), ('∫', "int"),
           ('∞', "inf"), ('≈', "apx"), ('≠', "neq"), ('≡', "cng"), ('⌐', "gen"), ('¬', "neg"), ('÷', "div"), ('×', "eks"),
           ('·', "blt"),
           ('₀', "nul"), ('₁', "one"), ('₂', "two"), ('₃', "tre"), ('₄', "for"), ('₅', "fiv"), ('₆', "six"), ('₇', "sev"),
           ('₈', "ate"), ('₉', "nin"), ('⌈', "lce"), ('⌉', "rce"), ('⌊', "lfl"), ('⌋', "rfl"), ('Γ', "Gam"), ('Δ', "Del"),
           ('Θ', "The"), ('Λ', "Lam"), ('Ξ', "Xi"),  ('Π', "Pi"),  ('Σ', "Sig"), ('Φ', "Phi"), ('Ψ', "Psi"), ('Ω', "Ohm"),
           ('α', "alp"), ('β', "bet"), ('γ', "gam"), ('δ', "del"), ('ε', "eps"), ('ζ', "zet"), ('η', "eta"), ('θ', "the"),
           ('λ', "lam"), ('μ', "mu"),  ('ξ', "xi"),  ('π', "pi"),  ('ρ', "rho"), ('ς', "sig"), ('σ', "sjg"), ('τ', "tau"),
           ('φ', "phi"), ('χ', "chi"), ('ψ', "psi"), ('ω', "ohm"), ('⁰', "Nul"), ('¹', "One"), ('²', "Two"), ('³', "Tre"),
           ('⁴', "For"), ('⁵', "Fiv"), ('⁶', "Six"), ('⁷', "Sev"), ('⁸', "Ate"), ('⁹', "Nin"), ('¢', "cnt"), ('£', "gbp"),
           ('€', "eur"), ('¥', "yen"), ('ƒ', "fl"),  ('´', "acu"), ('▲', "top"), ('▼', "bot"), ('►', "est"), ('◄', "wst"),
           ('§', "sec"), ('Ȧ', "dA"),  ('Ḃ', "dB"),  ('Ċ', "dC"),  ('Ḋ', "dD"),  ('Ė', "dE"),  ('Ḟ', "dF"),  ('Ġ', "dG"),
           ('Ḣ', "dH"),  ('İ', "dI"),  ('Ŀ', "dL"),  ('Ṁ', "dM"),  ('Ṅ', "dN"),  ('Ȯ', "dO"),  ('Ṗ', "dP"),  ('Ṙ', "dR"),
           ('Ṡ', "dS"),  ('Ṫ', "dT"),  ('Ẇ', "dW"),  ('Ẋ', "dX"),  ('Ẏ', "dY"),  ('Ż', "dZ"),  ('ȧ', "da"),  ('ḃ', "db"),
           ('ċ', "dc"),  ('ḋ', "dd"),  ('ė', "de"),  ('ḟ', "df"),  ('ġ', "dg"),  ('ḣ', "dh"),  ('ı', "di"),  ('ȷ', "dj"),
           ('ŀ', "dl"),  ('ṁ', "dm"),  ('ṅ', "dn"),  ('ȯ', "do"),  ('ṗ', "dp"),  ('ṙ', "dr"),  ('ṡ', "ds"),  ('ṫ', "dt"),
           ('ẇ', "dw"),  ('ẋ', "dx"),  ('ẏ', "dy"),  ('ż', "dz"),  ('¨', "die"), ('Ä', "DA"),  ('Ë', "DE"),  ('Ï', "DI"),
           ('Ö', "DO"),  ('Ü', "DU"),  ('Ÿ', "DY"),  ('Ø', "sO"),  ('ä', "Da"),  ('ë', "De"),  ('ï', "Di"),  ('ö', "Do"),
           ('ü', "Du"),  ('ÿ', "Dy"),  ('ø', "so"),  ('◊', "loz"), ('□', "squ"), ('¶', "pgf"), ('«', "ll"),  ('»', "rr")]

-- Convert a list of bytes into a string using the code page
getCommands :: [Word8] -> String
getCommands = map $ (codepage !!) . fromEnum

-- Get the position of a character in the code page
findByte :: Char -> Int
findByte byte | Just ix <- elemIndex byte codepage = ix
              | otherwise = error "Bad byte"

-- Convert a program to list of bytes
getBytes :: String -> [Word8]
getBytes = map $ toEnum . findByte

-- Get the alias of a character
getAlias :: Char -> String
getAlias c | Just str <- lookup c aliases = '\\' : str ++ "\\"
           | c == '\\'  = "\\\\"
           | otherwise = [c]

-- Convert terse program to verbose program
toAliases :: String -> String
toAliases = concatMap getAlias

-- Get the character of an alias
fromAlias :: String -> Maybe Char
fromAlias "" = Just '\\'
fromAlias str = lookup str $ map (\(a,b) -> (b,a)) aliases

-- Convert verbose program to terse program
parseAliases :: String -> Either String String
parseAliases ('\\':str) = do
  let (alias, rest) = break (== '\\') str
      char = case (rest, fromAlias alias) of
        ([], _)            -> Left $ "Parse error (missing \\): " ++ alias
        (_ : rest, Just c) -> Right c
        (_, Nothing)       -> Left $ "No character with alias " ++ alias
  
  (:) <$> char <*> parseAliases (drop 1 rest)
parseAliases (char : str) = (char :) <$> parseAliases str
parseAliases "" = Right ""
