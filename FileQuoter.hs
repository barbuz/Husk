module FileQuoter where

import Language.Haskell.TH
import Language.Haskell.TH.Quote

-- Quasi-quoter for adding files as string constants
-- Taken from https://stackoverflow.com/a/12717160/7588488
litFile :: QuasiQuoter
litFile = quoteFile $ QuasiQuoter {quoteExp = return . LitE . StringL,
                                   quotePat = undefined,
                                   quoteType = undefined,
                                   quoteDec = undefined}
