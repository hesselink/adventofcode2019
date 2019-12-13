module Parser where

import Data.Maybe (listToMaybe)
import Text.ParserCombinators.ReadP (ReadP)
import qualified Text.ParserCombinators.ReadP as P
import Control.Monad (void)

type Parser = ReadP

runParser :: Parser a -> String -> Maybe a
runParser p = fmap fst . listToMaybe . filter ((== "") . snd) . P.readP_to_S p

pInt :: Parser Integer
pInt = pRead

pComma :: Parser ()
pComma = void $ P.char ','

pRead :: Read a => Parser a
pRead = P.readS_to_P reads
