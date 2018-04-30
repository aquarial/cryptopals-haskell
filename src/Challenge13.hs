module Challenge13 where

import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Data.Map             as M

type Parser = Parsec Void String

parseKV :: Parser (M.Map String String)
parseKV = foldr ins M.empty <$> oneKV `sepBy` char '&'
  where
  ins (k,v) = M.insert k v

oneKV :: Parser (String, String)
oneKV = do k <- many (letterChar <|> char '@')
           char '='
           v <- many letterChar
           pure (k,v)

c13 = print "not done"
