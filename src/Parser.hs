module Parser 
  ( parseColor
  , parseColors
  )
  where

import           Control.Applicative              ((<|>))
import           Data.Attoparsec.ByteString       (Parser)
import qualified Data.Attoparsec.ByteString       as AP
import qualified Data.Attoparsec.ByteString.Char8 as AP8
import           Data.Vector.Unboxed              (fromList)
import           Types                            (Color(..), Colors)

skipSpace :: Parser ()
skipSpace = AP.skipWhile AP8.isHorizontalSpace

eolOrEof :: Parser ()
eolOrEof = AP8.endOfLine <|> AP.endOfInput

parseColor :: Parser Color
parseColor = Color <$> AP8.decimal

parseColors :: Parser Colors
parseColors = do
  fromList <$> (skipSpace *> AP.many1' (parseColor <* skipSpace) <* eolOrEof)
