module Web.Connexpay.Utils where

import Data.Aeson.Types
import Data.Char
import Data.Text (Text)
import Data.Text qualified as Text


starWords :: Text -> Text
starWords = Text.unwords . map toStars . Text.words
  where toStars w = Text.replicate (Text.length w) "*"

capitalize :: String -> String
capitalize [] = []
capitalize (x : xs) = toUpper x : xs

aesonOptions :: Options
aesonOptions = defaultOptions
  { fieldLabelModifier = capitalize
  , tagSingleConstructors = True
  , omitNothingFields = True
  }
