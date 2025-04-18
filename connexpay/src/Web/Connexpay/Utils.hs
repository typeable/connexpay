module Web.Connexpay.Utils where

import Data.Aeson.Types
import Data.Char
import Data.Text (Text)
import Data.Text qualified as Text


whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenJust (Just x) f = f x
whenJust Nothing _ = pure ()

tshow :: Show a => a -> Text
tshow = Text.pack . show

redactWords :: Text -> Text
redactWords = Text.unwords . ("<REDACTED>" <$) . Text.words

capitalize :: String -> String
capitalize [] = []
capitalize (x : xs) = toUpper x : xs

aesonOptions :: Options
aesonOptions = defaultOptions
  { fieldLabelModifier = capitalize
  , tagSingleConstructors = True
  , omitNothingFields = True
  }
