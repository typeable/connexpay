module Web.Connexpay.Utils where

import Data.Text (Text)
import Data.Text qualified as Text

whenLeft :: Monad m => Either a b -> (a -> m ()) -> m ()
whenLeft (Left l) f = f l
whenLeft (Right _) _ = pure ()

whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenJust (Just x) f = f x
whenJust Nothing _ = pure ()

tshow :: Show a => a -> Text
tshow = Text.pack . show
