module Web.Connexpay.Utils where

import Data.Char
import Data.Text (Text)
import Data.Text qualified as Text
import Network.HTTP.Client qualified as Client
import Network.HTTP.Req
import Network.HTTP.Types

whenLeft :: Monad m => Either a b -> (a -> m ()) -> m ()
whenLeft (Left l) f = f l
whenLeft (Right _) _ = pure ()

whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenJust (Just x) f = f x
whenJust Nothing _ = pure ()

tshow :: Show a => a -> Text
tshow = Text.pack . show

--responseHeaders :: HttpResponse response => response -> [Header]
--responseHeaders = Client.responseHeaders . toVanillaResponse

responseCode :: HttpResponse response => response -> Int
responseCode = statusCode . Client.responseStatus . toVanillaResponse

capitalize :: String -> String
capitalize [] = []
capitalize (x : xs) = toUpper x : xs
