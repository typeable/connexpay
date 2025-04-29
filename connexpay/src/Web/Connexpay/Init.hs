module Web.Connexpay.Init (initConnexpay) where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception.Safe
import Control.Monad
import Data.Aeson
import Data.Aeson.Text
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Lazy qualified as Lazy.Text
import Network.HTTP.Client (Manager)
import Numeric.Natural

import Web.Connexpay.Auth
import Web.Connexpay.Types


-- | Initialise Connexpay state. Log in, authenticate, and obtain bearer token.
initConnexpay
  :: Logger
  -> Manager
  -> Config
  -> IO Connexpay
initConnexpay logAction manager config = do
  bearerToken <- newMVar Nothing
  let env = Env{..}
  refreshAsync <- async $ forever $
    updateToken config env bearerToken 0 `catchAny` \err -> do
      doLog env $
        "Connexpay token update exception: " <> Text.pack (displayException err)
      threadDelay 5_000_000
  pure Connexpay{..}

updateToken :: Config -> Env -> MVar (Maybe BearerToken) -> Natural -> IO ()
updateToken config env tokVar w =
  threadDelay (fromIntegral w * 1_000_000) >> upd
  where
    upd = authenticate config env >>= \case
      Authorized tok -> do
        _ <- swapMVar tokVar (Just tok.token)
        updateToken config env tokVar (tok.expiresIn - 5)
      _ -> do
        updateToken config env tokVar 5

doLog :: Env -> Text -> IO ()
doLog env msg = env.logAction $ Lazy.Text.toStrict $ encodeToLazyText @Object $
  "connexpay" .= ("auth" .= msg :: Object)
