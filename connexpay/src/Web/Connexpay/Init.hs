module Web.Connexpay.Init (initConnexpay) where

import Web.Connexpay.Auth
import Web.Connexpay.Types
import Web.Connexpay.Utils

import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad.Except
import Control.Monad.IO.Class
import Data.Functor
import Data.Text (Text)
import Network.HTTP.Client (Manager)
import Numeric.Natural


-- | Initialise Connexpay state. Log in, authenticate, and obtain bearer token.
initConnexpay
  :: Logger
  -> Manager
  -> Config
  -> IO (Either Text Connexpay)
initConnexpay logAction manager config = runExceptT do
  let env = Env{..}
  tokenReply <- ExceptT $ authenticate config env <&> \case
    ResponseOk tokenReply -> Right tokenReply
    notOk -> Left $ "Connexpay init error: " <> tshow notOk
  bearerToken <- liftIO $ newMVar tokenReply.token
  refreshAsync <- liftIO $ async do
    updateToken config env bearerToken tokenReply.expiresIn
  pure Connexpay{..}

updateToken :: Config -> Env -> MVar BearerToken -> Natural -> IO ()
updateToken config env tokVar w =
  liftIO (threadDelay $ fromIntegral w * 1000000) >> upd
  where
    upd = authenticate config env >>= \case
      ResponseOk tok -> do
        _ <- swapMVar tokVar tok.token
        env.logAction "Connexpay token update success"
        updateToken config env tokVar (tok.expiresIn - 5)
      notOk -> do
        env.logAction $ "Connexpay token update failure: " <> tshow notOk
        updateToken config env tokVar 5
