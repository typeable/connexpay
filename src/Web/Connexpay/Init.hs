{-# LANGUAGE OverloadedStrings #-}
module Web.Connexpay.Init where

import Web.Connexpay.Auth
import Web.Connexpay.Types
import Web.Connexpay.Utils

import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad.Except (catchError)
import Control.Monad.IO.Class
import Control.Monad.Reader (asks)
import Data.Text (Text)
import Network.HTTP.Client (Manager)
import Numeric.Natural

-- | Initialise Connexpay state. Log in, authenticate, and obtain bearer token.
initConnexpay :: (Text -> IO ()) -- ^ Logging function
              -> Manager         -- ^ HTTP client manager
              -> DeviceGuid      -- ^ Device GUID. You must obtain this from ConnexPay
              -> Text            -- ^ Connexpay host to connect with
              -> Bool            -- ^ Whether to use TLS. If unsure, say True.
              -> Text            -- ^ Login name.
              -> Text            -- ^ Password.
              -> IO (Either PaymentError Connexpay)
initConnexpay logf mgr devguid url tls login password =
  do v <- newEmptyMVar
     let env = Connexpay { logAction = logf
                         , manager = mgr
                         , bearerToken = v
                         , refreshAsync = Nothing
                         , deviceGuid = devguid
                         , url = url
                         , useTLS = tls
                         , login = login
                         , password = password
                         }
     runConnexpay env $ do
       (tok, ts) <- authenticate
       a <- liftIO $ do logf "Connexpay authentication success"
                        putMVar v tok
                        async (runConnexpay_ env $ updateToken ts)
       pure (env { refreshAsync = Just a })

updateToken :: Natural -> ConnexpayM ()
updateToken w = liftIO (threadDelay w') >> upd
  where w' = fromIntegral w * 1000000
        upd = do (tok, ts) <- authenticate
                 logf <- asks (.logAction)
                 tokVar <- asks (.bearerToken)
                 liftIO (putMVar tokVar tok)
                 liftIO (logf "Connexpay token update success")
                 updateToken (ts - 5)
              `catchError` \err -> do
                 logf <- asks (.logAction)
                 liftIO (logf ("Connexpay token update failure: " <> tshow err))
                 updateToken 5
