module Web.Connexpay.Init (initConnexpay) where

import Web.Connexpay.Auth
import Web.Connexpay.Data
import Web.Connexpay.Types
import Web.Connexpay.Utils

import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad (void)
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
              -> IO (Either ConnexpayError Connexpay)
initConnexpay logf mgr devguid url tls login password =
  do v <- newMVar Nothing
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
       ts <- initialAuth v logf
       a <- liftIO (async (runConnexpay_ env $ updateToken ts))
       pure (env { refreshAsync = Just a })

initialAuth :: MVar (Maybe BearerToken) -> (Text -> IO ()) -> ConnexpayM Natural
initialAuth v logf =
  do (tok, ts) <- authenticate
     liftIO $ do logf "Connexpay authentication success"
                 void (swapMVar v (Just tok))
     return ts
  `catchError` \err -> do
     liftIO (logf ("Initial connexpay authentication failed: " <> tshow err))
     -- if initial authentication failed, wait 1 second?
     -- there isn't much else to do, really.
     return 1


updateToken :: Natural -> ConnexpayM ()
updateToken w = liftIO (threadDelay w') >> upd
  where w' = fromIntegral w * 1000000
        upd = do (tok, ts) <- authenticate
                 logf <- asks (.logAction)
                 tokVar <- asks (.bearerToken)
                 liftIO (void $ swapMVar tokVar (Just tok))
                 liftIO (logf "Connexpay token update success")
                 updateToken (ts - 5)
              `catchError` \err -> do
                 logf <- asks (.logAction)
                 liftIO (logf ("Connexpay token update failure: " <> tshow err))
                 updateToken 5
