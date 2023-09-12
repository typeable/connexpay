{-# LANGUAGE OverloadedStrings #-}
module Web.Connexpay.Init where

import Web.Connexpay.Auth
import Web.Connexpay.Types

import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad
import Control.Monad.Except (catchError)
import Control.Monad.IO.Class
import Control.Monad.Reader (asks)
import Data.Text (Text)
import Data.Text qualified as Text
import Network.HTTP.Client (Manager)
import Numeric.Natural

initConnexpay :: (Text -> IO ()) -> Manager -> DeviceGuid -> Text -> Text -> Text -> IO (Either PaymentError Connexpay)
initConnexpay logf mgr devguid url login password =
  do v <- newEmptyMVar
     let env = Connexpay { logAction = logf
                         , manager = mgr
                         , bearerToken = v
                         , refreshAsync = Nothing
                         , deviceGuid = devguid
                         , url = url
                         , login = login
                         , password = password
                         }
     runConnexpay env $ do
       (tok, ts) <- authenticate
       a <- liftIO $ do logf "Connexpay authentication success"
                        putMVar v tok
                        async (void $ runConnexpay env $ updateToken ts)
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
                 liftIO (logf ("Connexpay token update failure: " <> Text.pack (show err)))
                 updateToken 5
