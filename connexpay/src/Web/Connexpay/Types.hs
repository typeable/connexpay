{-# LANGUAGE OverloadedStrings #-}
module Web.Connexpay.Types where

import Web.Connexpay.Data
import Web.Connexpay.Utils

import Control.Concurrent.Async
import Control.Concurrent.MVar (MVar, readMVar)
import Control.Monad.Except (MonadError, ExceptT, runExceptT, throwError)
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Aeson
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.UUID (UUID)
import Network.HTTP.Client as Client
import Network.HTTP.Req
import Network.HTTP.Types

type BearerToken = Text
type DeviceGuid = UUID
type AuthOnlyGuid = UUID
type SaleGuid = UUID
type CaptureGuid = UUID

data Connexpay = Connexpay { logAction :: Text -> IO ()
                           , manager :: Manager
                           , bearerToken :: MVar (Maybe BearerToken)
                           , refreshAsync :: Maybe (Async ())
                           , deviceGuid :: DeviceGuid
                           , url :: Text
                           , useTLS :: Bool
                           , login :: Text
                           , password :: Text
                           }

newtype ConnexpayM a = ConnexpayM (ReaderT Connexpay (ExceptT ConnexpayError IO) a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader Connexpay, MonadError ConnexpayError)

instance MonadHttp ConnexpayM where
  handleHttpException (JsonHttpException e) = throwError (ConnectionError $ ParseError e)
  handleHttpException (VanillaHttpException (InvalidUrlException url why)) = throwError (ConnectionError $ InvalidUrl url why)
  handleHttpException (VanillaHttpException (HttpExceptionRequest _ (StatusCodeException resp bs)))
    | Just err <- decodeStrict @ErrorMessage bs
    , Just f <- guessFailure (statusCode $ responseStatus resp) err.message = throwError (PaymentFailure f (Just err.message))
  handleHttpException (VanillaHttpException (HttpExceptionRequest _ c)) = throwError (ConnectionError $ HttpFailure c)

  getHttpConfig =
    do mgr <- asks (.manager)
       log_ <- asks (.logAction)
       let cfg =
             defaultHttpConfig
               { httpConfigAltManager = Just mgr
               , httpConfigLogResponse = logResponse log_ }
       pure cfg


runConnexpay :: Connexpay -> ConnexpayM a -> IO (Either ConnexpayError a)
runConnexpay cp (ConnexpayM a) = runExceptT (runReaderT a cp)

runConnexpay_ :: Connexpay -> ConnexpayM a -> IO ()
runConnexpay_ cp m =
  do r <- runConnexpay cp m
     whenLeft r $ \err ->
       cp.logAction ("Uncaught Connexpay error: " <> Text.pack (show err))

bearerToken :: ConnexpayM (Maybe BearerToken)
bearerToken = do v <- asks (.bearerToken)
                 liftIO (readMVar v)

logResponse :: (Text -> IO ()) -> Request -> Response a -> ByteString -> IO ()
logResponse log_ _req resp body = log_ msg
  where msg = Text.unlines [ "Connexpay response:"
                           , "HTTP code: " <> tshow (statusCode (responseStatus resp))
                           , "Headers: " <> tshow (Client.responseHeaders resp)
                           , "Body: " <> tshow body
                           ]
