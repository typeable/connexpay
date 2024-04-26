{-# LANGUAGE OverloadedStrings #-}
module Web.Connexpay.Types where

import Web.Connexpay.Utils

import Control.Concurrent.Async
import Control.Concurrent.MVar (MVar, readMVar)
import Control.Monad.Except (MonadError, ExceptT, runExceptT, throwError)
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Aeson
import Data.Aeson.Types (typeMismatch)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.UUID (UUID)
import Network.HTTP.Client
import Network.HTTP.Req
import Network.HTTP.Types

type BearerToken = Text
type DeviceGuid = UUID
type SaleGuid = UUID

data Connexpay = Connexpay { logAction :: Text -> IO ()
                           , manager :: Manager
                           , bearerToken :: MVar BearerToken
                           , refreshAsync :: Maybe (Async ())
                           , deviceGuid :: DeviceGuid
                           , url :: Text
                           , useTLS :: Bool
                           , login :: Text
                           , password :: Text
                           }

-- | Payment failure types.
--   This type describes failures that related to either credit card being invalid,
--   client account having insufficient funds, and other non-technical conditions.
--   FIXME: this list is not exhaustive. Add more values whenever we encounter them.
data PaymentFailure = CVVFailed         -- ^ CVV verification failure
                    | CardInvalid       -- ^ Credit card details are invalid
                    | InvalidAmount     -- ^ Money amount is invalid
                    | LocalTransaction  -- ^ Special case for transactions that were registered but did't go through somehow.
                    deriving (Eq, Show)

-- | Guess failure type from HTTP code and supplied error string.
guessFailure :: Int -> Text -> Maybe PaymentFailure
guessFailure 422 "Error code D2020. CVV2 verification failed." = Just CVVFailed
guessFailure 422 "Error code D2005. Invalid Card." = Just CardInvalid
guessFailure 422 "Amount field don't allow a value greater than $999,999.99" = Just InvalidAmount
guessFailure _ _ = Nothing

-- | Error response from Connexpay
data ErrorMessage = ErrorMessage { message :: Text
                                 , errorId :: Text }

instance FromJSON ErrorMessage where
  parseJSON (Object o) = ErrorMessage <$> o .: "message"
                                      <*> o .: "errorId"
  parseJSON v = typeMismatch "ErrorMessage" v

data PaymentError = ParseError String
                  | InvalidUrl String String
                  | HttpFailure HttpExceptionContent
                  | PaymentFailure PaymentFailure
  deriving (Show)

newtype ConnexpayM a = ConnexpayM (ReaderT Connexpay (ExceptT PaymentError IO) a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader Connexpay, MonadError PaymentError)

instance MonadHttp ConnexpayM where
  handleHttpException (JsonHttpException e) = throwError (ParseError e)
  handleHttpException (VanillaHttpException (InvalidUrlException url why)) = throwError (InvalidUrl url why)
  handleHttpException (VanillaHttpException (HttpExceptionRequest _ (StatusCodeException resp bs)))
    | Just err <- decodeStrict @ErrorMessage bs
    , Just f <- guessFailure (statusCode $ responseStatus resp) err.message = throwError (PaymentFailure f)
  handleHttpException (VanillaHttpException (HttpExceptionRequest _ c)) = throwError (HttpFailure c)

  getHttpConfig = do mgr <- asks (.manager)
                     pure (defaultHttpConfig { httpConfigAltManager = Just mgr })


runConnexpay :: Connexpay -> ConnexpayM a -> IO (Either PaymentError a)
runConnexpay cp (ConnexpayM a) = runExceptT (runReaderT a cp)

runConnexpay_ :: Connexpay -> ConnexpayM a -> IO ()
runConnexpay_ cp m =
  do r <- runConnexpay cp m
     whenLeft r $ \err ->
       cp.logAction ("Uncaught Connexpay error: " <> Text.pack (show err))

bearerToken :: ConnexpayM BearerToken
bearerToken = do v <- asks (.bearerToken)
                 liftIO (readMVar v)
