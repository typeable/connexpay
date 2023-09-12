module Web.Connexpay.Types where

import Control.Concurrent.Async
import Control.Concurrent.MVar (MVar, readMVar)
import Control.Monad.Except (MonadError, ExceptT, runExceptT, throwError)
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Text (Text)
import Data.UUID (UUID)
import Network.HTTP.Client (Manager, HttpException(..), HttpExceptionContent)
import Network.HTTP.Req

type BearerToken = Text
type DeviceGuid = UUID
type SaleGuid = UUID

data Connexpay = Connexpay { logAction :: Text -> IO ()
                           , manager :: Manager
                           , bearerToken :: MVar BearerToken
                           , refreshAsync :: Maybe (Async ())
                           , deviceGuid :: DeviceGuid
                           , url :: Text
                           , login :: Text
                           , password :: Text
                           }

data PaymentError = AuthFailure
                  | ParseError String
                  | InvalidUrl String String
                  | HttpFailure HttpExceptionContent
  deriving (Show)


newtype ConnexpayM a = ConnexpayM (ReaderT Connexpay (ExceptT PaymentError IO) a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader Connexpay, MonadError PaymentError)

instance MonadHttp ConnexpayM where
  handleHttpException (JsonHttpException e) = throwError (ParseError e)
  handleHttpException (VanillaHttpException (InvalidUrlException url why)) = throwError (InvalidUrl url why)
  handleHttpException (VanillaHttpException (HttpExceptionRequest _ c)) = throwError (HttpFailure c)

  getHttpConfig = do mgr <- asks (.manager)
                     pure (defaultHttpConfig { httpConfigAltManager = Just mgr })


runConnexpay :: Connexpay -> ConnexpayM a -> IO (Either PaymentError a)
runConnexpay cp (ConnexpayM a) = runExceptT (runReaderT a cp)

bearerToken :: ConnexpayM BearerToken
bearerToken = do v <- asks (.bearerToken)
                 liftIO (readMVar v)
