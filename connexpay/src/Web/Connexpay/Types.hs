module Web.Connexpay.Types where

import Control.Concurrent.Async
import Control.Concurrent.MVar (MVar)
import Data.Aeson hiding (Error)
import Data.Bifunctor
import Data.Text (Text)
import Network.HTTP.Client (Manager)


newtype BearerToken = BearerToken
  { unBearerToken :: Text
  } deriving newtype (Show, FromJSON, ToJSON)

newtype AuthOnlyGuid = AuthOnlyGuid
  { unAuthOnlyGuid :: Text
  } deriving newtype (Show, FromJSON, ToJSON)

newtype SaleGuid = SaleGuid
  { unSaleGuid :: Text
  } deriving newtype (Show, FromJSON, ToJSON)

newtype CaptureGuid = CaptureGuid
  { unCaptureGuid :: Text
  } deriving newtype (Show, FromJSON, ToJSON)

type Logger = Text -> IO ()

data Config = Config
  { host :: Text
  , login :: Text
  , password :: Text
  , deviceGuid :: Text
  , useTLS :: Bool
  }

data Connexpay = Connexpay
  { config :: Config
  , bearerToken :: MVar (Maybe BearerToken)
  , refreshAsync :: Async ()
  }

data Env = Env
  { logAction :: Logger
  , manager :: Manager
  }

data Response e a
  = ResponseSuccess a
  | ResponseError (Error e)
    -- ^ Response structured error (422 status code) is received
  | MissingOrExpiredToken
    -- ^ Most likely invalid credentials/URL are passed during initialization.
    -- No request is even sent to Connexpay.
  deriving stock (Show, Functor)

instance Bifunctor Response where
  bimap f = bimapResponse (fmap f)

bimapResponse
  :: (Error e -> Error e') -> (a -> b) -> Response e a -> Response e' b
bimapResponse f g = \case
  ResponseSuccess a -> ResponseSuccess (g a)
  ResponseError e -> ResponseError (f e)
  MissingOrExpiredToken -> MissingOrExpiredToken

guessResponseErrorType :: (Error () -> e) -> Response () a -> Response e a
guessResponseErrorType mkErr = bimapResponse (\e -> e { errorType = mkErr e }) id

data Error e = Error
  { message :: Text
  , errorId :: Text
  , errorType :: e
  } deriving stock (Show, Functor)

instance FromJSON (Error ()) where
  parseJSON = withObject "Error" \o -> do
    message <- o .: "message"
    errorId <- o .: "error_id"
    pure Error
      { errorType = ()
      , ..
      }
