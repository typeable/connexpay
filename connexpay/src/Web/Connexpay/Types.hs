module Web.Connexpay.Types where

import Control.Concurrent.Async
import Control.Concurrent.MVar (MVar)
import Data.Aeson hiding (Error)
import Data.Bifunctor
import Data.ByteString.Lazy qualified as Lazy (ByteString)
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
  | BadRequest Lazy.ByteString
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
  BadRequest body -> BadRequest body
  MissingOrExpiredToken -> MissingOrExpiredToken

guessErrorType :: (Error () -> e) -> Error () -> Error e
guessErrorType mkErr e = e { errorType = mkErr e }

data Error e = Error
  { message :: Text
  , errorId :: Text
  , errorType :: e
  } deriving stock (Show, Functor)

instance FromJSON (Error ()) where
  parseJSON = withObject "Error" \o -> do
    message <- o .: "message"
    errorId <- o .: "errorId"
    pure Error
      { errorType = ()
      , ..
      }
