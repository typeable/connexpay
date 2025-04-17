{-# LANGUAGE TemplateHaskell #-}

module Web.Connexpay.Types where

import Control.Concurrent.Async
import Control.Concurrent.MVar (MVar)
import Data.Aeson.TH
import Data.ByteString.Lazy qualified as Lazy (ByteString)
import Data.Text (Text)
import Data.UUID (UUID)
import Network.HTTP.Client (Manager)
import Network.HTTP.Types (Status)


type BearerToken = Text
type DeviceGuid = Text
type AuthOnlyGuid = UUID
type SaleGuid = UUID
type CaptureGuid = UUID
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
  , bearerToken :: MVar BearerToken
  , refreshAsync :: Async ()
  }

data Env = Env
  { logAction :: Logger
  , manager :: Manager
  }

data Response a
  = ResponseOk a
  | ResponseError ErrorMessage
  | ResponseFailure Status Lazy.ByteString
  | ResponseParseError Text
  deriving stock (Show, Functor)

data ErrorMessage = ErrorMessage
  { message :: Text
  , errorId :: Text
  } deriving stock (Show)

deriveFromJSON defaultOptions ''ErrorMessage
