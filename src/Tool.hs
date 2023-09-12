{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Aeson
import Data.Text (Text)
import Data.Text.IO qualified as Text
import Data.UUID
import Data.Yaml
import GHC.Generics
import Network.HTTP.Client.TLS
import System.Environment
import Web.Connexpay

data Config = Config { login :: Text
                     , password :: Text
                     , host :: Text
                     , device_guid :: UUID
                     } deriving Generic

instance FromJSON Config

writeLog :: Text -> IO ()
writeLog msg = Text.putStrLn ("Connexpay log: " <> msg)

main :: IO ()
main = do cmdLine <- getArgs
          cnf :: Config <- decodeFileThrow (head cmdLine)
          mgr <- newTlsManager
          res <- initConnexpay writeLog mgr cnf.device_guid cnf.host cnf.login cnf.password
          case res of
            Left err -> putStrLn ("Error: " <> show err)
            Right _ -> pure ()
