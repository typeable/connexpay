{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import Data.Fixed
import Data.Maybe (fromMaybe)
import Data.Money
import Data.Text (Text)
import Data.Text.Encoding qualified as Text
import Data.Text.IO qualified as Text
import Data.UUID
import Data.Yaml (decodeFileThrow)
import GHC.Generics
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Options.Applicative
import Web.Connexpay

data Config = Config { login :: Text
                     , password :: Text
                     , host :: Text
                     , device_guid :: UUID
                     , proxy_host :: Maybe Text
                     , proxy_port :: Maybe Word
                     } deriving Generic

instance FromJSON Config

data Command = AuthSale CreditCard Centi
             | VoidSale SaleGuid
             | CaptureSale SaleGuid

data CmdLine = CmdLine { configPath :: FilePath
                       , operation :: Command
                       }

cmdParser :: Parser CmdLine
cmdParser = CmdLine <$> strOption (short 'c' <> metavar "FILE" <> help "Configuration file path")
                    <*> subparser operation
  where operation = command "auth" (info (AuthSale <$> cc <*> amt) (progDesc "Authorise payment"))
                 <> command "void" (info (VoidSale <$> guid) (progDesc "Void payment"))
                 <> command "capture" (info (CaptureSale <$> guid) (progDesc "Void payment"))
        amt = argument auto (metavar "Payment amount")
        cc = CreditCard <$> argument str mempty
                        <*> fmap pure (argument str mempty)
                        <*> argument expdate mempty
                        <*> fmap pure (argument str mempty)
        expdate = do s <- str
                     guard (length s == 4)
                     pure (read (take 2 s), read (drop 2 s))
        guid = argument auto (metavar "Payment UUID")



writeLog :: Text -> IO ()
writeLog msg = Text.putStrLn ("Connexpay log: " <> msg)

main :: IO ()
main = do cmdLine <- execParser (info cmdParser mempty)
          cnf :: Config <- decodeFileThrow cmdLine.configPath
          mgr <- fromMaybe newTlsManager
               $ do host <- cnf.proxy_host
                    port <- cnf.proxy_port
                    let proxy = useProxy (Proxy (Text.encodeUtf8 host) (fromIntegral port))
                        s = managerSetProxy proxy defaultManagerSettings
                    return (newManager s)
          res <- initConnexpay writeLog mgr cnf.device_guid cnf.host cnf.login cnf.password
          case res of
            Left err -> putStrLn ("Error: " <> show err)
            Right cpi -> print =<< runConnexpay cpi (doThing cmdLine.operation)

doThing :: Command -> ConnexpayM ()
doThing (AuthSale cc amt) = liftIO . print =<< authorisePayment cc usd
  where usd = Money amt
