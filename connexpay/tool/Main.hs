{-# LANGUAGE ApplicativeDo #-}

module Main where

import Control.Concurrent (yield)
import Control.Monad
import Data.Aeson
import Data.Fixed
import Data.Maybe (fromMaybe)
import Data.Money
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Text.IO qualified as Text
import Data.Yaml (decodeFileThrow)
import GHC.Generics
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Options.Applicative
import Web.Connexpay.Init
import Web.Connexpay.Payments
import Web.Connexpay.Types


data ConfigYaml = ConfigYaml
  { login :: Text
  , password :: Text
  , host :: Text
  , deviceGuid :: Text
  , useTLS :: Bool
  , proxyHost :: Maybe Text
  , proxyPort :: Maybe Word
  } deriving stock Generic

instance FromJSON ConfigYaml where
  parseJSON = genericParseJSON defaultOptions
    { fieldLabelModifier = camelTo2 '_'
    }

configFromYaml :: ConfigYaml -> Config
configFromYaml ConfigYaml{..} = Config{..}

data Command = AuthSale CreditCard Centi
             | VoidAuth AuthOnlyGuid
             | VoidSale SaleGuid (Maybe Centi)
             | CaptureSale AuthOnlyGuid
             | CancelSale SaleGuid
             | ReturnSale SaleGuid (Maybe Centi)
             | TestAuth

data CmdLine = CmdLine { configPath :: FilePath
                       , operation :: Command
                       }

cmdParser :: Parser CmdLine
cmdParser = CmdLine <$> strOption (short 'c' <> metavar "FILE" <> help "Configuration file path")
                    <*> subparser operation
  where operation = command "auth" (info (AuthSale <$> cc <*> amt) (progDesc "Authorise payment"))
                 <> command "void-auth" (info (VoidAuth <$> guid) (progDesc "Void payment"))
                 <> command "void-sale" (info (VoidSale <$> guid <*> optional amt) (progDesc "Void payment"))
                 <> command "capture" (info (CaptureSale <$> guid) (progDesc "Capture payment"))
                 <> command "cancel" (info (CancelSale <$> guid) (progDesc "Cancel payment"))
                 <> command "return" (info (ReturnSale <$> guid <*> optional amt) (progDesc "return payment"))
                 <> command "test-auth" (info (pure TestAuth) (progDesc "Test token authorisation"))
        amt = argument auto (metavar "Payment amount")
        cc = CreditCard <$> argument str mempty
                        <*> fmap pure (argument str mempty)
                        <*> argument expdate mempty
                        <*> fmap pure (argument str mempty)
                        <*> pure
                          ( Just Customer
                            { address1 = "123 Test St"
                            , address2 = Nothing
                            , zip = Just "EC1A1BB"
                            } )
        expdate = do s <- str
                     guard (length s == 4)
                     pure (read (take 2 s), read (drop 2 s))
        guid = argument auto (metavar "Payment UUID")

writeLog :: Text -> IO ()
writeLog msg = Text.putStrLn ("Connexpay log: " <> msg)

main :: IO ()
main = do cmdLine <- execParser (info (cmdParser <**> helper) mempty)
          cnf :: ConfigYaml <- decodeFileThrow cmdLine.configPath
          manager <- fromMaybe newTlsManager
               $ do host <- cnf.proxyHost
                    port <- cnf.proxyPort
                    let proxy = useProxy (Proxy (Text.encodeUtf8 host) (fromIntegral port))
                        s = managerSetProxy proxy defaultManagerSettings
                    return (newManager s)
          res <- initConnexpay writeLog manager $ configFromYaml cnf
          case res of
            Left err -> putStrLn ("Error: " <> Text.unpack err)
            Right cpi -> doThing cpi Env{ logAction = writeLog, manager }
              cmdLine.operation

doThing :: Connexpay -> Env -> Command -> IO ()
doThing connexpay env = \case
  AuthSale creditCard amt -> print =<< authorisePayment connexpay env AuthRequest
    { creditCard
    , amount = Money amt
    , invoice = Just "PNRPNR"
    , vendor = Just "Typeable payment"
    }
  VoidAuth guid -> print =<<
    voidPayment connexpay env (VoidAuthorized guid)
  VoidSale guid amt -> print =<<
    voidPayment connexpay env (VoidCaptured guid (Money <$> amt))
  CaptureSale guid -> print =<<
    capturePayment connexpay env guid
  CancelSale guid -> print =<<
    cancelPayment connexpay env guid
  ReturnSale guid amt -> print =<<
    returnPayment connexpay env guid (Money <$> amt)
  TestAuth -> forever yield
