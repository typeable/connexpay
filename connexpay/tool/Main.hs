{-# LANGUAGE ApplicativeDo #-}

module Main where

import Control.Concurrent (yield)
import Control.Monad
import Data.Aeson
import Data.Coerce
import Data.Maybe (fromMaybe)
import Data.Text (Text)
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

data Command = AuthSale CreditCard USD
             | VoidAuth AuthOnlyGuid
             | VoidSale SaleGuid (Maybe USD)
             | CaptureSale AuthOnlyGuid
             | CancelSale SaleGuid
             | ReturnSale SaleGuid (Maybe USD)
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
                     let (month, year) = splitAt 2 s
                     pure ExpirationDate
                       { year = read year
                       , month = read month
                       }
        guid :: (Coercible Text guid) => Parser guid
        guid = coerce @Text <$> strArgument (metavar "Payment UUID")

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
          cpi <- initConnexpay writeLog manager $ configFromYaml cnf
          doThing cpi Env{ logAction = writeLog, manager } cmdLine.operation

doThing :: Connexpay -> Env -> Command -> IO ()
doThing cp env = \case
  AuthSale card amount -> print =<< authorisePayment cp env AuthRequest
    { card
    , amount
    , orderNumber = Just "PNRPNR"
    , statementDescription = Just "Typeable payment"
    , riskData = RiskData
    }
  VoidAuth guid -> print =<< voidPayment cp env (VoidAuthorized guid)
  VoidSale guid amt -> print =<< voidPayment cp env (VoidCaptured guid amt)
  CaptureSale guid -> print =<< capturePayment cp env guid
  CancelSale guid -> print =<< cancelPayment cp env guid
  ReturnSale guid amt -> print =<< returnPayment cp env guid amt
  TestAuth -> forever yield
