module Web.Connexpay.Cli where

import Data.UUID
import Data.Text (Text)
import Options.Applicative

data ConnexpayCli = ConnexpayCli { login :: Text
                                 , password :: Text
                                 , deviceGuid :: UUID
                                 , endpoint :: Text
                                 } deriving Show

connexpayOpts :: Parser ConnexpayCli
connexpayOpts =
  ConnexpayCli <$> option str (long "connexpay login" <> metavar "LOGIN")
               <*> option str (long "connexpay password" <> metavar "PASSWORD")
               <*> option auto (long "connexpay device guid" <> metavar "GUID")
               <*> option str (long "connexpay endpoint url" <> metavar "URL")
