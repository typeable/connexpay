{-# LANGUAGE ApplicativeDo #-}

module Web.Connexpay.Cli where

import Options.Applicative
import Web.Connexpay.Types


connexpayOpts :: Parser Config
connexpayOpts = do
  host <- option str (long "connexpay-endpoint" <> metavar "URL")
  login <- option str (long "connexpay-login" <> metavar "LOGIN")
  password <- option str (long "connexpay-password" <> metavar "PASSWORD")
  deviceGuid <- option str (long "connexpay-devguid" <> metavar "GUID")
  useHttp <- switch (long "use-http" <> help "Use plain HTTP. Insecure!")
  pure Config
    { useTLS = not useHttp
    , ..
    }
