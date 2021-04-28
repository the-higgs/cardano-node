{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Logger.Configuration
  ( Host
  , Port
  , RemoteAddr (..)
  , Endpoint (..)
  , RotationParameters (..)
  , ScribeKind (..)
  , ScribeFormat (..)
  , ScribePrivacy (..)
  , ScribeDefinition (..)
  , Severity (..)
  , LoggerConfig (..)
  , readLoggerConfig
  ) where

import           Data.Aeson (FromJSON, ToJSON, eitherDecodeFileStrict')
import           Data.Fixed (Pico)
import           Data.Text (Text)
import           Data.Word (Word16, Word64)
import           GHC.Generics (Generic)
import qualified System.Exit as Ex

type Host = String
type Port = Int

data RemoteAddr
  = LocalPipe !FilePath
  | RemoteSocket !Host !Port
  deriving (Eq, Generic, FromJSON, Show, ToJSON)

data Endpoint = Endpoint !Host !Port
  deriving (Eq, Generic, FromJSON, Show, ToJSON)

data RotationParameters = RotationParameters
  { rpLogLimitBytes :: !Word64  -- ^ Max size of file in bytes
  , rpMaxAgeHours   :: !Word    -- ^ Hours
  , rpKeepFilesNum  :: !Word    -- ^ Number of files to keep
  } deriving (Eq, Generic, FromJSON, Show, ToJSON)

data ScribeKind
  = FileSK
  | StdoutSK
  | StderrSK
  | JournalSK
  | DevNullSK
  | UserDefinedSK
  deriving (Eq, Generic, FromJSON, Show, ToJSON)

data ScribeFormat
  = ScText
  | ScJson
  deriving (Eq, Generic, FromJSON, Show, ToJSON)

data ScribePrivacy
  = ScPublic
  | ScPrivate
  deriving (Eq, Generic, FromJSON, Show, ToJSON)

data ScribeDefinition = ScribeDefinition
  { scKind     :: !ScribeKind
  , scFormat   :: !ScribeFormat
  , scName     :: !Text
  , scPrivacy  :: !ScribePrivacy
  , scRotation :: !(Maybe RotationParameters)
  , scMinSev   :: !Severity
  , scMaxSev   :: !Severity
  } deriving (Eq, Generic, FromJSON, Show, ToJSON)

data Severity
  = Debug
  | Info
  | Notice
  | Warning
  | Error
  | Critical
  | Alert
  | Emergency
  deriving (Bounded, Enum, Eq, Generic, FromJSON, Read, Show, ToJSON)

data LoggerConfig = LoggerConfig
  { acceptAt       :: !RemoteAddr
  , setupScribes   :: ![ScribeDefinition]
  , defaultScribes :: ![(ScribeKind, Text)]
  , loRequestNum   :: !Word16 -- ^ How many 'LogObject's in one request.
  , ekgRequestFreq :: !Pico   -- ^ How often to request EKG-metrics.
  , hasEKG         :: !(Maybe Endpoint)
  , hasPrometheus  :: !(Maybe Endpoint)
  , rotation       :: !(Maybe RotationParameters)
  } deriving (Eq, Generic, FromJSON, Show, ToJSON)

-- | Reads the logger's configuration file (path is passed via '--config' CLI option).
readLoggerConfig :: FilePath -> IO LoggerConfig
readLoggerConfig pathToConfig =
  eitherDecodeFileStrict' pathToConfig >>= \case
    Left e -> Ex.die $ "Invalid logger's configuration: " <> show e
    Right (config :: LoggerConfig) -> return config
