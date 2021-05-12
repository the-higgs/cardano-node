
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Node.Orphans () where

import           Cardano.Prelude
import           Prelude (fail)

import           Cardano.Api.Orphans ()

import           Data.Aeson (FromJSON (..), ToJSON (..), ToJSONKey, Value (..), withObject, (.:))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Short as SBS
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text



import           Cardano.BM.Data.Tracer (TracingVerbosity (..))
import qualified Cardano.Chain.Update as Update
import qualified Cardano.Ledger.Alonzo as Alonzo
import qualified Cardano.Ledger.Alonzo.Language as Alonzo
import qualified Cardano.Ledger.Alonzo.PParams as Alonzo
import qualified Cardano.Ledger.Alonzo.Scripts as Alonzo
import           Cardano.Ledger.Alonzo.Translation (AlonzoGenesis (..))
import qualified Cardano.Ledger.Compactible as Ledger
import qualified Cardano.Ledger.Mary.Value as Mary
import qualified Data.MemoBytes as MemoBytes
import           Ouroboros.Consensus.Shelley.Protocol.Crypto (StandardCrypto)
import qualified Shelley.Spec.Ledger.CompactAddr as Shelley

instance FromJSON TracingVerbosity where
  parseJSON (String str) = case str of
    "MinimalVerbosity" -> pure MinimalVerbosity
    "MaximalVerbosity" -> pure MaximalVerbosity
    "NormalVerbosity" -> pure NormalVerbosity
    err -> fail $ "Parsing of TracingVerbosity failed, "
                <> Text.unpack err <> " is not a valid TracingVerbosity"
  parseJSON invalid  = fail $ "Parsing of TracingVerbosity failed due to type mismatch. "
                           <> "Encountered: " <> show invalid

deriving instance Show TracingVerbosity

deriving instance ToJSON (Alonzo.PParamsUpdate (Alonzo.AlonzoEra StandardCrypto))
deriving instance ToJSON Alonzo.ExUnits
deriving instance ToJSON Alonzo.Prices
deriving instance ToJSON Alonzo.Language
deriving instance ToJSON (MemoBytes.MemoBytes (Map ByteString Integer))

-- Plutus scripts are encoded as ShortByteString.
--TODO: I assume we are not interested in rendering
--plutus scripts as JSON.
instance ToJSON SBS.ShortByteString where
  toJSON plutusScriptBytes =
    case Base16.decode $ SBS.fromShort plutusScriptBytes of
      Right base16Bytes -> Aeson.String $ Text.decodeLatin1 base16Bytes
      Left err -> panic $ "Failed to decode plutus script: " <> Text.pack err

-- TODO: Obviously incorrect. Need to fix.
deriving instance ToJSONKey SBS.ShortByteString
-- TODO: Obviously incorrect. Need to fix.
deriving instance ToJSONKey ByteString
deriving instance ToJSONKey Alonzo.Language
deriving instance ToJSON Alonzo.CostModel
deriving instance ToJSON (MemoBytes.MemoBytes (Map Text Integer))
deriving instance FromJSON Alonzo.Prices
deriving instance FromJSON Alonzo.ExUnits

deriving instance ToJSON (Shelley.CompactAddr StandardCrypto)
deriving instance Generic (Shelley.CompactAddr StandardCrypto)

instance ToJSON (Ledger.CompactForm (Mary.Value StandardCrypto)) where
  toJSON valCompForm = Aeson.String . Text.pack $ show valCompForm

instance FromJSON Update.ApplicationName where
  parseJSON (String x) = pure $ Update.ApplicationName x
  parseJSON invalid  =
    fail $ "Parsing of application name failed due to type mismatch. "
    <> "Encountered: " <> show invalid

-- We defer parsing of the cost model so that we can
-- read it as a filepath. This is to reduce further pollution
-- of the genesis file.
instance FromJSON AlonzoGenesis where
  parseJSON = withObject "Alonzo Genesis" $ \o -> do
    adaPerWord <-  o .: "adaPerUTxOWord"
    execPrices <-  o .: "executionPrices"
    maxTxExUnits' <-  o .: "maxTxExUnits"
    maxBlockExUnits' <-  o .: "maxBlockExUnits"
    maxMaSize <-  o .: "maxMultiAssetSize"
    return $ AlonzoGenesis
               { adaPerUTxOWord = adaPerWord
               , costmdls = mempty
               , prices = execPrices
               , maxTxExUnits = maxTxExUnits'
               , maxBlockExUnits = maxBlockExUnits'
               , maxValSize = maxMaSize
               }
