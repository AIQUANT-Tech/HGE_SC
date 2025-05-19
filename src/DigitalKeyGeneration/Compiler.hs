{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module DigitalKeyGeneration.Compiler (writeDigitalKeyGenerationScript) where

import Cardano.Api
import Codec.Serialise (serialise)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS
import qualified Plutus.V2.Ledger.Api
import Cardano.Api.Shelley (PlutusScript (..))
import PlutusTx.Prelude
import Prelude (FilePath, IO)

import DigitalKeyGeneration.Validator as DigitalKeyGeneration

writeValidator :: FilePath -> Plutus.V2.Ledger.Api.Validator -> IO (Either (FileError ()) ())
writeValidator file = writeFileTextEnvelope @(PlutusScript PlutusScriptV2) file Nothing . PlutusScriptSerialised . SBS.toShort . LBS.toStrict . serialise . Plutus.V2.Ledger.Api.unValidatorScript

-- It copies .plutus file in the output dir
writeDigitalKeyGenerationScript :: IO (Either (FileError ()) ())
writeDigitalKeyGenerationScript = writeValidator "output/DigitalKeyGeneration2.json" DigitalKeyGeneration.validator
