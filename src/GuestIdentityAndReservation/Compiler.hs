{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module GuestIdentityAndReservation.Compiler (writeGuestIdentityAndReservationScript) where

import Cardano.Api
import Codec.Serialise (serialise)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS
import qualified Plutus.V2.Ledger.Api
import Cardano.Api.Shelley (PlutusScript (..))
import PlutusTx.Prelude
import Prelude (FilePath, IO)

import GuestIdentityAndReservation.Validator as GuestIdentityAndReservation

writeValidator :: FilePath -> Plutus.V2.Ledger.Api.Validator -> IO (Either (FileError ()) ())
writeValidator file = writeFileTextEnvelope @(PlutusScript PlutusScriptV2) file Nothing . PlutusScriptSerialised . SBS.toShort . LBS.toStrict . serialise . Plutus.V2.Ledger.Api.unValidatorScript

-- It copies .plutus file in the output dir
writeGuestIdentityAndReservationScript :: IO (Either (FileError ()) ())
writeGuestIdentityAndReservationScript = writeValidator "output/GuestIdentityAndReservation3.json" GuestIdentityAndReservation.validator
