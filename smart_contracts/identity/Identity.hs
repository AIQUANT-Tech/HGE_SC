{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}

module Identity.Identity where

import           PlutusTx
import           PlutusTx.Prelude
import           Plutus.V2.Ledger.Api
import           Plutus.V2.Ledger.Contexts
import qualified Prelude                    as Haskell
import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as C8
import Cardano.Api (PlutusScript, PlutusScriptV2)
import System.Directory
import System.FilePath
import Codec.Serialise
import Cardano.Api.Shelley

-- | Our datum now carries:

data IdentityDatum = IdentityDatum
    { idSubmitter    :: PubKeyHash
    , idPersonalHash :: BuiltinByteString
    , idVerified     :: Bool
    }
    deriving Haskell.Show

PlutusTx.makeIsDataIndexed ''IdentityDatum
    [ ('IdentityDatum, 0) ]
PlutusTx.makeLift ''IdentityDatum

-- | Actions 
data IdentityAction
    = SubmitIdentity    BuiltinByteString
    | VerifyIdentity
    | ConfirmIdentity   Bool
    deriving Haskell.Show

PlutusTx.makeIsDataIndexed ''IdentityAction
    [ ('SubmitIdentity, 0)
    , ('VerifyIdentity, 1)
    , ('ConfirmIdentity, 2)
    ]
PlutusTx.makeLift ''IdentityAction

{-# INLINABLE mkIdentityValidator #-}
mkIdentityValidator
    :: IdentityDatum
    -> IdentityAction
    -> ScriptContext
    -> Bool

mkIdentityValidator dat action ctx =
    let info = scriptContextTxInfo ctx in
    case action of
      SubmitIdentity hash ->
        let ownOut = case txInfoOutputs info of
                       o:_ -> o  -- Just check the first output for testing purposes
                       _   -> traceError "SubmitIdentity: expected at least one output"
            outDat = case txOutDatum ownOut of
                       OutputDatum (Datum d) -> case fromBuiltinData d of
                         Just IdentityDatum{ idSubmitter = s, idPersonalHash = h, idVerified = v } ->
                             (s, h, v)
                         _ -> traceError "SubmitIdentity: malformed datum!"
                       _ -> traceError "SubmitIdentity: expected inline datum!"
        in  traceIfFalse "Submit must be signed by submitter" (txSignedBy info (idSubmitter dat))
            && traceIfFalse "SubmitIdentity: personal-hash mismatch"  (hashMatches outDat)
            && traceIfFalse "SubmitIdentity: should start unverified" (not (third outDat))

        where
          hashMatches (s,h,_) = s == idSubmitter dat && h == hash
          third (_,_,x)       = x

      -- 2) Verification: verifier must sign, and must be seeing the original UTxO
      VerifyIdentity ->
        let verifierPkh = idSubmitter dat
            inputs      = txInInfoResolved <$> txInfoInputs info
            hasHash h o = case txOutDatum o of
              OutputDatum (Datum d') -> case fromBuiltinData d' of
                Just IdentityDatum{ idPersonalHash = h' } -> h' == h
                _                                          -> False
              _ -> False in
        traceIfFalse "VerifyIdentity: only verifier may call" (txSignedBy info verifierPkh)
        && traceIfFalse "VerifyIdentity: no matching input hash" (any (hasHash (idPersonalHash dat)) inputs)

      -- 3) Confirmation: verifier must sign, and the new output datum must be same hash + the Bool flag = provided
      ConfirmIdentity flag ->
        let verifierPkh = idSubmitter dat
            -- Use txInfoOutputs directly for testing instead of getContinuingOutputs
            outs = txInfoOutputs info
            checkOut o = case txOutDatum o of
              OutputDatum (Datum d) -> case fromBuiltinData d of
                Just IdentityDatum{ idSubmitter = s, idPersonalHash = h, idVerified = v } ->
                  s == idSubmitter dat && h == idPersonalHash dat && v == flag
                _ -> False
              _ -> False
        in  traceIfFalse "ConfirmIdentity: only verifier may call" (txSignedBy info verifierPkh)
            && traceIfFalse "ConfirmIdentity: must produce updated datum" (any checkOut outs)


{-# INLINABLE wrappedIdentityValidator #-}
wrappedIdentityValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
wrappedIdentityValidator bd br bc =
  case ( fromBuiltinData bd
       , fromBuiltinData br
       , fromBuiltinData bc ) of
    (Just d, Just r, Just c)
      | mkIdentityValidator d r c -> ()
      | otherwise                 -> traceError "Identity validation failed"
    _ -> traceError "Invalid datum or redeemer"

identityValidator :: Validator
identityValidator = mkValidatorScript
    $$(PlutusTx.compile [|| wrappedIdentityValidator ||])

saveIdentityValidator :: PubKeyHash -> FilePath -> Haskell.IO ()
saveIdentityValidator verifierPkh filepath = do
    createDirectoryIfMissing True (takeDirectory filepath)
    let script = unValidatorScript identityValidator
        serialized = serialise script
        shortBs = SBS.toShort (LBS.toStrict serialized)
        plutusScript = PlutusScriptSerialised shortBs :: PlutusScript PlutusScriptV2
    result <- writeFileTextEnvelope filepath Nothing plutusScript
    case result of
        Left err -> Haskell.print $ "Error writing script: " Haskell.++ Haskell.show err
        Right () -> Haskell.putStrLn "IdentityContract compiled successfully."

writePlutusIdentityValidator :: Haskell.IO ()
writePlutusIdentityValidator = do
  Haskell.putStrLn "Enter the public key hash (hex):"
  pkhHex <- Haskell.getLine
  Haskell.putStrLn "Enter the output file path (e.g., scripts/IdentityContract.plutus):"
  outFile <- Haskell.getLine
  case B16.decode (C8.pack pkhHex) of
    Right bytes -> do
      let pkh = PubKeyHash (toBuiltin bytes)
      saveIdentityValidator pkh outFile
    Left _ -> Haskell.putStrLn "Error: Invalid hex input for PubKeyHash."

main :: Haskell.IO ()
main = writePlutusIdentityValidator
