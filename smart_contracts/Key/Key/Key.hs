{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE NoImplicitPrelude     #-}

module Key.Key where

import           PlutusTx                       (compile, applyCode, liftCode, fromBuiltinData, makeIsDataIndexed, makeLift)
import           PlutusTx.Prelude               hiding (Semigroup(..), unless)
import           Plutus.V2.Ledger.Api           (BuiltinData, Datum(..), PubKeyHash (..), Validator, mkValidatorScript, unValidatorScript, OutputDatum (OutputDatum),TxOut,txOutAddress,txOutDatum)
import           Plutus.V2.Ledger.Contexts      (ScriptContext, getContinuingOutputs, txSignedBy, scriptContextTxInfo, txInfoInputs, txInInfoResolved, txOutDatum)
import qualified Prelude                        as Haskell
import           Cardano.Api.Shelley            (PlutusScript(..), PlutusScriptV2, writeFileTextEnvelope)
import           Codec.Serialise                (serialise)
import qualified Data.ByteString.Base16         as B16
import qualified Data.ByteString.Char8          as C8
import qualified Data.ByteString.Lazy           as LBS
import qualified Data.ByteString.Short          as SBS
import           System.Directory               (createDirectoryIfMissing)
import           System.FilePath                (takeDirectory)
import           System.IO                      (getLine, putStrLn)
import System.IO


-- | Datum
data KeyDatum = KeyDatum
    { kdGuest      :: PubKeyHash
    , kdSecretHash :: BuiltinByteString
    }
    deriving Haskell.Show

PlutusTx.makeIsDataIndexed ''KeyDatum [('KeyDatum,0)]
PlutusTx.makeLift ''KeyDatum

-- | Redeemer
data KeyAction
    = GenerateKey PubKeyHash BuiltinByteString
    | ValidateKey BuiltinByteString
    deriving Haskell.Show

PlutusTx.makeIsDataIndexed ''KeyAction [('GenerateKey,0),('ValidateKey,1)]
PlutusTx.makeLift ''KeyAction

-- | Core validator logic, parameterized by admin's PubKeyHash
{-# INLINABLE mkKeyValidator #-}
mkKeyValidator :: PubKeyHash -> KeyDatum -> KeyAction -> ScriptContext -> Bool
mkKeyValidator adminPkh dat action ctx =
    let info    = scriptContextTxInfo ctx
        inputs  = txInInfoResolved <$> txInfoInputs info
        outputs = getContinuingOutputs ctx
    in case action of
         -- Admin issues a new key UTxO for a guest
         GenerateKey guestPkh secretHash ->
           traceIfFalse "Only admin can generate key" (txSignedBy info adminPkh) &&
           case outputs of
             [o] -> case txOutDatum o of
                      OutputDatum (Datum d) -> case fromBuiltinData d of
                          Just (KeyDatum g h) -> g == guestPkh && h == secretHash
                          _                   -> False
                      _ -> False
             _   -> False

         -- Guest validates by revealing the secret preimage
         ValidateKey secretPreimage ->
           let guestPkh    = kdGuest dat
               expectedHash = kdSecretHash dat
           in traceIfFalse "Guest must sign" (txSignedBy info guestPkh) &&
              traceIfFalse "Secret mismatch" (sha2_256 secretPreimage == sha2_256 expectedHash)

-- | Wrapped validator for compilation
{-# INLINABLE wrappedValidator #-}
wrappedValidator :: PubKeyHash -> BuiltinData -> BuiltinData -> BuiltinData -> ()
wrappedValidator adminPkh bd br bc =
  case ( fromBuiltinData bd, fromBuiltinData br, fromBuiltinData bc ) of
    (Just d, Just r, Just c)
      | mkKeyValidator adminPkh d r c -> ()
      | otherwise                     -> traceError "Key validation failed"
    _ -> traceError "Invalid input"

-- | Create the parameterized Validator
validatorScript :: PubKeyHash -> Validator
validatorScript adminPkh = mkValidatorScript $
    $$(compile [|| wrappedValidator ||]) `applyCode` liftCode adminPkh

-- | Serialize and write the script to file
saveKeyValidator :: PubKeyHash -> FilePath -> Haskell.IO ()
saveKeyValidator adminPkh outFile = do
    putStrLn $ "Writing script to: " ++ outFile
    createDirectoryIfMissing True (takeDirectory outFile)
    let script     = unValidatorScript (validatorScript adminPkh)
        serialized = serialise script
        shortBs    = SBS.toShort (LBS.toStrict serialized)
        plutusScr  = PlutusScriptSerialised shortBs :: PlutusScript PlutusScriptV2
    result <- writeFileTextEnvelope outFile Nothing plutusScr
    case result of
      Left err -> putStrLn $ "Error writing script: " ++ Haskell.show err
      Right () -> putStrLn "Compiled successfully."

-- | CLI entrypoint
main :: Haskell.IO ()
main = do
    putStrLn "Enter admin PubKeyHash (hex):"
    hex <- getLine
    case B16.decode (C8.pack hex) of
      Right bs -> let adminPkh = PubKeyHash (toBuiltin bs)
                  in saveKeyValidator adminPkh "compiled/KeyContract.plutus"
      Left _   -> putStrLn "Invalid hex input!"

