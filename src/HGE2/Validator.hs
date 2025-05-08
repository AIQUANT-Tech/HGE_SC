{-# LANGUAGE DataKinds#-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE OverloadedStrings   #-}

module HGE2.Validator where

import           Plutus.V2.Ledger.Api
import           Plutus.V2.Ledger.Contexts
import           PlutusTx
import           PlutusTx.Prelude         hiding (Semigroup(..), unless)


-- Redeemer type
data GuestRedeemer = SubmitIdentity | DigitalKeyGeneration | DigitalKeyValidation
PlutusTx.unstableMakeIsData ''GuestRedeemer

-- Datum type
data GuestDatum = GuestDatum
  { guestAddress          :: BuiltinByteString
  , initiateCheckIn       :: Bool
  , adminPKH              :: PubKeyHash
  , digitalKey            :: BuiltinByteString
  , isDigitalKeyValidated :: Bool
  }
PlutusTx.unstableMakeIsData ''GuestDatum


emptyPubKeyHash :: PubKeyHash
emptyPubKeyHash = PubKeyHash emptyByteString

-- Validator logic
{-# INLINABLE mkValidator #-}
mkValidator :: GuestDatum -> GuestRedeemer -> ScriptContext -> Bool
mkValidator dat red ctx =
    let
        info :: TxInfo
        info = scriptContextTxInfo ctx

        -- Output Datum
        outDatum :: GuestDatum
        outDatum = case getContinuingOutputs ctx of
            [o] -> case txOutDatum o of
                OutputDatum (Datum d) -> case PlutusTx.fromBuiltinData d of
                    Just gd -> gd
                    Nothing -> traceError "Invalid output datum"
                _ -> traceError "Expected inline datum"
            _   -> traceError "Expected exactly one continuing output"
    in
    case red of

        -- 1. SubmitIdentity sets guestAddress, adminPKH 
        SubmitIdentity ->
            traceIfFalse "Guest address already set"      (guestAddress dat == emptyByteString) &&
            traceIfFalse "Admin PKH already set"          (adminPKH dat == emptyPubKeyHash) &&


            traceIfFalse "Guest address not submitted"    (guestAddress outDatum /= emptyByteString) &&
            traceIfFalse "Admin PKH not submitted"        (adminPKH outDatum /= emptyPubKeyHash) &&

            traceIfFalse "Check-in flag not updated" (initiateCheckIn outDatum == True) &&


            traceIfFalse "Other fields must remain unchanged"
                (  isDigitalKeyValidated outDatum == isDigitalKeyValidated dat
                && digitalKey outDatum            == digitalKey dat
                )

        -- 2. DigitalKeyGeneration by admin
        DigitalKeyGeneration ->
            traceIfFalse "Not authorized admin"             (txSignedBy info (adminPKH dat)) &&
            traceIfFalse "Check-in not initiated"           (initiateCheckIn dat == True) &&
            traceIfFalse "Digital key already generated"    (digitalKey dat == emptyByteString) &&
            traceIfFalse "Digital key must be updated"      (digitalKey outDatum /= emptyByteString) &&

            traceIfFalse "Other fields must remain unchanged"
                (  guestAddress outDatum          == guestAddress dat
                && adminPKH outDatum              == adminPKH dat
                && isDigitalKeyValidated outDatum == isDigitalKeyValidated dat
                && initiateCheckIn outDatum       == initiateCheckIn dat
                )
            
           
         -- 3. DigitalKeyValidation by user or system
        DigitalKeyValidation ->
            traceIfFalse "Digital key not generated"         (digitalKey dat /= emptyByteString) &&
            traceIfFalse "Digital key must be validated"     (isDigitalKeyValidated outDatum == True) &&

            traceIfFalse "Other fields must remain unchanged"
                (  guestAddress outDatum          == guestAddress dat
                && adminPKH outDatum              == adminPKH dat
                && digitalKey outDatum            == digitalKey dat
                && initiateCheckIn outDatum       == initiateCheckIn dat
                )    
 

       
        
-- Boilerplate
{-# INLINABLE wrapped #-}
wrapped :: BuiltinData -> BuiltinData -> BuiltinData -> ()
wrapped d r c =
    check $ mkValidator
        (unsafeFromBuiltinData d)
        (unsafeFromBuiltinData r)
        (unsafeFromBuiltinData c)

validator :: Validator
validator = mkValidatorScript $$(compile [|| wrapped ||])
