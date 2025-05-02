{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
 
module Reservation.Reservation where
 
import PlutusTx
import PlutusTx.Prelude
    ( otherwise,
      Bool(True),
      Integer,
      Maybe(Nothing, Just),
      Either(Left, Right),
      ($),
      (++),
      traceError,
      traceIfFalse,
      toBuiltin,
      Eq((==)) )
import Plutus.V2.Ledger.Api
import Plutus.V2.Ledger.Contexts
 
import qualified Prelude as Haskell
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.ByteString.Short as SBS
import Cardano.Api.Shelley (PlutusScript(..), PlutusScriptV2, writeFileTextEnvelope)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)
import Codec.Serialise (serialise)
import Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as C8
import System.IO
import Prelude (show)
 
-- | Reservation Status
data ReservationStatus = Reserved | CheckedIn | CheckedOut | Cancelled
    deriving Haskell.Show
 
PlutusTx.makeLift ''ReservationStatus
 
instance ToData ReservationStatus where
    toBuiltinData Reserved   = toBuiltinData (0 :: Integer)
    toBuiltinData CheckedIn  = toBuiltinData (1 :: Integer)
    toBuiltinData CheckedOut = toBuiltinData (2 :: Integer)
    toBuiltinData Cancelled  = toBuiltinData (3 :: Integer)
 
instance FromData ReservationStatus where
    fromBuiltinData b
        | b == toBuiltinData (0 :: Integer) = Just Reserved
        | b == toBuiltinData (1 :: Integer) = Just CheckedIn
        | b == toBuiltinData (2 :: Integer) = Just CheckedOut
        | b == toBuiltinData (3 :: Integer) = Just Cancelled
        | otherwise = Nothing
 
instance UnsafeFromData ReservationStatus where
    unsafeFromBuiltinData b =
        case fromBuiltinData b of
            Just x  -> x
            Nothing -> traceError "UnsafeFromData: Failed to convert ReservationStatus"
 
-- | Reservation Datum
data Reservation = Reservation
    { resId       :: Integer
    , resGuest    :: PubKeyHash
    , resRoomId   :: Integer
    , resCheckIn  :: Integer
    , resCheckOut :: Integer
    , resStatus   :: ReservationStatus
    } deriving Haskell.Show
 
-- | Make the Reservation type ToData and FromData (automatically generated)
PlutusTx.makeIsDataIndexed ''Reservation [('Reservation, 0)]
PlutusTx.makeLift ''Reservation
 
-- ReservationAction (Actions for Reservation)
data ReservationAction = ReserveRoom PubKeyHash Reservation
                       | ConfirmReservation PubKeyHash
                       | InitiateCheckIn PubKeyHash
                       | CheckOut PubKeyHash
    deriving Haskell.Show
 
PlutusTx.makeIsDataIndexed ''ReservationAction [('ReserveRoom, 0), ('ConfirmReservation, 1), ('InitiateCheckIn, 2), ('CheckOut, 3)]
PlutusTx.makeLift ''ReservationAction
 
{-# INLINABLE mkReservationValidator #-}
mkReservationValidator :: PubKeyHash -> () -> ReservationAction -> ScriptContext -> Bool
mkReservationValidator hotelPKH _ action ctx =
    case action of
        ReserveRoom guestPKH reservation ->
            traceIfFalse "Guest must sign the reservation" (txSignedBy (scriptContextTxInfo ctx) guestPKH)
        ConfirmReservation _ ->
            traceIfFalse "Only hotel staff can confirm reservation" (txSignedBy (scriptContextTxInfo ctx) hotelPKH)
        InitiateCheckIn guestPKH ->
            txSignedBy (scriptContextTxInfo ctx) guestPKH
        CheckOut guestPKH ->
            txSignedBy (scriptContextTxInfo ctx) guestPKH
 
{-# INLINABLE wrappedReservationValidator #-}
wrappedReservationValidator :: PubKeyHash -> BuiltinData -> BuiltinData -> BuiltinData -> ()
wrappedReservationValidator hotelPKH _ r c =
    case (fromBuiltinData r, fromBuiltinData c) of
        (Just redeemer, Just context) ->
            if mkReservationValidator hotelPKH () redeemer context then () else traceError "Reservation validation failed"
        _ -> traceError "Invalid input"
 
reservationValidator :: PubKeyHash -> Validator
reservationValidator hotelPKH = mkValidatorScript $
    $$(PlutusTx.compile [|| wrappedReservationValidator ||])
    `applyCode` liftCode hotelPKH
 
-- | Save the compiled contract to file
saveReservationValidator :: Haskell.IO ()
saveReservationValidator = do
    putStrLn "Enter the public key hash (hex) for the hotel staff account:"
    pkhHex <- getLine
    putStrLn "Enter the output file path (e.g., compiled/ReservationContract.plutus):"
    outFile <- getLine
    case B16.decode (C8.pack pkhHex) of
        Right bytes -> do
            let hotelPKH = PubKeyHash (toBuiltin bytes)
            let filepath = outFile
            createDirectoryIfMissing True (takeDirectory filepath)
            let script = unValidatorScript (reservationValidator hotelPKH)
                serialized = serialise script
                shortBs = SBS.toShort (LBS.toStrict serialized)
                plutusScript = PlutusScriptSerialised shortBs :: PlutusScript PlutusScriptV2
            result <- writeFileTextEnvelope filepath Nothing plutusScript
            case result of
                Left err -> putStrLn $ "Error writing script: " ++ show err
                Right () -> putStrLn "ReservationContract compiled successfully."
        Left _ -> putStrLn "Error: Invalid hex input for PubKeyHash."
 
main :: Haskell.IO ()
main = saveReservationValidator
 