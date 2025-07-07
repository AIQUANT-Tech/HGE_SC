{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module GuestIdentityAndReservation.Validator2 where

import           Plutus.V2.Ledger.Api
import           Plutus.V2.Ledger.Contexts
import           PlutusTx
import           PlutusTx.Prelude hiding (Semigroup(..), unless)
import GHC.Generics (Generic)
import qualified Prelude as P

-- Nested Types to Reduce Memory Footprint

data IdentityInfo = IdentityInfo
  { name            :: BuiltinByteString
  , passportNumber  :: BuiltinByteString
  , photoHash       :: BuiltinByteString
  , isUserVerified  :: Bool
  , identityStatus  :: Bool
  } deriving (P.Show, Generic, P.Eq)

PlutusTx.unstableMakeIsData ''IdentityInfo


data ReservationInfo = ReservationInfo
  { isReserved        :: Bool
  , reservationStatus :: Bool
  , reservationId     :: BuiltinByteString
  , roomId            :: BuiltinByteString
  , checkInDate       :: BuiltinByteString
  , checkOutDate      :: BuiltinByteString
  } deriving (P.Show, Generic, P.Eq)

PlutusTx.unstableMakeIsData ''ReservationInfo


data KeyInfo = KeyInfo
  { initiateCheckIn       :: Bool
  , digitalKey            :: BuiltinByteString
  , isDigitalKeyValidated :: Bool
  } deriving (P.Show, Generic, P.Eq)

PlutusTx.unstableMakeIsData ''KeyInfo



-- Unified Datum and Redeemer Types

data GuestDatum = GuestDatum
  { guestAddress :: BuiltinByteString
  , adminPKH     :: PubKeyHash
  , identity     :: IdentityInfo
  , reservation  :: ReservationInfo
  , keyInfo      :: KeyInfo
  , userId       :: BuiltinByteString
  } deriving (P.Show, Generic, P.Eq)

PlutusTx.unstableMakeIsData ''GuestDatum


data GuestRedeemer
  = FullReservation
  | FullIdentitySubmit
  | GenerateAndValidateKey
  | CheckOut
  | UpdateAddress
PlutusTx.unstableMakeIsData ''GuestRedeemer

-- Helper Functions

{-# INLINABLE unchangedExceptReservation #-}
unchangedExceptReservation :: GuestDatum -> GuestDatum -> Bool
unchangedExceptReservation d1 d2 =
     identity d1 == identity d2
  && keyInfo d1 == keyInfo d2
  && adminPKH d1 == adminPKH d2
  && guestAddress d1 == guestAddress d2
  && userId d1 == userId d2

{-# INLINABLE unchangedExceptIdentity #-}
unchangedExceptIdentity :: GuestDatum -> GuestDatum -> Bool
unchangedExceptIdentity d1 d2 =
     reservation d1 == reservation d2
  && keyInfo d1 == keyInfo d2
  && adminPKH d1 == adminPKH d2
  && guestAddress d1 == guestAddress d2
  && userId d1 == userId d2

{-# INLINABLE unchangedExceptKeyInfo #-}
unchangedExceptKeyInfo :: GuestDatum -> GuestDatum -> Bool
unchangedExceptKeyInfo d1 d2 =
     identity d1 == identity d2
  && reservation d1 == reservation d2
  && adminPKH d1 == adminPKH d2
  && guestAddress d1 == guestAddress d2
  && userId d1 == userId d2

instance Eq IdentityInfo where
  {-# INLINABLE (==) #-}
  i1 == i2 =
       name i1 == name i2
    && passportNumber i1 == passportNumber i2
    && photoHash i1 == photoHash i2
    && isUserVerified i1 == isUserVerified i2
    && identityStatus i1 == identityStatus i2

instance Eq ReservationInfo where
  {-# INLINABLE (==) #-}
  r1 == r2 =
       isReserved r1 == isReserved r2
    && reservationStatus r1 == reservationStatus r2
    && reservationId r1 == reservationId r2
    && roomId r1 == roomId r2
    && checkInDate r1 == checkInDate r2
    && checkOutDate r1 == checkOutDate r2

instance Eq KeyInfo where
  {-# INLINABLE (==) #-}
  k1 == k2 =
       initiateCheckIn k1 == initiateCheckIn k2
    && digitalKey k1 == digitalKey k2
    && isDigitalKeyValidated k1 == isDigitalKeyValidated k2


-- Validator Logic

{-# INLINABLE mkValidator #-}
mkValidator :: GuestDatum -> GuestRedeemer -> ScriptContext -> Bool
mkValidator dat red ctx =
  let
    info = scriptContextTxInfo ctx
    outDatum = case getContinuingOutputs ctx of
      [o] -> case txOutDatum o of
        OutputDatum (Datum d) -> case PlutusTx.fromBuiltinData d of
          Just gd -> gd
          Nothing -> traceError "Invalid output datum"
        _ -> traceError "Expected inline datum"
      _ -> traceError "Expected exactly one continuing output"
  in case red of

    FullReservation ->
      let rIn  = reservation dat
          rOut = reservation outDatum
      in traceIfFalse "Not admin" (txSignedBy info (adminPKH dat)) &&
         traceIfFalse "Guest address not set , need to set the guest address" (guestAddress dat /= emptyByteString) &&
         traceIfFalse "Reservation is already done" (not (isReserved rIn)) &&
         traceIfFalse "Invalid update"
           (reservationId rOut /= emptyByteString &&
            roomId rOut /= emptyByteString &&
            checkInDate rOut /= emptyByteString &&
            checkOutDate rOut /= emptyByteString &&
            isReserved rOut == True &&
            reservationStatus rOut == True) &&
         traceIfFalse "Other fields changed" (unchangedExceptReservation dat outDatum)

    FullIdentitySubmit ->
      let idIn  = identity dat
          idOut = identity outDatum
          rIn = reservation dat
      in traceIfFalse "Not admin" (txSignedBy info (adminPKH dat)) &&
         traceIfFalse "Reservation not made" (isReserved rIn == True) &&
         traceIfFalse " Name , passport number and photo hash already submitted" (name idIn == emptyByteString && passportNumber idIn == emptyByteString && photoHash idIn == emptyByteString) &&
         traceIfFalse "Incomplete update"
            (name idOut /= emptyByteString &&
             passportNumber idOut /= emptyByteString &&
             photoHash idOut /= emptyByteString &&
             isUserVerified idOut == True &&
             identityStatus idOut == True) &&
         traceIfFalse "Other fields modified" (unchangedExceptIdentity dat outDatum)

    GenerateAndValidateKey ->
      let kIn  = keyInfo dat
          kOut = keyInfo outDatum
          rIn = reservation dat
          idIn = identity dat
      in traceIfFalse "Not admin" (txSignedBy info (adminPKH dat)) &&
         traceIfFalse "Reservation not active" (isReserved rIn == True) &&
         traceIfFalse "Identity not verified" (isUserVerified idIn == True) &&
         traceIfFalse "Key already set" (digitalKey kIn == emptyByteString) &&
         traceIfFalse "Key or flags not valid"
           (digitalKey kOut /= emptyByteString &&
            isDigitalKeyValidated kOut == True &&
            initiateCheckIn kOut == True) &&
         traceIfFalse "Other fields changed" (unchangedExceptKeyInfo dat outDatum)

    CheckOut ->
      let rIn  = reservation dat
          rOut = reservation outDatum
          kOut = keyInfo outDatum
      in
        traceIfFalse "Not admin" (txSignedBy info (adminPKH dat)) &&

    -- Reservation must be cleared
        traceIfFalse "Reservation not cleared"
          ( isReserved rOut        == False &&
            reservationStatus rOut == False &&
            reservationId rOut     == emptyByteString &&
            roomId rOut            == emptyByteString &&
            checkInDate rOut       == emptyByteString &&
            checkOutDate rOut      == emptyByteString
          ) &&

        --  KeyInfo must be cleared
        traceIfFalse "Digital key not cleared"
          ( digitalKey kOut         == emptyByteString &&
            isDigitalKeyValidated kOut == False &&
            initiateCheckIn kOut    == False
          ) &&

        --  Identity and admin must remain unchanged
        traceIfFalse "Other fields changed"
          ( identity dat      == identity outDatum &&
            adminPKH dat      == adminPKH outDatum &&
            guestAddress dat  == guestAddress outDatum &&
            userId dat        == userId outDatum
          )


    UpdateAddress ->
      let newAddress = guestAddress outDatum
          oldAddress = guestAddress dat
      in traceIfFalse "Not admin" (txSignedBy info (adminPKH dat)) &&
         traceIfFalse "Address not actually changed" (newAddress /= oldAddress) &&
         traceIfFalse "Other fields modified"
           ( identity dat     == identity outDatum &&
             reservation dat  == reservation outDatum &&
             keyInfo dat      == keyInfo outDatum &&
             adminPKH dat     == adminPKH outDatum &&
             userId dat       == userId outDatum
           )

      
{-# INLINABLE wrapped #-}
wrapped d r c =
  case (fromBuiltinData d, fromBuiltinData r, fromBuiltinData c) of
    (Just dat, Just red, Just ctx) -> check $ mkValidator dat red ctx
    _ -> traceError "Invalid input data"

validator :: Validator
validator = mkValidatorScript $$(compile [|| wrapped ||])