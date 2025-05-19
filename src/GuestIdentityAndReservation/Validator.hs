{-# LANGUAGE DataKinds#-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE OverloadedStrings   #-}

module GuestIdentityAndReservation.Validator where

import           Plutus.V2.Ledger.Api
import           Plutus.V2.Ledger.Contexts
import           PlutusTx
import           PlutusTx.Prelude         hiding (Semigroup(..), unless)

-- Redeemer type
data GuestRedeemer = InitialSubmit | SubmitIdentity | VerifyIdentity | ConfirmIdentity | ReserveRoom | ConfirmReservation | InitiateCheckIn | CheckOut

PlutusTx.unstableMakeIsData ''GuestRedeemer

-- Datum type
data GuestDatum = GuestDatum
  { guestAddress     :: BuiltinByteString --1
  , name             :: BuiltinByteString --2
  , passportNumber   :: BuiltinByteString --3
  , photoHash        :: BuiltinByteString --4
  , isUserVerified   :: Bool              --5
  , identityStatus   :: Bool              --6  
  , isReserved       :: Bool              --7
  , initiateCheckIn  :: Bool              --8
  , reservationStatus:: Bool              --9
  , reservationId    :: BuiltinByteString --10
  , roomId           :: BuiltinByteString --11
  , checkInDate      :: BuiltinByteString --12
  , checkOutDate     :: BuiltinByteString --13
  , adminPKH         :: PubKeyHash        --14
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

        -- 1. InitialSubmit: initial submission of guest address and admin PKH
        -- 0
        InitialSubmit ->
            traceIfFalse "Guest address already set"  (guestAddress dat == emptyByteString) &&
            traceIfFalse "Admin PKH already set"      (adminPKH dat == emptyPubKeyHash) &&

            traceIfFalse "Guest address not submitted" (guestAddress outDatum /= emptyByteString) &&
            traceIfFalse "Admin PKH not submitted"     (adminPKH outDatum /= emptyPubKeyHash) &&

            traceIfFalse "Other fields must remain unchanged"
                (  name outDatum             == name dat
                && passportNumber outDatum   == passportNumber dat
                && photoHash outDatum        == photoHash dat
                && isUserVerified outDatum   == isUserVerified dat
                && identityStatus outDatum   == identityStatus dat
                && isReserved outDatum       == isReserved dat
                && initiateCheckIn outDatum  == initiateCheckIn dat
                && reservationStatus outDatum== reservationStatus dat
                && reservationId outDatum    == reservationId dat
                && roomId outDatum           == roomId dat
                && checkInDate outDatum      == checkInDate dat
                && checkOutDate outDatum     == checkOutDate dat
                )
        -- 2. SubmitIdentity: guest submits full identity details (name, passport number, photo hash, and address)
        --1
        SubmitIdentity ->
            traceIfFalse "Not authorized admin" (txSignedBy info (adminPKH dat)) &&
            traceIfFalse "Name already set"            (name dat == emptyByteString) &&
            traceIfFalse "Passport number already set" (passportNumber dat == emptyByteString) &&
            traceIfFalse "Photo hash already set"      (photoHash dat == emptyByteString) &&

            traceIfFalse "Name not submitted"          (name outDatum /= emptyByteString) &&
            traceIfFalse "Passport not submitted"      (passportNumber outDatum /= emptyByteString) &&
            traceIfFalse "Photo hash not submitted"    (photoHash outDatum /= emptyByteString) &&

            traceIfFalse "Other fields must remain unchanged"
                (  guestAddress outDatum     == guestAddress dat
                && adminPKH outDatum         == adminPKH dat
                && isUserVerified outDatum   == isUserVerified dat
                && identityStatus outDatum   == identityStatus dat
                && isReserved outDatum       == isReserved dat
                && initiateCheckIn outDatum  == initiateCheckIn dat
                && reservationStatus outDatum== reservationStatus dat
                && reservationId outDatum    == reservationId dat
                && roomId outDatum           == roomId dat
                && checkInDate outDatum      == checkInDate dat
                && checkOutDate outDatum     == checkOutDate dat
                )



        -- 3. VerifyIdentit // Use proper redeemer index for checkOuty: admin verifies identity
        --2
        VerifyIdentity ->
            traceIfFalse "Not authorized admin" (txSignedBy info (adminPKH dat)) &&

            -- Ensure identity submission is completed before verification
            traceIfFalse "Guest address not submitted"      (guestAddress dat /= emptyByteString) &&
            traceIfFalse "Name not submitted"               (name dat /= emptyByteString) &&
            traceIfFalse "Passport number not submitted"    (passportNumber dat /= emptyByteString) &&
            traceIfFalse "Photo hash not submitted"         (photoHash dat /= emptyByteString) &&
            traceIfFalse "Admin PKH not set"                (adminPKH dat /= emptyPubKeyHash) &&

            traceIfFalse "User already verified" (isUserVerified dat == False) &&
            traceIfFalse "Verification flag not correctly updated"
                (isUserVerified outDatum == True) &&

            traceIfFalse "Other fields must remain unchanged"
                (  name outDatum             == name dat
                && guestAddress outDatum    == guestAddress dat
                && passportNumber outDatum  == passportNumber dat
                && photoHash outDatum       == photoHash dat
                && identityStatus outDatum  == identityStatus dat
                && isReserved outDatum      == isReserved dat
                && initiateCheckIn outDatum == initiateCheckIn dat
                && reservationStatus outDatum == reservationStatus dat
                && reservationId outDatum   == reservationId dat
                && roomId outDatum          == roomId dat
                && checkInDate outDatum     == checkInDate dat
                && checkOutDate outDatum    == checkOutDate dat
                && adminPKH outDatum        == adminPKH dat
                )
        --3
        ConfirmIdentity ->
            traceIfFalse "User is not verified yet" (isUserVerified dat == True) &&
            traceIfFalse "Identity status already confirmed" (identityStatus dat == False) &&
            traceIfFalse "Identity status not correctly updated" (identityStatus outDatum == True) &&

            traceIfFalse "Other fields must remain unchanged"
                (  guestAddress outDatum    == guestAddress dat
                && name outDatum            == name dat
                && passportNumber outDatum  == passportNumber dat
                && photoHash outDatum       == photoHash dat
                && isUserVerified outDatum  == isUserVerified dat
                && isReserved outDatum      == isReserved dat
                && initiateCheckIn outDatum == initiateCheckIn dat
                && reservationStatus outDatum == reservationStatus dat
                && reservationId outDatum   == reservationId dat
                && roomId outDatum          == roomId dat
                && checkInDate outDatum     == checkInDate dat
                && checkOutDate outDatum    == checkOutDate dat
                && adminPKH outDatum        == adminPKH dat
                )
 // Use proper redeemer index for checkOut

        -- 5. ReserveRoom: guest reserves a room
        --4
        ReserveRoom ->
            traceIfFalse "Not authorized admin" (txSignedBy info (adminPKH dat)) &&
            traceIfFalse "Room already reserved" (isReserved dat == False) &&

            traceIfFalse "Room ID already set"       (roomId dat == emptyByteString) &&
            traceIfFalse "Check-in date already set" (checkInDate dat == emptyByteString) &&
            traceIfFalse "Check-out date already set" (checkOutDate dat == emptyByteString) &&

            traceIfFalse "Room ID not submitted"       (roomId outDatum /= emptyByteString) &&
            traceIfFalse "Check-in date not submitted" (checkInDate outDatum /= emptyByteString) &&
            traceIfFalse "Check-out date not submitted" (checkOutDate outDatum /= emptyByteString) &&

            traceIfFalse "Reservation flag not set" (isReserved outDatum == True) &&

            traceIfFalse "Other fields must remain unchanged"
                (  guestAddress outDatum     == guestAddress dat
                && name outDatum             == name dat
                && passportNumber outDatum   == passportNumber dat
                && photoHash outDatum        == photoHash dat
                && isUserVerified outDatum   == isUserVerified dat
                && identityStatus outDatum   == identityStatus dat
                && initiateCheckIn outDatum  == initiateCheckIn dat
                && reservationStatus outDatum== reservationStatus dat
                && reservationId outDatum    == reservationId dat
                && adminPKH outDatum         == adminPKH dat
                
                )


        
                -- 6. ConfirmReservation: confirms the reservation with a reservation ID
                --5
        ConfirmReservation ->
            traceIfFalse "Reservation not yet made" (isReserved dat == True) &&

            traceIfFalse "Reservation ID already set" (reservationId dat == emptyByteString) &&
            traceIfFalse "Reservation ID not submitted" (reservationId outDatum /= emptyByteString) &&

            traceIfFalse "Reservation status not confirmed" (reservationStatus outDatum == True) &&

            traceIfFalse "Other fields must remain unchanged"
                (  guestAddress outDatum     == guestAddress dat
                && name outDatum             == name dat
                && passportNumber outDatum   == passportNumber dat
                && photoHash outDatum        == photoHash dat
                && isUserVerified outDatum   == isUserVerified dat
                && identityStatus outDatum   == identityStatus dat
                && isReserved outDatum       == isReserved dat
                && initiateCheckIn outDatum  == initiateCheckIn dat
                && roomId outDatum           == roomId dat
                && checkInDate outDatum      == checkInDate dat
                && checkOutDate outDatum     == checkOutDate dat
                && adminPKH outDatum         == adminPKH dat
                
                )
        -- 7. CheckIn Initiation: marks the start of the pre-checkin process
        --6
        InitiateCheckIn ->
            traceIfFalse "User not verified" (isUserVerified dat == True) &&
            traceIfFalse "Identity not confirmed" (identityStatus dat == True) &&
            traceIfFalse "Reservation not made" (isReserved dat == True) &&
            traceIfFalse "Reservation not confirmed" (reservationStatus dat == True) &&

            traceIfFalse "Check-in already initiated" (initiateCheckIn dat == False) &&
            traceIfFalse "Check-in flag not updated" (initiateCheckIn outDatum == True) &&

            traceIfFalse "Other fields must remain unchanged"
                (  guestAddress outDatum     == guestAddress dat
                && name outDatum             == name dat
                && passportNumber outDatum   == passportNumber dat
                && photoHash outDatum        == photoHash dat
                && isUserVerified outDatum   == isUserVerified dat
                && identityStatus outDatum   == identityStatus dat
                && isReserved outDatum       == isReserved dat
                && reservationStatus outDatum == reservationStatus dat
                && reservationId outDatum    == reservationId dat
                && roomId outDatum           == roomId dat
                && checkInDate outDatum      == checkInDate dat
                && checkOutDate outDatum     == checkOutDate dat
                    && adminPKH outDatum         == adminPKH dat
                    
                    )

        -- 8. CheckOut: user checks out
        -- 7
        CheckOut ->
            traceIfFalse "Only admin can revoke access"     (txSignedBy info (adminPKH dat)) &&
            traceIfFalse "Reservation must be active before checkout" (isReserved dat == True) &&
            traceIfFalse "Reservation must be cleared after checkout" (isReserved outDatum == False) &&

            traceIfFalse "Reservation status must be confirmed before checkout" (reservationStatus dat == True) &&
            traceIfFalse "Reservation status must be cleared after checkout" (reservationStatus outDatum == False) &&

            traceIfFalse "Check-in must have been initiated before checkout" (initiateCheckIn dat == True) &&
            traceIfFalse "Check-in flag must be cleared after checkout" (initiateCheckIn outDatum == False) &&
            traceIfFalse "Reservation ID must be present before checkout" (reservationId dat /= emptyByteString) &&
            traceIfFalse "Reservation ID must be cleared after checkout" (reservationId outDatum == emptyByteString) &&

            traceIfFalse "Room ID must be cleared after checkout" (roomId outDatum == emptyByteString) &&
            traceIfFalse "Check-in date must be cleared after checkout" (checkInDate outDatum == emptyByteString) &&
            traceIfFalse "Check-out date must be cleared after checkout" (checkOutDate outDatum == emptyByteString) &&
            -- Ensure other fields remain unchanged
            traceIfFalse "Other fields must remain unchanged after checkout"
                (  guestAddress outDatum     == guestAddress dat
                && name outDatum             == name dat
                && passportNumber outDatum   == passportNumber dat
                && photoHash outDatum        == photoHash dat
                && isUserVerified outDatum   == isUserVerified dat
                && identityStatus outDatum   == identityStatus dat
                && adminPKH outDatum         == adminPKH dat
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
