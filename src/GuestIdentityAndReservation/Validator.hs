-- {-# LANGUAGE DataKinds#-}
-- {-# LANGUAGE NoImplicitPrelude   #-}
-- {-# LANGUAGE TemplateHaskell     #-}
-- {-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE TypeFamilies        #-}
-- {-# LANGUAGE OverloadedStrings   #-}

-- module GuestIdentityAndReservation.Validator where

-- import           Plutus.V2.Ledger.Api
-- import           Plutus.V2.Ledger.Contexts
-- import           PlutusTx
-- import         PlutusTx.Prelude         hiding (Semigroup(..), unless)

-- -- Redeemer type
-- data GuestRedeemer = InitialSubmit | FullIdentitySubmit | FullReservation | CheckOut 

-- PlutusTx.unstableMakeIsData ''GuestRedeemer

-- -- Datum type
-- data GuestDatum = GuestDatum
--   { guestAddress     :: BuiltinByteString --1
--   , name             :: BuiltinByteString --2
--   , passportNumber   :: BuiltinByteString --3
--   , photoHash        :: BuiltinByteString --4
--   , isUserVerified   :: Bool              --5
--   , identityStatus   :: Bool              --6  
--   , isReserved       :: Bool              --7
--   , reservationStatus:: Bool              --8
--   , reservationId    :: BuiltinByteString --9
--   , roomId           :: BuiltinByteString --10
--   , checkInDate      :: BuiltinByteString --11
--   , checkOutDate     :: BuiltinByteString --12
--   , adminPKH         :: PubKeyHash        --13
--   }
-- PlutusTx.unstableMakeIsData ''GuestDatum



-- emptyPubKeyHash :: PubKeyHash
-- emptyPubKeyHash = PubKeyHash emptyByteString

-- -- Validator logic
-- {-# INLINABLE mkValidator #-}
-- mkValidator :: GuestDatum -> GuestRedeemer -> ScriptContext -> Bool
-- mkValidator dat red ctx =
--     let
--         info :: TxInfo
--         info = scriptContextTxInfo ctx

--         -- Output Datum
--         outDatum :: GuestDatum
--         outDatum = case getContinuingOutputs ctx of
--             [o] -> case txOutDatum o of
--                 OutputDatum (Datum d) -> case PlutusTx.fromBuiltinData d of
--                     Just gd -> gd
--                     Nothing -> traceError "Invalid output datum"
--                 _ -> traceError "Expected inline datum"
--             _   -> traceError "Expected exactly one continuing output"
--     in
--     case red of

--         -- 1. InitialSubmit: initial submission of guest address and admin PKH
     
--         InitialSubmit ->
--             traceIfFalse "Guest address already set"  (guestAddress dat == emptyByteString) &&
--             traceIfFalse "Admin PKH already set"      (adminPKH dat == emptyPubKeyHash) &&

--             traceIfFalse "Guest address not submitted" (guestAddress outDatum /= emptyByteString) &&
--             traceIfFalse "Admin PKH not submitted"     (adminPKH outDatum /= emptyPubKeyHash) &&

--             traceIfFalse "Other fields must remain unchanged"
--                 (  name outDatum             == name dat
--                 && passportNumber outDatum   == passportNumber dat
--                 && photoHash outDatum        == photoHash dat
--                 && isUserVerified outDatum   == isUserVerified dat
--                 && identityStatus outDatum   == identityStatus dat
--                 && isReserved outDatum       == isReserved dat
--                 && reservationStatus outDatum== reservationStatus dat
--                 && reservationId outDatum    == reservationId dat
--                 && roomId outDatum           == roomId dat
--                 && checkInDate outDatum      == checkInDate dat
--                 && checkOutDate outDatum     == checkOutDate dat
--                 )
       


        
--         -- FullIdentitySubmit: submits identity, verifies it, and confirms
--         FullIdentitySubmit ->
--                traceIfFalse "Not authorized admin" (txSignedBy info (adminPKH dat)) &&

--     -- Preconditions: ensure all identity fields are unset
--                traceIfFalse "Name already set"             (name dat == emptyByteString) &&
--                traceIfFalse "Passport number already set"  (passportNumber dat == emptyByteString) &&
--                traceIfFalse "Photo hash already set"       (photoHash dat == emptyByteString) &&
--                traceIfFalse "User already verified"        (isUserVerified dat == False) &&
--                traceIfFalse "Identity already confirmed"   (identityStatus dat == False) &&
--                traceIfFalse "Admin PKH not set"            (adminPKH dat /= emptyPubKeyHash) &&

--     -- Postconditions: fields must be submitted and flags set
--                traceIfFalse "Name not submitted"           (name outDatum /= emptyByteString) &&
--                traceIfFalse "Passport not submitted"       (passportNumber outDatum /= emptyByteString) &&
--                traceIfFalse "Photo hash not submitted"     (photoHash outDatum /= emptyByteString) &&
--                traceIfFalse "User not verified"            (isUserVerified outDatum == True) &&
--                traceIfFalse "Identity not confirmed"       (identityStatus outDatum == True) &&

--     -- Ensure all other fields remain unchanged
--                traceIfFalse "Other fields must remain unchanged"
--                     (  guestAddress outDatum     == guestAddress dat
--                     && adminPKH outDatum         == adminPKH dat
--                     && isReserved outDatum       == isReserved dat
--                     && reservationStatus outDatum== reservationStatus dat
--                     && reservationId outDatum    == reservationId dat
--                     && roomId outDatum           == roomId dat
--                     && checkInDate outDatum      == checkInDate dat
--                     && checkOutDate outDatum     == checkOutDate dat
--                     )



--         -- 4. FullReservation: reserve room + confirm it in one call
--         FullReservation ->
--             traceIfFalse "Not authorized admin" (txSignedBy info (adminPKH dat)) &&

--     -- Preconditions
--             traceIfFalse "Room already reserved"            (isReserved dat == False) &&
--             traceIfFalse "Reservation ID already set"       (reservationId dat == emptyByteString) &&

--             traceIfFalse "Room ID already set"              (roomId dat == emptyByteString) &&
--             traceIfFalse "Check-in date already set"        (checkInDate dat == emptyByteString) &&
--             traceIfFalse "Check-out date already set"       (checkOutDate dat == emptyByteString) &&

--     -- Postconditions
--             traceIfFalse "Room ID not submitted"            (roomId outDatum /= emptyByteString) &&
--             traceIfFalse "Check-in date not submitted"      (checkInDate outDatum /= emptyByteString) &&
--             traceIfFalse "Check-out date not submitted"     (checkOutDate outDatum /= emptyByteString) &&

--             traceIfFalse "Reservation flag not set"         (isReserved outDatum == True) &&
--             traceIfFalse "Reservation ID not submitted"     (reservationId outDatum /= emptyByteString) &&
--             traceIfFalse "Reservation status not confirmed" (reservationStatus outDatum == True) &&

--     -- Unchanged fields
--             traceIfFalse "Other fields must remain unchanged"
--                 (  guestAddress outDatum     == guestAddress dat
--                 && name outDatum             == name dat
--                 && passportNumber outDatum   == passportNumber dat
--                 && photoHash outDatum        == photoHash dat
--                 && isUserVerified outDatum   == isUserVerified dat
--                 && identityStatus outDatum   == identityStatus dat
--                 && adminPKH outDatum         == adminPKH dat
--                )


--         CheckOut ->
--             traceIfFalse "Only admin can revoke access"     (txSignedBy info (adminPKH dat)) &&
--             traceIfFalse "Reservation must be active before checkout" (isReserved dat == True) &&
--             traceIfFalse "Reservation must be cleared after checkout" (isReserved outDatum == False) &&

--             traceIfFalse "Reservation status must be confirmed before checkout" (reservationStatus dat == True) &&
--             traceIfFalse "Reservation status must be cleared after checkout" (reservationStatus outDatum == False) &&
--             traceIfFalse "Reservation ID must be present before checkout" (reservationId dat /= emptyByteString) &&
--             traceIfFalse "Reservation ID must be cleared after checkout" (reservationId outDatum == emptyByteString) &&

--             traceIfFalse "Room ID must be cleared after checkout" (roomId outDatum == emptyByteString) &&
--             traceIfFalse "Check-in date must be cleared after checkout" (checkInDate outDatum == emptyByteString) &&
--             traceIfFalse "Check-out date must be cleared after checkout" (checkOutDate outDatum == emptyByteString) &&
--             -- Ensure other fields remain unchanged
--             traceIfFalse "Other fields must remain unchanged after checkout"
--                 (  guestAddress outDatum     == guestAddress dat
--                 && name outDatum             == name dat
--                 && passportNumber outDatum   == passportNumber dat
--                 && photoHash outDatum        == photoHash dat
--                 && isUserVerified outDatum   == isUserVerified dat
--                 && identityStatus outDatum   == identityStatus dat
--                 && adminPKH outDatum         == adminPKH dat
--                 )
        
        

       
        
-- -- Boilerplate
-- {-# INLINABLE wrapped #-}
-- wrapped :: BuiltinData -> BuiltinData -> BuiltinData -> ()
-- wrapped d r c =
--     check $ mkValidator
--         (unsafeFromBuiltinData d)
--         (unsafeFromBuiltinData r)
--         (unsafeFromBuiltinData c)

-- validator :: Validator
-- validator = mkValidatorScript $$(compile [|| wrapped ||])