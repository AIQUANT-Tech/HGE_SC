{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

-- Modified import to include <$>, <*>, and ==
import Prelude                                  (IO, String, show, ($), (++), Bool(..), (==), mempty, Maybe(Nothing),undefined,(.))
import Test.Tasty                               (defaultMain, testGroup, TestTree)
import Test.Tasty.HUnit                         (testCase, (@?=))
import Test.Tasty.QuickCheck                    (testProperty, forAll, (.&&.), (===), Arbitrary(..), Gen, elements, oneof, vectorOf, Property)
import qualified Data.ByteString.Char8       as C
import qualified PlutusTx.AssocMap           as AssocMap
import Control.Applicative                      ((<$>), (<*>), pure) -- Explicitly import all missing operators

import Plutus.V2.Ledger.Api                     (PubKeyHash(..), TxOutRef(..), TxId(..), TxOut(..), Datum(..), txOutAddress, txOutValue, txOutDatum, txOutReferenceScript, OutputDatum(..))
import Plutus.V2.Ledger.Contexts                (ScriptContext(..), TxInfo(..), TxInInfo(..), ScriptPurpose(..), ScriptPurpose(Spending))
import Plutus.V1.Ledger.Interval                (always)
import PlutusTx                                 (toBuiltinData, fromBuiltinData)
import qualified PlutusTx
import PlutusTx.Builtins                        (BuiltinByteString, toBuiltin)

import Reservation.Reservation                   ( Reservation(..)
                                                , ReservationStatus(..)
                                                , ReservationAction(..)
                                                , mkReservationValidator
                                                )

-- | Utility to turn a String into BuiltinByteString
stringToBS :: String -> BuiltinByteString
stringToBS = toBuiltin . C.pack

-- | Dummy PubKeyHashes
guestPkh, hotelPkh, otherPkh :: PubKeyHash
guestPkh = PubKeyHash (stringToBS "guest1234")
hotelPkh = PubKeyHash (stringToBS "hotel5678")
otherPkh = PubKeyHash (stringToBS "other9999")

-- | Dummy TxOutRef
dummyRef :: TxOutRef
dummyRef = TxOutRef (TxId $ stringToBS "txid0000") 0

-- | Build a mock ScriptContext
mkCtx :: [PubKeyHash]   -- ^ signers
      -> [TxInInfo]     -- ^ script inputs
      -> ScriptPurpose  -- ^ purpose
      -> ScriptContext
mkCtx signers ins purpose = ScriptContext
  { scriptContextTxInfo = TxInfo
      { txInfoInputs      = ins
      , txInfoOutputs     = []           -- not used by this validator
      , txInfoFee         = mempty
      , txInfoMint        = mempty
      , txInfoDCert       = []
      , txInfoWdrl        = AssocMap.empty
      , txInfoValidRange  = always      -- <— use `always` here
      , txInfoSignatories = signers
      , txInfoData        = AssocMap.empty
      , txInfoId          = TxId (stringToBS "infoid")
      }
  , scriptContextPurpose = purpose
  }

-- | Wrap a Reservation into a script‐input
mkIn :: Reservation -> TxInInfo
mkIn r = TxInInfo dummyRef $
  let d = toBuiltinData r
  in  TxOut
        { txOutAddress         = undefined
        , txOutValue           = mempty
        , txOutDatum           = OutputDatum (Datum d)
        , txOutReferenceScript = Nothing
        }

-- === Unit tests ===

unitTests :: TestTree
unitTests = testGroup "Reservation Validator Unit Tests"
  [ testCase "ReserveRoom: valid guest" $
      let res = Reservation 1 guestPkh 101 1000 2000 Reserved
          ctx = mkCtx [guestPkh] [mkIn res] (Spending dummyRef)
      in mkReservationValidator hotelPkh () (ReserveRoom guestPkh res) ctx @?= True

  , testCase "ReserveRoom: wrong signer" $
      let res = Reservation 2 guestPkh 102 1000 2000 Reserved
          ctx = mkCtx [otherPkh] [mkIn res] (Spending dummyRef)
      in mkReservationValidator hotelPkh () (ReserveRoom guestPkh res) ctx @?= False

  , testCase "ConfirmReservation: hotel signs" $
      let ctx = mkCtx [hotelPkh] [] (Spending dummyRef)
      in mkReservationValidator hotelPkh () (ConfirmReservation guestPkh) ctx @?= True

  , testCase "ConfirmReservation: non-hotel fails" $
      let ctx = mkCtx [guestPkh] [] (Spending dummyRef)
      in mkReservationValidator hotelPkh () (ConfirmReservation guestPkh) ctx @?= False

  , testCase "InitiateCheckIn: guest signs" $
      let ctx = mkCtx [guestPkh] [] (Spending dummyRef)
      in mkReservationValidator hotelPkh () (InitiateCheckIn guestPkh) ctx @?= True

  , testCase "InitiateCheckIn: other fails" $
      let ctx = mkCtx [hotelPkh] [] (Spending dummyRef)
      in mkReservationValidator hotelPkh () (InitiateCheckIn guestPkh) ctx @?= False

  , testCase "CheckOut: guest signs" $
      let ctx = mkCtx [guestPkh] [] (Spending dummyRef)
      in mkReservationValidator hotelPkh () (CheckOut guestPkh) ctx @?= True

  , testCase "CheckOut: other fails" $
      let ctx = mkCtx [hotelPkh] [] (Spending dummyRef)
      in mkReservationValidator hotelPkh () (CheckOut guestPkh) ctx @?= False
  ]

-- === QuickCheck properties ===

instance Arbitrary PubKeyHash where
  arbitrary = elements [guestPkh, hotelPkh, otherPkh]

instance Arbitrary Reservation where
  arbitrary = do
    rid      <- arbitrary
    guest    <- elements [guestPkh, otherPkh]
    room     <- arbitrary
    cin      <- arbitrary
    cout     <- arbitrary
    status   <- elements [Reserved, CheckedIn, CheckedOut, Cancelled]
    pure $ Reservation rid guest room cin cout status

instance Arbitrary ReservationAction where
  arbitrary = oneof
    [ ReserveRoom    <$> elements [guestPkh] <*> arbitrary
    , ConfirmReservation <$> elements [guestPkh]
    , InitiateCheckIn    <$> elements [guestPkh]
    , CheckOut           <$> elements [guestPkh]
    ]

prop_reserve_quick :: Reservation -> Property
prop_reserve_quick r =
  let ctxGood = mkCtx [resGuest r] [mkIn r] (Spending dummyRef)
  in mkReservationValidator hotelPkh () (ReserveRoom (resGuest r) r) ctxGood === True

prop_confirm_quick :: PubKeyHash -> Property
prop_confirm_quick staff =
  let ctx = mkCtx [hotelPkh] [] (Spending dummyRef)
  in mkReservationValidator hotelPkh () (ConfirmReservation staff) ctx === True

prop_checkin_quick :: PubKeyHash -> Property
prop_checkin_quick guest =
  let ctx = mkCtx [guest] [] (Spending dummyRef)
  in mkReservationValidator hotelPkh () (InitiateCheckIn guest) ctx === True

prop_checkout_quick :: PubKeyHash -> Property
prop_checkout_quick guest =
  let ctx = mkCtx [guest] [] (Spending dummyRef)
  in mkReservationValidator hotelPkh () (CheckOut guest) ctx === True

-- | All tests
tests :: TestTree
tests = testGroup "Reservation Validator Tests"
  [ unitTests
  , testGroup "QuickCheck Properties"
      [ testProperty "reserve succeeds for guest"     prop_reserve_quick
      , testProperty "confirm only hotel"             prop_confirm_quick
      , testProperty "check-in always allowed by guest" prop_checkin_quick
      , testProperty "checkout always allowed by guest" prop_checkout_quick
      ]
  ]

main :: IO ()
main = defaultMain tests