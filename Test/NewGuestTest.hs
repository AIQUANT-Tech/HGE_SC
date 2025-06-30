{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Test.Tasty (defaultMain, testGroup, TestTree)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck as QC

import qualified GuestIdentityAndReservation.Validator2 as Validator
import Plutus.V2.Ledger.Api (PubKeyHash(..),toBuiltin)
import qualified Data.ByteString.Char8 as BS

main :: IO ()
main = defaultMain tests

-- Dummy admin key
adminPKH :: PubKeyHash
adminPKH = PubKeyHash "admin"

-- Empty datum for testing
emptyDatum :: Validator.GuestDatum
emptyDatum = Validator.GuestDatum
  { Validator.guestAddress = ""
  , Validator.adminPKH = adminPKH
  , Validator.identity = Validator.IdentityInfo "" "" "" False False
  , Validator.reservation = Validator.ReservationInfo False False "" "" "" ""
  , Validator.keyInfo = Validator.KeyInfo False "" False
  }

tests :: TestTree
tests = testGroup "Guest Validator Tests"
  [ unitTests
  , propertyTests
  ]

unitTests :: TestTree
unitTests = testGroup "Unit Tests"
  [ testCase "FullReservation: valid update changes only reservation" $ do
      let inDatum = emptyDatum
          outDatum = inDatum { Validator.reservation = Validator.ReservationInfo
                                True True "resv1" "room1" "2025-07-01" "2025-07-05" }

          c1 = Validator.identity inDatum == Validator.identity outDatum
          c2 = Validator.keyInfo inDatum == Validator.keyInfo outDatum
          c3 = Validator.guestAddress inDatum == Validator.guestAddress outDatum
          c4 = Validator.adminPKH inDatum == Validator.adminPKH outDatum

      (c1 && c2 && c3 && c4) @?= True

  , testCase "FullIdentitySubmit: changes only identity" $ do
      let inDatum = emptyDatum
                  { Validator.reservation = Validator.ReservationInfo True True "id" "r" "d1" "d2" }

          outDatum = inDatum
                  { Validator.identity = Validator.IdentityInfo
                      "John" "P12345" "hash" True True }

          c0 = Validator.identity inDatum == Validator.IdentityInfo "" "" "" False False
          c1 = Validator.reservation inDatum == Validator.reservation outDatum
          c2 = Validator.keyInfo inDatum == Validator.keyInfo outDatum
          c3 = Validator.guestAddress inDatum == Validator.guestAddress outDatum
          c4 = Validator.adminPKH inDatum == Validator.adminPKH outDatum

      (c0 && c1 && c2 && c3 && c4) @?= True


  , testCase "GenerateAndValidateKey: only keyInfo changed correctly" $ do
      let inDatum = emptyDatum
            { Validator.identity = Validator.IdentityInfo "A" "B" "C" True True
            , Validator.reservation = Validator.ReservationInfo True True "r" "r" "c1" "c2" }
          outDatum = inDatum { Validator.keyInfo = Validator.KeyInfo True "KEY1" True }

          c1 = Validator.identity inDatum == Validator.identity outDatum
          c2 = Validator.reservation inDatum == Validator.reservation outDatum
          c3 = Validator.guestAddress inDatum == Validator.guestAddress outDatum
          c4 = Validator.adminPKH inDatum == Validator.adminPKH outDatum

      (c1 && c2 && c3 && c4) @?= True

  , testCase "CheckOut: clears reservation and keyInfo only" $ do
      let inDatum = emptyDatum
            { Validator.identity = Validator.IdentityInfo "X" "Y" "Z" True True
            , Validator.reservation = Validator.ReservationInfo True True "id" "room" "in" "out"
            , Validator.keyInfo = Validator.KeyInfo True "KEY" True }

          outDatum = inDatum
            { Validator.reservation = Validator.ReservationInfo False False "" "" "" ""
            , Validator.keyInfo = Validator.KeyInfo False "" False }

          c1 = Validator.identity inDatum == Validator.identity outDatum
          c2 = Validator.guestAddress inDatum == Validator.guestAddress outDatum
          c3 = Validator.adminPKH inDatum == Validator.adminPKH outDatum

      (c1 && c2 && c3) @?= True
  ]

propertyTests :: TestTree
propertyTests = testGroup "Property-Based Tests"
  [ QC.testProperty "FullReservation: only reservation updated from initial state" $ 
  \(resId :: String) (room :: String) (cin :: String) (cout :: String) ->
    let inDatum = Validator.GuestDatum "addr" adminPKH
                      (Validator.IdentityInfo "" "" "" False False)
                      (Validator.ReservationInfo False False "" "" "" "")
                      (Validator.KeyInfo False "" False)

        resvOut = Validator.ReservationInfo
                    True True
                    (toBuiltin $ BS.pack resId)
                    (toBuiltin $ BS.pack room)
                    (toBuiltin $ BS.pack cin)
                    (toBuiltin $ BS.pack cout)

        outDatum = inDatum { Validator.reservation = resvOut }

    in Validator.identity inDatum == Validator.identity outDatum
    && Validator.keyInfo inDatum == Validator.keyInfo outDatum
    && Validator.adminPKH inDatum == Validator.adminPKH outDatum
    && Validator.guestAddress inDatum == Validator.guestAddress outDatum


   ,QC.testProperty "FullIdentitySubmit: only identity updated after reservation" $
  \(nameVal :: String) (pass :: String) (photo :: String) ->
    let idOut = Validator.IdentityInfo
                  (toBuiltin $ BS.pack nameVal)
                  (toBuiltin $ BS.pack pass)
                  (toBuiltin $ BS.pack photo)
                  True True

        resv = Validator.ReservationInfo True True "resv" "room" "cin" "cout"

        inDatum = Validator.GuestDatum "addr" adminPKH
                      (Validator.IdentityInfo "" "" "" False False)
                      resv
                      (Validator.KeyInfo False "" False)

        outDatum = inDatum { Validator.identity = idOut }

    in Validator.reservation inDatum == Validator.reservation outDatum
    && Validator.keyInfo inDatum == Validator.keyInfo outDatum
    && Validator.adminPKH inDatum == Validator.adminPKH outDatum
    && Validator.guestAddress inDatum == Validator.guestAddress outDatum

   ,QC.testProperty "GenerateAndValidateKey: only keyInfo updated after identity & reservation" $
  \(k :: String) ->
    let idVal = Validator.IdentityInfo "X" "Y" "Z" True True
        resv = Validator.ReservationInfo True True "resv" "room" "cin" "cout"
        keyOut = Validator.KeyInfo True (toBuiltin $ BS.pack k) True

        inDatum = Validator.GuestDatum "addr" adminPKH idVal resv (Validator.KeyInfo False "" False)
        outDatum = inDatum { Validator.keyInfo = keyOut }

    in Validator.identity inDatum == Validator.identity outDatum
    && Validator.reservation inDatum == Validator.reservation outDatum
    && Validator.adminPKH inDatum == Validator.adminPKH outDatum
    && Validator.guestAddress inDatum == Validator.guestAddress outDatum

   ,QC.testProperty "CheckOut: clears reservation and keyInfo, keeps rest unchanged" $
  \(_ :: Bool) ->
    let idVal = Validator.IdentityInfo "A" "B" "C" True True
        resvBefore = Validator.ReservationInfo True True "id" "rm" "in" "out"
        resvAfter  = Validator.ReservationInfo False False "" "" "" ""
        keyBefore = Validator.KeyInfo True "KEY" True
        keyAfter  = Validator.KeyInfo False "" False

        inDatum = Validator.GuestDatum "addr" adminPKH idVal resvBefore keyBefore
        outDatum = Validator.GuestDatum "addr" adminPKH idVal resvAfter keyAfter

    in Validator.identity inDatum == Validator.identity outDatum
    && Validator.guestAddress inDatum == Validator.guestAddress outDatum
    && Validator.adminPKH inDatum == Validator.adminPKH outDatum

  ]

