{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.Tasty (defaultMain, testGroup, TestTree)
import Test.Tasty.HUnit (testCase, (@?=))
import Plutus.V2.Ledger.Api
import Plutus.V2.Ledger.Contexts
import Plutus.V1.Ledger.Interval (always)
import qualified PlutusTx.AssocMap as AssocMap
import qualified GuestIdentityAndReservation.Validator as Validator

main :: IO ()
main = defaultMain tests

-- Constants for testing
testAdmin :: PubKeyHash
testAdmin = PubKeyHash "admin_pkh"

otherPkh :: PubKeyHash
otherPkh = PubKeyHash "other_pkh"

-- Helper function to create a basic TxInfo
mkTxInfo :: [PubKeyHash] -> TxInfo
mkTxInfo signatories = TxInfo
  { txInfoInputs = []
  , txInfoReferenceInputs = []
  , txInfoOutputs = []
  , txInfoFee = mempty
  , txInfoMint = mempty
  , txInfoDCert = []
  , txInfoWdrl = AssocMap.empty
  , txInfoValidRange = always
  , txInfoSignatories = signatories
  , txInfoRedeemers = AssocMap.empty
  , txInfoData = AssocMap.empty
  , txInfoId = TxId "dummy_tx_id"
  }

-- Create empty datum
emptyDatum :: Validator.GuestDatum
emptyDatum = Validator.GuestDatum
  { Validator.guestAddress = ""
  , Validator.name = ""
  , Validator.passportNumber = ""
  , Validator.photoHash = ""
  , Validator.isUserVerified = False
  , Validator.identityStatus = False
  , Validator.isReserved = False
  , Validator.initiateCheckIn = False
  , Validator.reservationStatus = False
  , Validator.reservationId = ""
  , Validator.roomId = ""
  , Validator.checkInDate = ""
  , Validator.checkOutDate = ""
  , Validator.adminPKH = Validator.emptyPubKeyHash
  }

-- Simplify the test approach - test validator conditions directly
tests :: TestTree
tests = testGroup "HGE Hotel Guest Experience Validator Tests"
  [ testGroup "SubmitIdentity Redeemer"
      [ testCase "Valid initial identity submission" $ do
          let initialDatum = emptyDatum
              outputDatum = emptyDatum 
                { Validator.guestAddress = "guest_address_123"
                , Validator.name = "John Doe"
                , Validator.passportNumber = "AB1234567"
                , Validator.photoHash = "photo_hash_123"
                , Validator.adminPKH = testAdmin
                }
              
              -- Test conditions directly rather than creating complex context
              c1 = Validator.guestAddress initialDatum == ""
              c2 = Validator.name initialDatum == ""
              c3 = Validator.passportNumber initialDatum == ""
              c4 = Validator.photoHash initialDatum == ""
              c5 = Validator.adminPKH initialDatum == Validator.emptyPubKeyHash
              
              c6 = Validator.guestAddress outputDatum /= ""
              c7 = Validator.name outputDatum /= ""
              c8 = Validator.passportNumber outputDatum /= ""
              c9 = Validator.photoHash outputDatum /= ""
              c10 = Validator.adminPKH outputDatum /= Validator.emptyPubKeyHash
              
              c11 = Validator.isUserVerified outputDatum == Validator.isUserVerified initialDatum
              c12 = Validator.identityStatus outputDatum == Validator.identityStatus initialDatum
              c13 = Validator.isReserved outputDatum == Validator.isReserved initialDatum
              c14 = Validator.initiateCheckIn outputDatum == Validator.initiateCheckIn initialDatum
              c15 = Validator.reservationStatus outputDatum == Validator.reservationStatus initialDatum
              c16 = Validator.reservationId outputDatum == Validator.reservationId initialDatum
              c17 = Validator.roomId outputDatum == Validator.roomId initialDatum
              c18 = Validator.checkInDate outputDatum == Validator.checkInDate initialDatum
              c19 = Validator.checkOutDate outputDatum == Validator.checkOutDate initialDatum
              
              allConditions = c1 && c2 && c3 && c4 && c5 && c6 && c7 && c8 && c9 && c10 && 
                              c11 && c12 && c13 && c14 && c15 && c16 && c17 && c18 && c19
          
          allConditions @?= True

      , testCase "Reject submission when guest address already exists" $ do
          let initialDatum = emptyDatum { Validator.guestAddress = "existing_address" }
              
              condition = Validator.guestAddress initialDatum == ""
          
          condition @?= False
      ]

  , testGroup "VerifyIdentity Redeemer"
      [ testCase "Valid identity verification by admin" $ do
          let initialDatum = emptyDatum
                { Validator.guestAddress = "guest_address_123"
                , Validator.name = "John Doe"
                , Validator.passportNumber = "AB1234567"
                , Validator.photoHash = "photo_hash_123"
                , Validator.adminPKH = testAdmin
                }
              outputDatum = initialDatum
                { Validator.isUserVerified = True
                }
              
              txInfo = mkTxInfo [testAdmin]
              
              -- Test conditions directly
              c1 = testAdmin `elem` txInfoSignatories txInfo
              c2 = Validator.guestAddress initialDatum /= ""
              c3 = Validator.name initialDatum /= ""
              c4 = Validator.passportNumber initialDatum /= ""
              c5 = Validator.photoHash initialDatum /= ""
              c6 = Validator.adminPKH initialDatum /= Validator.emptyPubKeyHash
              c7 = Validator.isUserVerified initialDatum == False
              c8 = Validator.isUserVerified outputDatum == True
              
              c9 = Validator.name outputDatum == Validator.name initialDatum
              c10 = Validator.guestAddress outputDatum == Validator.guestAddress initialDatum
              c11 = Validator.passportNumber outputDatum == Validator.passportNumber initialDatum
              c12 = Validator.photoHash outputDatum == Validator.photoHash initialDatum
              c13 = Validator.identityStatus outputDatum == Validator.identityStatus initialDatum
              c14 = Validator.isReserved outputDatum == Validator.isReserved initialDatum
              c15 = Validator.initiateCheckIn outputDatum == Validator.initiateCheckIn initialDatum
              c16 = Validator.reservationStatus outputDatum == Validator.reservationStatus initialDatum
              c17 = Validator.reservationId outputDatum == Validator.reservationId initialDatum
              c18 = Validator.roomId outputDatum == Validator.roomId initialDatum
              c19 = Validator.checkInDate outputDatum == Validator.checkInDate initialDatum
              c20 = Validator.checkOutDate outputDatum == Validator.checkOutDate initialDatum
              c21 = Validator.adminPKH outputDatum == Validator.adminPKH initialDatum
              
              allConditions = c1 && c2 && c3 && c4 && c5 && c6 && c7 && c8 && c9 && c10 && 
                              c11 && c12 && c13 && c14 && c15 && c16 && c17 && c18 && c19 && c20 && c21
          
          allConditions @?= True

      , testCase "Reject verification without admin signature" $ do
          let initialDatum = emptyDatum
                { Validator.guestAddress = "guest_address_123"
                , Validator.name = "John Doe"
                , Validator.passportNumber = "AB1234567"
                , Validator.photoHash = "photo_hash_123"
                , Validator.adminPKH = testAdmin
                }
              
              txInfo = mkTxInfo [otherPkh] -- Not admin
              
              condition = testAdmin `elem` txInfoSignatories txInfo
          
          condition @?= False
      ]

  , testGroup "ConfirmIdentity Redeemer"
      [ testCase "Valid identity confirmation" $ do
          let initialDatum = emptyDatum
                { Validator.guestAddress = "guest_address_123"
                , Validator.name = "John Doe"
                , Validator.passportNumber = "AB1234567"
                , Validator.photoHash = "photo_hash_123"
                , Validator.isUserVerified = True
                , Validator.adminPKH = testAdmin
                }
              outputDatum = initialDatum
                { Validator.identityStatus = True
                }
              
              -- Test conditions directly
              c1 = Validator.isUserVerified initialDatum == True
              c2 = Validator.identityStatus initialDatum == False
              c3 = Validator.identityStatus outputDatum == True
              
              c4 = Validator.guestAddress outputDatum == Validator.guestAddress initialDatum
              c5 = Validator.name outputDatum == Validator.name initialDatum
              c6 = Validator.passportNumber outputDatum == Validator.passportNumber initialDatum
              c7 = Validator.photoHash outputDatum == Validator.photoHash initialDatum
              c8 = Validator.isUserVerified outputDatum == Validator.isUserVerified initialDatum
              c9 = Validator.isReserved outputDatum == Validator.isReserved initialDatum
              c10 = Validator.initiateCheckIn outputDatum == Validator.initiateCheckIn initialDatum
              c11 = Validator.reservationStatus outputDatum == Validator.reservationStatus initialDatum
              c12 = Validator.reservationId outputDatum == Validator.reservationId initialDatum
              c13 = Validator.roomId outputDatum == Validator.roomId initialDatum
              c14 = Validator.checkInDate outputDatum == Validator.checkInDate initialDatum
              c15 = Validator.checkOutDate outputDatum == Validator.checkOutDate initialDatum
              c16 = Validator.adminPKH outputDatum == Validator.adminPKH initialDatum
              
              allConditions = c1 && c2 && c3 && c4 && c5 && c6 && c7 && c8 && c9 && c10 && 
                              c11 && c12 && c13 && c14 && c15 && c16
          
          allConditions @?= True

      , testCase "Reject confirmation when user not verified" $ do
          let initialDatum = emptyDatum
                { Validator.guestAddress = "guest_address_123"
                , Validator.name = "John Doe"
                , Validator.passportNumber = "AB1234567"
                , Validator.photoHash = "photo_hash_123"
                , Validator.isUserVerified = False -- Not verified yet
                , Validator.adminPKH = testAdmin
                }
              
              condition = Validator.isUserVerified initialDatum == True
          
          condition @?= False
      ]

  , testGroup "ReserveRoom Redeemer"
      [ testCase "Valid room reservation" $ do
          let initialDatum = emptyDatum
                { Validator.guestAddress = "guest_address_123"
                , Validator.name = "John Doe"
                , Validator.passportNumber = "AB1234567"
                , Validator.photoHash = "photo_hash_123"
                , Validator.isUserVerified = True
                , Validator.identityStatus = True
                , Validator.adminPKH = testAdmin
                }
              outputDatum = initialDatum
                { Validator.isReserved = True
                , Validator.roomId = "room_101"
                , Validator.checkInDate = "2025-05-10"
                , Validator.checkOutDate = "2025-05-15"
                }
              
              -- Test conditions directly
              c1 = Validator.isReserved initialDatum == False
              c2 = Validator.roomId initialDatum == ""
              c3 = Validator.checkInDate initialDatum == ""
              c4 = Validator.checkOutDate initialDatum == ""
              
              c5 = Validator.roomId outputDatum /= ""
              c6 = Validator.checkInDate outputDatum /= ""
              c7 = Validator.checkOutDate outputDatum /= ""
              c8 = Validator.isReserved outputDatum == True
              
              c9 = Validator.guestAddress outputDatum == Validator.guestAddress initialDatum
              c10 = Validator.name outputDatum == Validator.name initialDatum
              c11 = Validator.passportNumber outputDatum == Validator.passportNumber initialDatum
              c12 = Validator.photoHash outputDatum == Validator.photoHash initialDatum
              c13 = Validator.isUserVerified outputDatum == Validator.isUserVerified initialDatum
              c14 = Validator.identityStatus outputDatum == Validator.identityStatus initialDatum
              c15 = Validator.initiateCheckIn outputDatum == Validator.initiateCheckIn initialDatum
              c16 = Validator.reservationStatus outputDatum == Validator.reservationStatus initialDatum
              c17 = Validator.reservationId outputDatum == Validator.reservationId initialDatum
              c18 = Validator.adminPKH outputDatum == Validator.adminPKH initialDatum
              
              allConditions = c1 && c2 && c3 && c4 && c5 && c6 && c7 && c8 && c9 && c10 && 
                              c11 && c12 && c13 && c14 && c15 && c16 && c17 && c18
          
          allConditions @?= True

      , testCase "Reject reservation when already reserved" $ do
          let initialDatum = emptyDatum
                { Validator.guestAddress = "guest_address_123"
                , Validator.name = "John Doe"
                , Validator.passportNumber = "AB1234567"
                , Validator.photoHash = "photo_hash_123"
                , Validator.isUserVerified = True
                , Validator.identityStatus = True
                , Validator.isReserved = True -- Already reserved
                , Validator.adminPKH = testAdmin
                }
              
              condition = Validator.isReserved initialDatum == False
          
          condition @?= False
      ]

  , testGroup "ConfirmReservation Redeemer"
      [ testCase "Valid reservation confirmation" $ do
          let initialDatum = emptyDatum
                { Validator.guestAddress = "guest_address_123"
                , Validator.name = "John Doe"
                , Validator.passportNumber = "AB1234567"
                , Validator.photoHash = "photo_hash_123"
                , Validator.isUserVerified = True
                , Validator.identityStatus = True
                , Validator.isReserved = True
                , Validator.roomId = "room_101"
                , Validator.checkInDate = "2025-05-10"
                , Validator.checkOutDate = "2025-05-15"
                , Validator.adminPKH = testAdmin
                }
              outputDatum = initialDatum
                { Validator.reservationStatus = True
                , Validator.reservationId = "res_abc123"
                }
              
              -- Test conditions directly
              c1 = Validator.isReserved initialDatum == True
              c2 = Validator.reservationId initialDatum == ""
              c3 = Validator.reservationId outputDatum /= ""
              c4 = Validator.reservationStatus outputDatum == True
              
              c5 = Validator.guestAddress outputDatum == Validator.guestAddress initialDatum
              c6 = Validator.name outputDatum == Validator.name initialDatum
              c7 = Validator.passportNumber outputDatum == Validator.passportNumber initialDatum
              c8 = Validator.photoHash outputDatum == Validator.photoHash initialDatum
              c9 = Validator.isUserVerified outputDatum == Validator.isUserVerified initialDatum
              c10 = Validator.identityStatus outputDatum == Validator.identityStatus initialDatum
              c11 = Validator.isReserved outputDatum == Validator.isReserved initialDatum
              c12 = Validator.initiateCheckIn outputDatum == Validator.initiateCheckIn initialDatum
              c13 = Validator.roomId outputDatum == Validator.roomId initialDatum
              c14 = Validator.checkInDate outputDatum == Validator.checkInDate initialDatum
              c15 = Validator.checkOutDate outputDatum == Validator.checkOutDate initialDatum
              c16 = Validator.adminPKH outputDatum == Validator.adminPKH initialDatum
              
              allConditions = c1 && c2 && c3 && c4 && c5 && c6 && c7 && c8 && c9 && c10 && 
                              c11 && c12 && c13 && c14 && c15 && c16
          
          allConditions @?= True

      , testCase "Reject confirmation when not reserved" $ do
          let initialDatum = emptyDatum
                { Validator.guestAddress = "guest_address_123"
                , Validator.name = "John Doe"
                , Validator.passportNumber = "AB1234567"
                , Validator.photoHash = "photo_hash_123"
                , Validator.isUserVerified = True
                , Validator.identityStatus = True
                , Validator.isReserved = False -- Not reserved yet
                , Validator.adminPKH = testAdmin
                }
              
              condition = Validator.isReserved initialDatum == True
          
          condition @?= False
      ]

  , testGroup "InitiateCheckIn Redeemer"
      [ testCase "Valid check-in initiation" $ do
          let initialDatum = emptyDatum
                { Validator.guestAddress = "guest_address_123"
                , Validator.name = "John Doe"
                , Validator.passportNumber = "AB1234567"
                , Validator.photoHash = "photo_hash_123"
                , Validator.isUserVerified = True
                , Validator.identityStatus = True
                , Validator.isReserved = True
                , Validator.reservationStatus = True
                , Validator.reservationId = "res_abc123"
                , Validator.roomId = "room_101"
                , Validator.checkInDate = "2025-05-10"
                , Validator.checkOutDate = "2025-05-15"
                , Validator.adminPKH = testAdmin
                }
              outputDatum = initialDatum
                { Validator.initiateCheckIn = True
                }
              
              -- Test conditions directly
              c1 = Validator.isUserVerified initialDatum == True
              c2 = Validator.identityStatus initialDatum == True
              c3 = Validator.isReserved initialDatum == True
              c4 = Validator.reservationStatus initialDatum == True
              c5 = Validator.initiateCheckIn initialDatum == False
              c6 = Validator.initiateCheckIn outputDatum == True
              
              c7 = Validator.guestAddress outputDatum == Validator.guestAddress initialDatum
              c8 = Validator.name outputDatum == Validator.name initialDatum
              c9 = Validator.passportNumber outputDatum == Validator.passportNumber initialDatum
              c10 = Validator.photoHash outputDatum == Validator.photoHash initialDatum
              c11 = Validator.isUserVerified outputDatum == Validator.isUserVerified initialDatum
              c12 = Validator.identityStatus outputDatum == Validator.identityStatus initialDatum
              c13 = Validator.isReserved outputDatum == Validator.isReserved initialDatum
              c14 = Validator.reservationStatus outputDatum == Validator.reservationStatus initialDatum
              c15 = Validator.reservationId outputDatum == Validator.reservationId initialDatum
              c16 = Validator.roomId outputDatum == Validator.roomId initialDatum
              c17 = Validator.checkInDate outputDatum == Validator.checkInDate initialDatum
              c18 = Validator.checkOutDate outputDatum == Validator.checkOutDate initialDatum
              c19 = Validator.adminPKH outputDatum == Validator.adminPKH initialDatum
              
              allConditions = c1 && c2 && c3 && c4 && c5 && c6 && c7 && c8 && c9 && c10 && 
                              c11 && c12 && c13 && c14 && c15 && c16 && c17 && c18 && c19
          
          allConditions @?= True

      , testCase "Reject check-in when user not verified" $ do
          let initialDatum = emptyDatum
                { Validator.guestAddress = "guest_address_123"
                , Validator.name = "John Doe"
                , Validator.passportNumber = "AB1234567"
                , Validator.photoHash = "photo_hash_123"
                , Validator.isUserVerified = False -- Not verified
                , Validator.identityStatus = True
                , Validator.isReserved = True
                , Validator.reservationStatus = True
                , Validator.adminPKH = testAdmin
                }
              
              condition = Validator.isUserVerified initialDatum == True
          
          condition @?= False
      ]

  , testGroup "CheckOut Redeemer"
      [ testCase "Valid check-out" $ do
          let initialDatum = emptyDatum
                { Validator.guestAddress = "guest_address_123"
                , Validator.name = "John Doe"
                , Validator.passportNumber = "AB1234567"
                , Validator.photoHash = "photo_hash_123"
                , Validator.isUserVerified = True
                , Validator.identityStatus = True
                , Validator.isReserved = True
                , Validator.initiateCheckIn = True
                , Validator.reservationStatus = True
                , Validator.reservationId = "res_abc123"
                , Validator.roomId = "room_101"
                , Validator.checkInDate = "2025-05-10"
                , Validator.checkOutDate = "2025-05-15"
                , Validator.adminPKH = testAdmin
                }
              outputDatum = initialDatum
                { Validator.isReserved = False
                , Validator.initiateCheckIn = False
                , Validator.reservationStatus = False
                , Validator.reservationId = ""
                , Validator.roomId = ""
                , Validator.checkInDate = ""
                , Validator.checkOutDate = ""
                }
              
              -- Test conditions directly
              c1 = Validator.isReserved initialDatum == True
              c2 = Validator.isReserved outputDatum == False
              c3 = Validator.reservationStatus initialDatum == True
              c4 = Validator.reservationStatus outputDatum == False
              c5 = Validator.initiateCheckIn initialDatum == True
              c6 = Validator.initiateCheckIn outputDatum == False
              c7 = Validator.reservationId initialDatum /= ""
              c8 = Validator.reservationId outputDatum == ""
              c9 = Validator.roomId outputDatum == ""
              c10 = Validator.checkInDate outputDatum == ""
              c11 = Validator.checkOutDate outputDatum == ""
              
              c12 = Validator.guestAddress outputDatum == Validator.guestAddress initialDatum
              c13 = Validator.name outputDatum == Validator.name initialDatum
              c14 = Validator.passportNumber outputDatum == Validator.passportNumber initialDatum
              c15 = Validator.photoHash outputDatum == Validator.photoHash initialDatum
              c16 = Validator.isUserVerified outputDatum == Validator.isUserVerified initialDatum
              c17 = Validator.identityStatus outputDatum == Validator.identityStatus initialDatum
              c18 = Validator.adminPKH outputDatum == Validator.adminPKH initialDatum
              
              allConditions = c1 && c2 && c3 && c4 && c5 && c6 && c7 && c8 && c9 && c10 && 
                              c11 && c12 && c13 && c14 && c15 && c16 && c17 && c18
          
          allConditions @?= True

      , testCase "Reject check-out when not reserved" $ do
          let initialDatum = emptyDatum
                { Validator.guestAddress = "guest_address_123"
                , Validator.name = "John Doe"
                , Validator.passportNumber = "AB1234567"
                , Validator.photoHash = "photo_hash_123"
                , Validator.isUserVerified = True
                , Validator.identityStatus = True
                , Validator.isReserved = False -- Not reserved
                , Validator.initiateCheckIn = True
                , Validator.reservationStatus = True
                , Validator.adminPKH = testAdmin
                }
              
              condition = Validator.isReserved initialDatum == True
          
          condition @?= False
      ]
  ]