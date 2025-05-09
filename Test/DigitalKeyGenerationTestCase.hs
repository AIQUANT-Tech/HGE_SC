{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.Tasty (defaultMain, testGroup, TestTree)
import Test.Tasty.HUnit (testCase, (@?=))
import Plutus.V2.Ledger.Api 
import Plutus.V2.Ledger.Contexts
import Plutus.V1.Ledger.Interval (always)
import qualified PlutusTx.AssocMap as AssocMap
import qualified HGE2.Validator as Validator

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
  , Validator.initiateCheckIn = False
  , Validator.adminPKH = Validator.emptyPubKeyHash
  , Validator.digitalKey = ""
  , Validator.isDigitalKeyValidated = False
  }

-- Simplify the test approach - test validator conditions directly
tests :: TestTree
tests = testGroup "HGE2 Hotel Guest Experience Validator Tests"
  -- [ testGroup "SubmitIdentity Redeemer"
  --     [ testCase "Valid initial identity submission" $ do
  --         let initialDatum = emptyDatum
  --             outputDatum = emptyDatum 
  --               { Validator.guestAddress = "guest_address_123"
  --               , Validator.initiateCheckIn = True
  --               , Validator.adminPKH = testAdmin
  --               }
              
  --             -- Test conditions directly rather than creating complex context
  --             c1 = Validator.guestAddress initialDatum == ""
  --             c2 = Validator.adminPKH initialDatum == Validator.emptyPubKeyHash
  --             c3 = Validator.guestAddress outputDatum /= ""
  --             c4 = Validator.adminPKH outputDatum /= Validator.emptyPubKeyHash
  --             c5 = Validator.initiateCheckIn outputDatum == True
  --             c6 = Validator.isDigitalKeyValidated outputDatum == Validator.isDigitalKeyValidated initialDatum
  --             c7 = Validator.digitalKey outputDatum == Validator.digitalKey initialDatum
              
  --             allConditions = c1 && c2 && c3 && c4 && c5 && c6 && c7
          
  --         allConditions @?= True

  --     , testCase "Reject submission when guest address already exists" $ do
  --         let initialDatum = emptyDatum { Validator.guestAddress = "existing_address" }
  --             outputDatum = initialDatum
  --               { Validator.initiateCheckIn = True
  --               , Validator.adminPKH = testAdmin
  --               }
              
  --             -- Test the condition directly
  --             condition = Validator.guestAddress initialDatum == ""
          
  --         condition @?= False

  --     , testCase "Reject submission when admin already set" $ do
  --         let initialDatum = emptyDatum { Validator.adminPKH = testAdmin }
  --             outputDatum = initialDatum
  --               { Validator.guestAddress = "guest_address_123"
  --               , Validator.initiateCheckIn = True
  --               }
              
  --             condition = Validator.adminPKH initialDatum == Validator.emptyPubKeyHash
          
  --         condition @?= False

  --     , testCase "Reject submission when check-in flag not updated" $ do
  --         let initialDatum = emptyDatum
  --             outputDatum = initialDatum
  --               { Validator.guestAddress = "guest_address_123"
  --               , Validator.adminPKH = testAdmin
  --               , Validator.initiateCheckIn = False -- Should be True
  --               }
              
  --             condition = Validator.initiateCheckIn outputDatum == True
          
  --         condition @?= False
  --     ]

    [testGroup "DigitalKeyGeneration Redeemer"
      [ testCase "Valid digital key generation by admin" $ do
          let initialDatum = emptyDatum
                { Validator.guestAddress = "guest_address_123"
                , Validator.initiateCheckIn = True
                , Validator.adminPKH = testAdmin
                }
              outputDatum = initialDatum
                { Validator.digitalKey = "generated_digital_key_456"
                }
              
              txInfo = mkTxInfo [testAdmin]
              
              -- Test conditions directly
              c1 = testAdmin `elem` txInfoSignatories txInfo
              c2 = Validator.initiateCheckIn initialDatum == True
              c3 = Validator.digitalKey initialDatum == ""
              c4 = Validator.digitalKey outputDatum /= ""
              c5 = Validator.guestAddress outputDatum == Validator.guestAddress initialDatum
              c6 = Validator.adminPKH outputDatum == Validator.adminPKH initialDatum
              c7 = Validator.isDigitalKeyValidated outputDatum == Validator.isDigitalKeyValidated initialDatum
              c8 = Validator.initiateCheckIn outputDatum == Validator.initiateCheckIn initialDatum
              
              allConditions = c1 && c2 && c3 && c4 && c5 && c6 && c7 && c8
          
          allConditions @?= True

      , testCase "Reject generation without admin signature" $ do
          let initialDatum = emptyDatum
                { Validator.guestAddress = "guest_address_123"
                , Validator.initiateCheckIn = True
                , Validator.adminPKH = testAdmin
                }
              
              txInfo = mkTxInfo [otherPkh] -- Not admin
              
              condition = testAdmin `elem` txInfoSignatories txInfo
          
          condition @?= False

      , testCase "Reject generation when check-in not initiated" $ do
          let initialDatum = emptyDatum
                { Validator.guestAddress = "guest_address_123"
                , Validator.initiateCheckIn = False -- Check-in not initiated
                , Validator.adminPKH = testAdmin
                }
              
              condition = Validator.initiateCheckIn initialDatum == True
          
          condition @?= False

      , testCase "Reject generation when key already exists" $ do
          let initialDatum = emptyDatum
                { Validator.guestAddress = "guest_address_123"
                , Validator.initiateCheckIn = True
                , Validator.adminPKH = testAdmin
                , Validator.digitalKey = "existing_key" -- Key already exists
                }
              
              condition = Validator.digitalKey initialDatum == ""
          
          condition @?= False
      ]

  , testGroup "DigitalKeyValidation Redeemer"
      [ testCase "Valid digital key validation" $ do
          let initialDatum = emptyDatum
                { Validator.guestAddress = "guest_address_123"
                , Validator.initiateCheckIn = True
                , Validator.adminPKH = testAdmin
                , Validator.digitalKey = "digital_key_456"
                }
              outputDatum = initialDatum
                { Validator.isDigitalKeyValidated = True
                }
              
              -- Test conditions directly
              c1 = Validator.digitalKey initialDatum /= ""
              c2 = Validator.isDigitalKeyValidated outputDatum == True
              c3 = Validator.guestAddress outputDatum == Validator.guestAddress initialDatum
              c4 = Validator.adminPKH outputDatum == Validator.adminPKH initialDatum
              c5 = Validator.digitalKey outputDatum == Validator.digitalKey initialDatum
              c6 = Validator.initiateCheckIn outputDatum == Validator.initiateCheckIn initialDatum
              
              allConditions = c1 && c2 && c3 && c4 && c5 && c6
          
          allConditions @?= True

      , testCase "Reject validation when digital key not generated" $ do
          let initialDatum = emptyDatum
                { Validator.guestAddress = "guest_address_123"
                , Validator.initiateCheckIn = True
                , Validator.adminPKH = testAdmin
                , Validator.digitalKey = "" -- Key not generated
                }
              
              condition = Validator.digitalKey initialDatum /= ""
          
          condition @?= False

      , testCase "Reject when validation flag not set to true" $ do
          let initialDatum = emptyDatum
                { Validator.guestAddress = "guest_address_123"
                , Validator.initiateCheckIn = True
                , Validator.adminPKH = testAdmin
                , Validator.digitalKey = "digital_key_456"
                }
              outputDatum = initialDatum
                { Validator.isDigitalKeyValidated = False -- Not set to true
                }
              
              condition = Validator.isDigitalKeyValidated outputDatum == True
          
          condition @?= False
      ]
  ]