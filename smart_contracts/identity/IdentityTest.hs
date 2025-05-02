{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE InstanceSigs      #-}

module Main where

import Prelude
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Test.QuickCheck (Arbitrary(..), Gen, vectorOf, elements, oneof)
import qualified Data.ByteString.Char8  as C
import qualified Data.Map               as Map

import Plutus.V2.Ledger.Api
import Plutus.V2.Ledger.Contexts
import Plutus.V1.Ledger.Time      (POSIXTime (..))
import PlutusTx                    (toBuiltinData, fromBuiltinData)
import qualified PlutusTx
import qualified PlutusTx.AssocMap       as AssocMap

import NewContract.Contract       -- your on-chain module
    ( IdentityDatum (..)
    , IdentityAction (..)
    , mkIdentityValidator
    )

-- | Utility to turn a String into BuiltinByteString
stringToBS :: String -> BuiltinByteString
stringToBS = toBuiltin . C.pack

-- | Dummy PubKeyHash
dummyPkh :: PubKeyHash
dummyPkh = PubKeyHash (stringToBS "deadbeef")

-- | Dummy TxOutRef
dummyRef :: TxOutRef
dummyRef = TxOutRef (TxId $ stringToBS "cafebabe") 0

-- | Modified mockCtx function to also create continuing outputs for the validator
mockCtx :: [PubKeyHash]   -- ^ signers
        -> [TxOut]        -- ^ resolved inputs
        -> [TxOut]        -- ^ outputs
        -> POSIXTime      -- ^ current slot/time (unused here)
        -> ScriptContext
mockCtx signers ins outs _now = ScriptContext
  { scriptContextTxInfo = TxInfo
      { txInfoInputs      = [ TxInInfo dummyRef o | o <- ins ]
      , txInfoOutputs     = outs
      , txInfoFee         = mempty
      , txInfoMint        = mempty
      , txInfoDCert       = []
      , txInfoWdrl        = AssocMap.empty
      , txInfoValidRange  = to _now
      , txInfoSignatories = signers
      , txInfoData        = AssocMap.empty
      , txInfoId          = TxId (stringToBS "txid")
      }
  , scriptContextPurpose = Spending dummyRef
  }

-- | Helper to turn an IdentityDatum into a TxOut carrying an inline datum
mkOut :: IdentityDatum -> TxOut
mkOut dat = TxOut
  { txOutAddress         = Address (ScriptCredential (ValidatorHash (stringToBS "script"))) Nothing
  , txOutValue           = mempty
  , txOutDatum           = OutputDatum (Datum (toBuiltinData dat))
  , txOutReferenceScript = Nothing
  }

-- ==== Unit tests with HUnit ====

unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [ testCase "SubmitIdentity: valid" $
      let dat = IdentityDatum dummyPkh "h" False
          out = mkOut dat
          ctx = mockCtx [dummyPkh] [] [out] (POSIXTime 1)
      in mkIdentityValidator dat (SubmitIdentity "h") ctx @?= True

  , testCase "SubmitIdentity: wrong signer" $
      let dat = IdentityDatum dummyPkh "h" False
          out = mkOut dat
          ctx = mockCtx [] [] [out] (POSIXTime 1)
      in mkIdentityValidator dat (SubmitIdentity "h") ctx @?= False

  , testCase "SubmitIdentity: mismatched hash" $
      let dat = IdentityDatum dummyPkh "h1" False
          out = mkOut (IdentityDatum dummyPkh "h2" False)
          ctx = mockCtx [dummyPkh] [] [out] (POSIXTime 1)
      in mkIdentityValidator dat (SubmitIdentity "h1") ctx @?= False

  , testCase "SubmitIdentity: already verified" $
      let dat = IdentityDatum dummyPkh "h" True
          out = mkOut dat
          ctx = mockCtx [dummyPkh] [] [out] (POSIXTime 1)
      in mkIdentityValidator dat (SubmitIdentity "h") ctx @?= False

  , testCase "VerifyIdentity: no input" $
      let dat = IdentityDatum dummyPkh "h" False
          ctx = mockCtx [dummyPkh] [] [] (POSIXTime 1)
      in mkIdentityValidator dat VerifyIdentity ctx @?= False

  , testCase "VerifyIdentity: wrong signer" $
      let dat = IdentityDatum dummyPkh "h" False
          inp = mkOut dat
          ctx = mockCtx [] [inp] [] (POSIXTime 1)
      in mkIdentityValidator dat VerifyIdentity ctx @?= False

  , testCase "VerifyIdentity: incorrect input hash" $
      let dat = IdentityDatum dummyPkh "h1" False
          inp = mkOut (IdentityDatum dummyPkh "h2" False)
          ctx = mockCtx [dummyPkh] [inp] [] (POSIXTime 1)
      in mkIdentityValidator dat VerifyIdentity ctx @?= False

  , testCase "ConfirmIdentity: correct flag" $
      let dat = IdentityDatum dummyPkh "h" False
          out = mkOut (IdentityDatum dummyPkh "h" True)
          ctx = mockCtx [dummyPkh] [] [out] (POSIXTime 1)
      in mkIdentityValidator dat (ConfirmIdentity True) ctx @?= True

  , testCase "ConfirmIdentity: wrong flag" $
      let dat = IdentityDatum dummyPkh "h" False
          out = mkOut (IdentityDatum dummyPkh "h" False)
          ctx = mockCtx [dummyPkh] [] [out] (POSIXTime 1)
      in mkIdentityValidator dat (ConfirmIdentity True) ctx @?= False

  , testCase "ConfirmIdentity: missing output" $
      let dat = IdentityDatum dummyPkh "h" False
          ctx = mockCtx [dummyPkh] [] [] (POSIXTime 1)
      in mkIdentityValidator dat (ConfirmIdentity True) ctx @?= False

  , testCase "ConfirmIdentity: wrong signer" $
      let dat = IdentityDatum dummyPkh "h" False
          out = mkOut (IdentityDatum dummyPkh "h" True)
          ctx = mockCtx [] [] [out] (POSIXTime 1)
      in mkIdentityValidator dat (ConfirmIdentity True) ctx @?= False
  ]

-- ==== Arbitrary instances for QuickCheck ====

instance Arbitrary BuiltinByteString where
  arbitrary :: Gen BuiltinByteString
  arbitrary = stringToBS <$> vectorOf 8 (elements ['a'..'f'])

instance Arbitrary PubKeyHash where
  arbitrary = PubKeyHash <$> arbitrary

instance Arbitrary IdentityDatum where
  arbitrary = do
    submitter <- arbitrary
    personalHash <- arbitrary
    verified <- arbitrary
    return $ IdentityDatum submitter personalHash verified

instance Arbitrary IdentityAction where
  arbitrary = oneof
    [ SubmitIdentity <$> arbitrary
    , pure VerifyIdentity
    , ConfirmIdentity <$> arbitrary
    ]

-- ==== Property tests with QuickCheck ====

-- | Modified mockCtx function with specific setup for continuing outputs
mockCtxWithContinuingOutputs :: [PubKeyHash] -> [TxOut] -> [TxOut] -> POSIXTime -> ScriptContext
mockCtxWithContinuingOutputs signers ins outs _now = 
  let outCtx = ScriptContext
        { scriptContextTxInfo = TxInfo
            { txInfoInputs      = [ TxInInfo dummyRef (TxOut 
                                     { txOutAddress = Address (ScriptCredential (ValidatorHash (stringToBS "script"))) Nothing
                                     , txOutValue = mempty
                                     , txOutDatum = NoOutputDatum
                                     , txOutReferenceScript = Nothing
                                     }) ]  -- This creates an input from the script
            , txInfoOutputs     = outs
            , txInfoFee         = mempty
            , txInfoMint        = mempty
            , txInfoDCert       = []
            , txInfoWdrl        = AssocMap.empty
            , txInfoValidRange  = to _now
            , txInfoSignatories = signers
            , txInfoData        = AssocMap.empty
            , txInfoId          = TxId (stringToBS "txid")
            }
        , scriptContextPurpose = Spending dummyRef
        }
  in outCtx

prop_submit :: IdentityDatum -> Property
prop_submit dat@(IdentityDatum sub h _) =
  let out = mkOut (IdentityDatum sub h False)
      ctx = mockCtxWithContinuingOutputs [sub] [] [out] (POSIXTime 100)
  in collect (idVerified dat) $ mkIdentityValidator dat (SubmitIdentity h) ctx === True

prop_submit_badSigner :: IdentityDatum -> PubKeyHash -> Property
prop_submit_badSigner dat@(IdentityDatum sub h _) other =
  other /= sub ==>
    let out = mkOut (IdentityDatum sub h False)
        ctx = mockCtx [other] [] [out] (POSIXTime 100)
    in mkIdentityValidator dat (SubmitIdentity h) ctx === False

prop_verify :: IdentityDatum -> Property
prop_verify dat@(IdentityDatum sub h _) =
  let inp = mkOut dat
      ctx = mockCtx [sub] [inp] [] (POSIXTime 200)
  in mkIdentityValidator dat VerifyIdentity ctx === True

-- Fixed version of the confirm property test
prop_confirm :: Property
prop_confirm = 
  forAll arbitrary $ \sub ->
  forAll arbitrary $ \hash ->
  forAll arbitrary $ \flag ->
  let dat = IdentityDatum sub hash False
      outDat = IdentityDatum sub hash flag
      out = mkOut outDat
      ctx = mockCtxWithContinuingOutputs [sub] [] [out] (POSIXTime 300)
  in mkIdentityValidator dat (ConfirmIdentity flag) ctx === True

-- | Combine HUnit + QuickCheck into one test suite
tests :: TestTree
tests = testGroup "Identity Contract Tests"
  [ unitTests
  , testGroup "QuickCheck properties"
      [ testProperty "submit succeeds"       prop_submit
      , testProperty "submit fails with wrong signer" prop_submit_badSigner
      , testProperty "verify succeeds"       prop_verify
      , testProperty "confirm succeeds"      prop_confirm
      ]
  ]

main :: IO ()
main = defaultMain tests
