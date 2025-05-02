{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DeriveAnyClass        #-}
 
module Main where
 
import           PlutusTx.Prelude                (Bool(..), (&&), Bool(..), mempty)
import           PlutusTx                        (toBuiltinData, fromBuiltinData)
import           Plutus.V2.Ledger.Api            (PubKeyHash(..), TxOut(..), OutputDatum(..), Datum(..), Address(..), Credential(..), TxOutRef(..), TxId(..))
import           Plutus.V2.Ledger.Contexts       (ScriptContext(..), TxInfo(..), ScriptPurpose(..))
import qualified PlutusTx.AssocMap               as AssocMap
import           Plutus.V1.Ledger.Interval       (always)
import           Test.Tasty                      (defaultMain, testGroup, TestTree)
import           Test.Tasty.QuickCheck           (testProperty, (.&&.), (==>), Arbitrary(..), Gen, oneof, vectorOf, Property)
import           PlutusTx.Builtins               (BuiltinByteString, toBuiltin, sha2_256)
import qualified Data.ByteString                 as BS
import           Data.ByteString                 (ByteString, pack)
import           GHC.Generics                    (Generic)
import           Prelude                         ((==), IO, Maybe(..), Eq(..),Bool(..), not, (<$>), (.), (<*>), ($))
import           Key.Key                         (KeyDatum(..), KeyAction(..), mkKeyValidator)
 
-- | Standalone Eq instances for comparing round-tripped data/actions
deriving instance Eq KeyDatum
deriving instance Eq KeyAction
 
-- | Arbitrary instances for testing
instance Arbitrary BuiltinByteString where
  arbitrary = toBuiltin . BS.pack <$> vectorOf 32 arbitrary  -- 32 bytes
 
instance Arbitrary PubKeyHash where
  arbitrary = PubKeyHash . toBuiltin . BS.pack <$> vectorOf 28 arbitrary
 
instance Arbitrary KeyDatum where
  arbitrary = KeyDatum <$> arbitrary <*> arbitrary
 
instance Arbitrary KeyAction where
  arbitrary = oneof
    [ GenerateKey <$> arbitrary <*> arbitrary
    , ValidateKey <$> arbitrary
    ]
 
-- | Build a minimal TxInfo record with empty maps/range
mkCtx :: [PubKeyHash] -> [TxOut] -> ScriptContext
mkCtx signers outs = ScriptContext txInfo (Spending dummyRef)
  where
    txInfo = TxInfo
      { txInfoInputs      = []
      , txInfoOutputs     = outs
      , txInfoFee         = mempty
      , txInfoMint        = mempty
      , txInfoDCert       = []
      , txInfoWdrl        = AssocMap.empty
      , txInfoValidRange  = always
      , txInfoSignatories = signers
      , txInfoData        = AssocMap.empty
      , txInfoId          = TxId ""
      }
    dummyRef = TxOutRef (TxId "") 0
 
-- | Helper to construct a script output carrying a KeyDatum
mkOut :: KeyDatum -> TxOut
mkOut dat = TxOut
  { txOutAddress          = Address (PubKeyCredential (kdGuest dat)) Nothing
  , txOutValue            = mempty
  , txOutDatum            = OutputDatum (Datum $ toBuiltinData dat)
  , txOutReferenceScript  = Nothing
  }
 
-- 1) GenerateKey must succeed only when admin signs and output matches
 
prop_generateKey_notAdmin :: PubKeyHash -> PubKeyHash -> BuiltinByteString -> PubKeyHash -> Property
prop_generateKey_notAdmin admin guest secret other =
  other /= admin ==>  
    let dat = KeyDatum guest secret
        act = GenerateKey guest secret
        ctx = mkCtx [other] [mkOut dat]
    in not (mkKeyValidator admin dat act ctx)
 
-- 2) ValidateKey must succeed when guest signs and preimage matches
prop_validateKey_success :: KeyDatum -> Bool
prop_validateKey_success dat@(KeyDatum guest secretHash) =
    let preimage = secretHash
        act      = ValidateKey preimage
        ctx      = mkCtx [guest] []
    in mkKeyValidator guest dat act ctx
 
prop_validateKey_failure :: KeyDatum -> BuiltinByteString -> Property
prop_validateKey_failure dat@(KeyDatum guest secretHash) wrong =
  wrong /= secretHash ==>  
    let act = ValidateKey wrong
        ctx = mkCtx [guest] []
    in not (mkKeyValidator guest dat act ctx)
 
-- 3) Round‑trip via builtin data
prop_datum_roundtrip :: KeyDatum -> Bool
prop_datum_roundtrip dt = case fromBuiltinData (toBuiltinData dt) of
    Just dt' -> dt' == dt
    _        -> False
 
prop_action_roundtrip :: KeyAction -> Bool
prop_action_roundtrip ac = case fromBuiltinData (toBuiltinData ac) of
    Just ac' -> ac' == ac
    _        -> False
 
-- | All tests
tests :: TestTree
tests = testGroup "CICO.Key Validator properties"
  [ testProperty "Datum round‑trip"        prop_datum_roundtrip
  , testProperty "Action round‑trip"       prop_action_roundtrip
  , testProperty "Reject non-admin generate" prop_generateKey_notAdmin
  , testProperty "ValidateKey success"     prop_validateKey_success
  , testProperty "ValidateKey failure"     prop_validateKey_failure
  ]
 
main :: IO ()
main = defaultMain tests
 