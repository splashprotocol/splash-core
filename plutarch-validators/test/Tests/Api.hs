{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE FlexibleContexts #-}

module Tests.Api where

import qualified WhalePoolsDex.PContracts.PDeposit as PDeposit
import WhalePoolsDex.PValidators

import Eval
import Gen.Utils

import Hedgehog

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog as HH

import Gen.Models
import Gen.DepositGen
import Gen.PoolGen

import PlutusLedgerApi.V2

import Plutarch
import Plutarch.Prelude
import Plutarch.DataRepr
import Plutarch.Api.V2.Contexts
import Plutarch.Lift
import PExtra.Ada
import qualified PExtra.API as API
import Debug.Trace

import Plutarch.Api.V1 (PValue (..))

checkPValueLength = testGroup "pValueLengthTests"
  [ HH.testProperty "pValueLength_correct_token_value_same_policy_id" correctTokensQtySamePolicyId
  , HH.testProperty "pValueLength_correct_token_value_different_policy_id" correctTokensQtyDifferentPolicyId
  , HH.testProperty "pValueLength_incorrect_token_value_same_policy_id" incorrectTokensQtyDifferentPolicyId
  , HH.testProperty "foreign token quantity is preserved" foreignTokenPreserved
  , HH.testProperty "foreign token cannot be skimmed with same asset count" foreignTokenSkimRejected
  , HH.testProperty "foreign token cannot be replaced" foreignTokenReplacementRejected
  , HH.testProperty "four distinct pool assets are accepted" distinctPoolAssetsAccepted
  , HH.testProperty "pool asset identities must be distinct" overlappingPoolAssetsRejected
  ]

preservesOtherAssets before after x y nft lq =
  pif (API.pPreserveOtherAssets # pconstant before # pconstant after
        # pconstant x # pconstant y # pconstant lq # pconstant nft)
      (pcon PTrue) perror

foreignTokenPreserved :: Property
foreignTokenPreserved = withTests 1 $ property $ do
  let (x, y, nft, lq) = genAssetClasses
      before = mkValues [mkValue x 10, mkValue y 10, mkValue nft 1,
                         mkValue lq 10, mkValue genFakeToken 7] mempty
      after = mkValues [mkValue x 12, mkValue y 8, mkValue nft 1,
                        mkValue lq 11, mkValue genFakeToken 7] mempty
      result = eraseBoth $ evalWithArgs (preservesOtherAssets before after x y nft lq) []
  result === Right ()

foreignTokenSkimRejected :: Property
foreignTokenSkimRejected = withTests 1 $ property $ do
  let (x, y, nft, lq) = genAssetClasses
      before = mkValues [mkValue x 10, mkValue genFakeToken 7] mempty
      after = mkValues [mkValue x 12, mkValue genFakeToken 6] mempty
      result = eraseBoth $ evalWithArgs (preservesOtherAssets before after x y nft lq) []
  result === Left ()

foreignTokenReplacementRejected :: Property
foreignTokenReplacementRejected = withTests 1 $ property $ do
  let (x, y, nft, lq) = genAssetClasses
      before = mkValue genFakeToken 7
      after = mkValue x 7
      result = eraseBoth $ evalWithArgs (preservesOtherAssets before after x y nft lq) []
  result === Left ()

distinctPoolAssetsAccepted :: Property
distinctPoolAssetsAccepted = withTests 1 $ property $ do
  let (x, y, nft, lq) = genAssetClasses
      term = pif (API.pDistinctPoolAssets # pconstant x # pconstant y
                  # pconstant lq # pconstant nft) (pcon PTrue) perror
      result = eraseBoth $ evalWithArgs term []
  result === Right ()

overlappingPoolAssetsRejected :: Property
overlappingPoolAssetsRejected = withTests 1 $ property $ do
  let (x, y, nft, _) = genAssetClasses
      term = pif (API.pDistinctPoolAssets # pconstant x # pconstant y
                  # pconstant x # pconstant nft) (pcon PTrue) perror
      result = eraseBoth $ evalWithArgs term []
  result === Left ()

-- wrapper around pValueLength
internalPValueLengthTest :: ClosedTerm (PValue _ _ :--> PInteger :--> PBool)
internalPValueLengthTest = plam $ \testValue testQty -> unTermCont $ do
    let realQty     = API.pValueLength # testValue
    pure $ pif (realQty #== testQty) (pcon PTrue) perror

correctTokensQtySamePolicyId :: Property
correctTokensQtySamePolicyId = withTests 1 $ property $ do
  let 
    (x, y, nft, lq) = genAssetClasses
    -- x, y, lq, nft have same policy
    value     = mkValues [mkValue x 10, mkValue y 10, mkValue lq 10, mkValue nft 10, mkValue genFakeToken 100, mkAdaValue 10] mempty

    correctTokenQty = 6

    result = eraseRight $ evalWithArgs (internalPValueLengthTest # (pconstant value) # (pconstant correctTokenQty)) []

  result === Right ()

correctTokensQtyDifferentPolicyId :: Property
correctTokensQtyDifferentPolicyId = withTests 1 $ property $ do
  (x, y, nft, lq) <- forAll genRandomAssetClasses
  let
    value     = mkValues [mkValue x 10, mkValue y 10, mkValue lq 10, mkValue nft 10, mkAdaValue 10] mempty
    correctTokenQty = 5
    result = eraseRight $ evalWithArgs (internalPValueLengthTest # (pconstant value) # (pconstant correctTokenQty)) []

  result === Right ()

incorrectTokensQtyDifferentPolicyId :: Property
incorrectTokensQtyDifferentPolicyId = withTests 1 $ property $ do
  (x, y, nft, _) <- forAll genRandomAssetClasses
  let
    value     = mkValues [mkValue x 10, mkValue y 10, mkValue nft 10, mkAdaValue 10] mempty
    incorrectTokenQty = 5
    result = eraseLeft $ evalWithArgs (internalPValueLengthTest # (pconstant value) # (pconstant incorrectTokenQty)) []

  result === Left ()
