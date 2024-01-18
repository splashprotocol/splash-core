{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Tests.FeeSwitch where

import HaskellWorks.Hedgehog.Gen

import qualified WhalePoolsDex.PContracts.PPool as PPool
import qualified WhalePoolsDex.Contracts.Pool   as Pool
import WhalePoolsDex.Contracts.Proxy.FeeSwitch
import WhalePoolsDex.PValidators
import WhalePoolsDex.PConstants
import Data.Either

import Eval
import Gen.Utils hiding (BalancePool(..))

import PlutusLedgerApi.V2
import Plutarch.Api.V2 as PV2
import WhalePoolsDex.PConstants
import WhalePoolsDex.PMintingValidators

import Plutarch.Api.V1 ( PValue (..) )

import Hedgehog
import Hedgehog.Internal.Property

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog as HH

import Gen.FeeSwitchGen
import Gen.Models (mkAdaValue, genPkh, mkContext)
import qualified Gen.FeeSwitchGen as TestAction
import Data.List (intersperse, take)
import Data.Char

import Debug.Trace

feeSwitch = testGroup "FeeSwitchV1"
  ((genTests `map` [daoSwitchTests , treasuryFeeTests,  treasuryAddressTests, stakeAddressTests, treasuryWithdrawTests, poolFeeTests]) ++ [lpFeeEditableTest, incorrectPoolInputTest])

genTests TestGroup{..} = 
  let
    failedCases  = (constructCase Failed) `map` invalidActions
    successCases = constructCase Success validAction
    
    incorrectThreshold = treasholdCase contractAction $ constructIncorrectSignatureThresholdCase validAction
  in testGroup name ((testCases contractAction ([successCases] ++ failedCases)) ++ [incorrectThreshold])

testCases action cases =
   (\(name, propertyName, poolUpdater, testResult) ->
     HH.testPropertyNamed name propertyName (actionWithValidSignersQty 2 poolUpdater action testResult)
   ) `map` cases

treasholdCase action (name, propertyName, poolUpdater, testResult) =
  HH.testPropertyNamed name propertyName (actionWithValidSignersQty 1 poolUpdater action testResult)

daoSwitchTests = TestGroup
  { name = "DAO Switch tests"
  , contractAction = ChangeAdminAddress
  , validAction = changeDAOAdminAddress
  , invalidActions = 
    [ changePoolTreasury
    , changeTreasuryFee
    , changePoolValue
    , changePoolTokensInfo
    , changeTreasuryAddress
    , changeStakePartOfAddress
    , withdrawTreasury
    ]
  }

treasuryFeeTests = TestGroup
  { name = "Treasury Fee tests"
  , contractAction = ChangeTreasuryFee
  , validAction = changeTreasuryFee
  , invalidActions = 
    [ changePoolTreasury
    , changeDAOAdminAddress
    , changePoolValue
    , changePoolTokensInfo
    , changeTreasuryAddress
    , changeStakePartOfAddress
    , withdrawTreasury
    , incorrectTreasuryFee
    ]
  }

treasuryAddressTests = TestGroup
  { name = "Treasury Address tests"
  , contractAction = ChangeTreasuryAddress
  , validAction = changeTreasuryAddress
  , invalidActions = 
    [ changePoolTreasury
    , changeDAOAdminAddress
    , changePoolValue
    , changePoolTokensInfo
    , changeTreasuryFee
    , changeStakePartOfAddress
    , withdrawTreasury
    ]
  }

stakeAddressTests = TestGroup
  { name = "Stake Address tests"
  , contractAction = ChangeStakePart
  , validAction = changeStakePartOfAddress
  , invalidActions = 
    [ changePoolTreasury
    , changeDAOAdminAddress
    , changePoolValue
    , changePoolTokensInfo
    , changeTreasuryFee
    , changeTreasuryAddress
    , withdrawTreasury
    ]
  }

treasuryWithdrawTests = TestGroup
  { name = "Treasury withdraw tests"
  , contractAction = WithdrawTreasury
  , validAction = withdrawTreasury
  , invalidActions = 
    [ changePoolTreasury
    , changeStakePartOfAddress
    , changeDAOAdminAddress
    , changePoolValue
    , changePoolTokensInfo
    , changeTreasuryFee
    , changeTreasuryAddress
    , incorrectWithdrawValueTreasury
    , incorrectWithdrawAddressTreasury
    ]
  }

poolFeeTests = TestGroup
  { name = "Pool fee tests"
  , contractAction = ChangePoolFee
  , validAction = changePoolFee
  , invalidActions = 
    [ changePoolTreasury
    , changeStakePartOfAddress
    , changeDAOAdminAddress
    , changePoolValue
    , changePoolTokensInfo
    , changeTreasuryFee
    , changeTreasuryAddress
    , withdrawTreasury
    , incorrectChangePoolFee
    ]
  }
  
actionWithValidSignersQty :: Int -> (Pool -> Gen ActionResult) -> DAOAction -> TestResult -> Property
actionWithValidSignersQty sigsQty poolUpdater action testResultShouldBe = withShrinks 1 $ withTests 1 $ property $ do
  let
    threshold = 2

  (pkh1, pkh2, pkh3)  <- forAll $ tuple3 genPkh
  prevPool            <- forAll $ genPool [pkh1, pkh2, pkh3] threshold True
  updateResult        <- forAll $ poolUpdater prevPool

  txInInfo <- forAll $ createTxInfo prevPool updateResult (take sigsQty [pkh1, pkh2, pkh3])
  let
    purpose  = daoMintingPurpose prevPool
    context  = toData $ mkContext txInInfo purpose
    redeemer = toData $ DAORedeemer action 0

  traceM $ show redeemer

  let 
    correctResult = 
      case testResultShouldBe of
        Success -> Right()
        Failed  -> Left()

    result = eraseBoth $ evalWithArgs (daoValidator prevPool [pkh1, pkh2, pkh3] threshold True) [redeemer, context]
  
  result === correctResult

lpFeeEditableTest = HH.testPropertyNamed "Pool fee tests" "fail_if_lpFeeEditable_is_false" lpFeeEditableProperty

lpFeeEditableProperty :: Property
lpFeeEditableProperty = withShrinks 1 $ withTests 1 $ property $ do
  let
    threshold = 2

  (pkh1, pkh2, pkh3)  <- forAll $ tuple3 genPkh
  prevPool            <- forAll $ genPool [pkh1, pkh2, pkh3] threshold False
  updateResult        <- forAll $ (Gen.Utils.action changePoolFee) prevPool

  txInInfo <- forAll $ createTxInfo prevPool updateResult (take 2 [pkh1, pkh2, pkh3])
  let
    purpose  = daoMintingPurpose prevPool
    context  = toData $ mkContext txInInfo purpose
    redeemer = toData $ DAORedeemer ChangePoolFee 0

    result = eraseBoth $ evalWithArgs (daoValidator prevPool [pkh1, pkh2, pkh3] threshold False) [redeemer, context]
  
  result === Left()

incorrectPoolInputTest = HH.testPropertyNamed "Incorrect pool input tests" "fail_if_pool_nft_is_not_preserved_in_indexed_input" incorrectPoolInputProperty

incorrectPoolInputProperty :: Property
incorrectPoolInputProperty = withShrinks 1 $ withTests 1 $ property $ do
  let
    threshold = 2

  (pkh1, pkh2, pkh3)  <- forAll $ tuple3 genPkh
  prevPool@Pool{..}   <- forAll $ genPool [pkh1, pkh2, pkh3] threshold False
  let
    newPoolValue      = filterAssetClass (Pool.poolNft $ config) value
    fakePool          = prevPool {
      value  = newPoolValue
    }
  updateResult        <- forAll $ (Gen.Utils.action withdrawTreasury) prevPool

  txInInfo <- forAll $ createTxInfoCustom prevPool [toTxOut fakePool] ([toTxOut (newPool updateResult)] ++ additionalOutputs updateResult) [] (take 2 [pkh1, pkh2, pkh3])
  let
    purpose  = daoMintingPurpose prevPool
    context  = toData $ mkContext txInInfo purpose
    redeemer = toData $ DAORedeemer WithdrawTreasury 1

    result = eraseBoth $ evalWithArgs (daoValidator prevPool [pkh1, pkh2, pkh3] threshold False) [redeemer, context]
  
  result === Left()