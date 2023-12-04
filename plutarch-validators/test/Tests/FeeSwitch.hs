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
import Gen.Utils

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
  (genTests `map` [daoSwitchTests , treasuryFeeTests,  treasuryAddressTests, stakeAddressTests, treasuryWithdrawTests])

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
  
actionWithValidSignersQty :: Int -> (Pool -> Gen ActionResult) -> DAOAction -> TestResult -> Property
actionWithValidSignersQty sigsQty poolUpdater action testResultShouldBe = withShrinks 1 $ withTests 1 $ property $ do
  let
    threshold = 2

  (pkh1, pkh2, pkh3)  <- forAll $ tuple3 genPkh
  prevPool            <- forAll $ genPool [pkh1, pkh2, pkh3] threshold
  updateResult        <- forAll $ poolUpdater prevPool

  txInInfo <- forAll $ createTxInfo prevPool updateResult (take sigsQty [pkh1, pkh2, pkh3])
  let
    purpose  = daoMintingPurpose prevPool
    context  = toData $ mkContext txInInfo purpose
    redeemer = toData $ DAORedeemer action 0

    correctResult = 
      case testResultShouldBe of
        Success -> Right()
        Failed  -> Left()

    result = eraseBoth $ evalWithArgs (daoValidator prevPool [pkh1, pkh2, pkh3] threshold) [redeemer, context]
  
  result === correctResult