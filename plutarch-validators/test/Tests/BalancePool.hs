{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Tests.BalancePool where

import qualified WhalePoolsDex.PContracts.PPool as PPool
import qualified WhalePoolsDex.Contracts.Pool   as Pool
import WhalePoolsDex.Contracts.Proxy.FeeSwitch
import WhalePoolsDex.PValidators
import WhalePoolsDex.PConstants
import Data.Either

import Eval
import Hedgehog
import HaskellWorks.Hedgehog.Gen hiding (MonadGen)
import Gen.Utils hiding (Pool(..), TestAction(..), constructCase)

import PlutusLedgerApi.V2
import Plutarch.Api.V2 as PV2
import WhalePoolsDex.PConstants
import WhalePoolsDex.PMintingValidators

import Hedgehog

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog as HH

import Gen.Models
import Gen.DepositGen
import Gen.BalancePoolGen
import Gen.SwapGen
import Gen.RedeemGen
import Gen.DestroyGen
import Hedgehog.Range as Range
import Debug.Trace

balancePool = testGroup "BalancePool"
  ((genTests `map` [swapTests]))

genTests BalancePoolTestGroup{..} = 
  let
    failedCases  = (constructCase Failed) `map` invalidActions
    successCases = constructCase Success validAction
    
    --incorrectThreshold = treasholdCase contractAction $ constructIncorrectSignatureThresholdCase validAction
  in testGroup name ((testCases contractAction ([successCases] ++ failedCases))) -- ++ [incorrectThreshold])

testCases action cases =
   (\(name, propertyName, poolUpdater, testResult) ->
     HH.testPropertyNamed name propertyName (actionWithValidSignersQty 2 poolUpdater action testResult)
   ) `map` cases

treasholdCase action (name, propertyName, poolUpdater, testResult) =
  HH.testPropertyNamed name propertyName (actionWithValidSignersQty 1 poolUpdater action testResult)

-- Test groups --

swapTests = BalancePoolTestGroup
  { name = "Swap tests"
  , contractAction = Swap
  , validAction = correctSwap
  , invalidActions = []
  }

-----------------


actionWithValidSignersQty :: Int -> (BalancePool -> Gen BalancePoolActionResult) -> DAOAction -> TestResult -> Property
actionWithValidSignersQty sigsQty poolUpdater action testResultShouldBe = withShrinks 1 $ withTests 1 $ property $ do
  let
    threshold = 2

  (pkh1, pkh2, pkh3)  <- forAll $ tuple3 genPkh
  prevPool            <- forAll $ genBalancePool [pkh1, pkh2, pkh3] threshold True
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