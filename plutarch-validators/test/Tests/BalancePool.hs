{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Tests.BalancePool where

import qualified WhalePoolsDex.PContracts.PBalancePool as PPool
import qualified WhalePoolsDex.Contracts.BalancePool   as Pool
import WhalePoolsDex.Contracts.Proxy.FeeSwitch
import WhalePoolsDex.PValidators
import WhalePoolsDex.PConstants
import Data.Either

import Eval
import Hedgehog
import Numeric
import HaskellWorks.Hedgehog.Gen hiding (MonadGen)
import Gen.Utils hiding (Pool(..), TestAction(..), constructCase)

import PlutusLedgerApi.V2
import Plutarch.Api.V2 as PV2
import WhalePoolsDex.PConstants
import WhalePoolsDex.PMintingValidators

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
import Data.Text as T (pack, unpack, splitOn)

balancePool = testGroup "BalancePool"
  ((genTests `map` [swapTests, depositAllTests, redeemAllTests]))

validPoolHash :: Property
validPoolHash = withTests 1 $ property $ do
  let
    actualPoolValidatorHash = PV2.validatorHash poolValidator
  actualPoolValidatorHash === poolValidatorHash

genTests BalancePoolTestGroup{..} = 
  let
    failedCases  = (constructCase Failed) `map` invalidActions
    successCases = constructCase Success validAction
  in testGroup name ((testCases contractAction ([successCases] ++ failedCases)))

testCases action cases =
   (\(name, propertyName, poolUpdater, testResult) ->
     HH.testPropertyNamed name propertyName (actionWithValidSignersQty 2 poolUpdater action testResult)
   ) `map` cases

treasholdCase action (name, propertyName, poolUpdater, testResult) =
  HH.testPropertyNamed name propertyName (actionWithValidSignersQty 1 poolUpdater action testResult)

-- Test groups --

swapTests = BalancePoolTestGroup
  { name = "Swap tests"
  , contractAction = Pool.Swap
  , validAction = correctSwap
  , invalidActions = 
    [ incorrectSwapGT
    , incorrectSwapPoolFinalXValue
    , incorrectSwapPoolFinalYValue
    , incorrectSwapTrFeeValue
    ]
  }

depositAllTests = BalancePoolTestGroup
  { name = "Deposit tests"
  , contractAction = Pool.Deposit
  , validAction = correctDeposit
  , invalidActions = [incorrectDepositLqOut]
  }

redeemAllTests = BalancePoolTestGroup
  { name = "Redeem tests"
  , contractAction = Pool.Redeem
  , validAction = correctRedeem
  , invalidActions = [incorrectRedeemLQFinalValue]
  }

-----------------

cutFloatD :: Double -> Int -> Integer
cutFloatD toCut maxInt = let
    strValue = T.pack $ showFFloat (Just maxInt) toCut ""
    splitted = T.splitOn "." strValue
  in read $ T.unpack . Prelude.head $ splitted

actionWithValidSignersQty :: Int -> (BalancePool -> Gen BalancePoolActionResult) -> Pool.BalancePoolAction -> TestResult -> Property
actionWithValidSignersQty sigsQty poolUpdater action testResultShouldBe = withShrinks 1 $ withTests 10 $ property $ do
  let
    threshold = 2

  (pkh1, pkh2, pkh3)  <- forAll $ tuple3 genPkh
  prevPool            <- forAll $ genBalancePool [pkh1, pkh2, pkh3] threshold True
  updateResult        <- forAll $ poolUpdater prevPool
  txInInfo <- forAll $ createTxInfo prevPool updateResult (take sigsQty [pkh1, pkh2, pkh3])
  let
    purpose  = mkPurpose (txInInfoOutRef . head . txInfoInputs $ txInInfo)
  let
    datum    = toData $ (config prevPool)
  let
    context  = toData $ mkContext txInInfo purpose
    redeemer = toData $ Pool.BalancePoolRedeemer action 0 (g updateResult) (t updateResult) (lList updateResult)

    correctResult = 
      case testResultShouldBe of
        Success -> Right()
        Failed  -> Left()
  
    result = eraseBoth $ evalWithArgs (wrapValidator PPool.balancePoolValidatorT) [datum, redeemer, context]

  result === correctResult