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

balancePool = testGroup "BalancePool"-- [HH.testPropertyNamed "name" "propertyName" test123]
  ((genTests `map` [depositSingleTests]))

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
  , contractAction = Pool.Swap
  , validAction = correctSwap
  , invalidActions = 
    [ incorrectSwapGT
    , incorrectSwapPoolFinalXValue
    , incorrectSwapPoolFinalYValue
    ]
  }

depositAllTests = BalancePoolTestGroup
  { name = "Deposit tests"
  , contractAction = Pool.Deposit
  , validAction = correctDeposit
  , invalidActions = [incorrectDepositLqOut]
  }

depositSingleTests = BalancePoolTestGroup
  { name = "Deposit single tests"
  , contractAction = Pool.DepositSingle
  , validAction = correctDepositSingle
  , invalidActions = []
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

-- test123 :: Property
-- test123 = withShrinks 100 $ withTests 100 $ property $ do
--   balanceX <- forAll $ integral (Range.constant 10000000000 10000000000000)
--   weightX  <- forAll $ integral (Range.constant 2 9)
--   let
--     balanceLength = length $ show balanceX

--     balanceXFloat = (fromIntegral balanceX) :: Double
--     weightXFloat  = (fromIntegral weightX) :: Double
--     gX = balanceXFloat ** (weightXFloat / (10 :: Double))
--     xT = balanceXFloat ** (1 / 10)
--     xTWeight = xT ** weightXFloat

--     cutGx = cutFloatD gX balanceLength
--     cutXTWeight = cutFloatD xTWeight balanceLength
  
--   cutGx === cutXTWeight


actionWithValidSignersQty :: Int -> (BalancePool -> Gen BalancePoolActionResult) -> Pool.BalancePoolAction -> TestResult -> Property
actionWithValidSignersQty sigsQty poolUpdater action testResultShouldBe = withTests 1 $ property $ do
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
    redeemer = toData $ Pool.BalancePoolRedeemer action 0 (g updateResult) (t updateResult) (maxDen updateResult)

    correctResult = 
      case testResultShouldBe of
        Success -> Right()
        Failed  -> Left()
  
    result = eraseBoth $ evalWithArgs (wrapValidator PPool.balancePoolValidatorT) [datum, redeemer, context]

  traceM $ show result

  result === correctResult