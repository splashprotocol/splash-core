{-# LANGUAGE OverloadedStrings #-}

module Tests.BalancePool where

import qualified WhalePoolsDex.PContracts.PPool as PPool
import qualified WhalePoolsDex.Contracts.Pool   as Pool
import WhalePoolsDex.PValidators
import WhalePoolsDex.PConstants
import Data.Either

import Eval
import Hedgehog
import Hedgehog.Gen
import HaskellWorks.Hedgehog.Gen hiding (MonadGen)
import Gen.Utils

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
import Gen.PoolGen
import Gen.SwapGen
import Gen.RedeemGen
import Gen.DestroyGen
import Hedgehog.Range as Range
import Debug.Trace

balancePool = testGroup "BalancePool"
  ((genTests `map` []))

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