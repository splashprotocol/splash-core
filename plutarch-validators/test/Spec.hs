module Main(main) where

import WhalePoolsDex.PMintingValidators

import Tests.Deposit
import Tests.Pool
import Tests.PoolBFee
import Tests.Swap
import Tests.Redeem
import Tests.Staking
import Tests.Api
import Tests.FeeSwitch
import Tests.FeeSwitchBFee
import Tests.BalancePool

import Test.Tasty
import Test.Tasty.HUnit

import WhalePoolsDex.PValidators
import PlutusLedgerApi.V2 as PV2
import Plutarch.Api.V2

main :: IO ()
main = do
  defaultMain tests

tests = testGroup "Contracts"
  [ feeSwitch
  , feeSwitchBFee
  , balancePool
  , checkPValueLength
  , checkPool
  , checkPoolRedeemer
  , checkPoolBFee
  , checkPoolBFeeRedeemer
  , checkRedeem
  , checkRedeemIdentity
  , checkRedeemIsFair
  , checkRedeemRedeemer
  , checkDeposit 
  , checkDepositChange
  , checkDepositRedeemer
  , checkDepositIdentity
  , checkDepositLq
  , checkDepositTokenReward
  , checkSwap
  , checkSwapRedeemer
  , checkSwapIdentity
  ]