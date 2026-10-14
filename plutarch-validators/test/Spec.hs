module Main (main) where

import Test.Tasty (defaultMain, testGroup)

import Tests.Api (checkPValueLength)
import Tests.BalancePool (balancePool)
import Tests.Deposit
import Tests.FeeSwitch (feeSwitch)
import Tests.FeeSwitchBFee (feeSwitchBFee)
import Tests.MainnetArtifacts (mainnetArtifacts)
import Tests.Pool
import Tests.PoolBFee
import Tests.PoolSafety (poolSafety)
import Tests.Redeem
import Tests.Swap

main :: IO ()
main = defaultMain $ testGroup "Contracts"
  [ mainnetArtifacts
  , feeSwitch
  , feeSwitchBFee
  , balancePool
  , checkPValueLength
  , checkPool
  , checkPoolRedeemer
  , checkPoolBFee
  , checkPoolBFeeRedeemer
  , poolSafety
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
