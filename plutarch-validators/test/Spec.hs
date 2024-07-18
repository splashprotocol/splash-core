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
import qualified Data.ByteString as BS
import Plutarch.Api.V2
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Codec.Serialise (serialise, deserialise)
import Debug.Trace
import Codec.Serialise (serialise, deserialise)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Base16  as Hex
import qualified Data.Text as T
import qualified Data.Text.Encoding      as T
import Debug.Trace

main :: IO ()
main = defaultMain tests

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
