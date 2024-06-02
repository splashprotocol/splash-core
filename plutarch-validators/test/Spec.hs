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
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Codec.Serialise (serialise, deserialise)
import Debug.Trace

main :: IO ()
main = do
  let
    shortBSRedeem = validatorHash redeemBalanceValidator
    shortBSDeposit = validatorHash depositBalanceValidator
  traceM $ show shortBSRedeem
  traceM $ show shortBSDeposit
  -- BS.writeFile ("/home/bromel/projects/whalepools-core/plutarch-validators/redeem.uplc") shortBSRedeem
  -- BS.writeFile ("/home/bromel/projects/whalepools-core/plutarch-validators/deposit.uplc") shortBSDeposit
  pure ()

tests = testGroup "Contracts"
  [ 
  --   feeSwitch
  -- , feeSwitchBFee
  -- , 
  balancePool
  -- , checkPValueLength
  -- , checkPool
  -- , checkPoolRedeemer
  -- , checkPoolBFee
  -- , checkPoolBFeeRedeemer
  -- , checkRedeem
  -- , checkRedeemIdentity
  -- , checkRedeemIsFair
  -- , checkRedeemRedeemer
  -- , checkDeposit 
  -- , checkDepositChange
  -- , checkDepositRedeemer
  -- , checkDepositIdentity
  -- , checkDepositLq
  -- , checkDepositTokenReward
  -- , checkSwap
  -- , checkSwapRedeemer
  -- , checkSwapIdentity
  ]