{-# LANGUAGE OverloadedStrings #-}

module Tests.Pool where

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

checkPool = testGroup "CheckPoolContract"
  [ HH.testProperty "pool_validator_hash_is_correct" validPoolHash
  , HH.testProperty "pool_change_stake_part_is_correct (correct minting)" successPoolChangeStakePartCorrectMinting
  , HH.testProperty "pool_change_stake_part_is_incorrect (incorrect minting)" failedPoolChangeStakePartIncorrectMinting
  , HH.testProperty "pool_deposit_is_correct" successPoolDeposit
  , HH.testProperty "pool_swap_is_correct" successPoolSwap
  , HH.testProperty "pool_swap_incorrect_treasury" incorrectPoolYTreasury
  , HH.testProperty "pool_swap_incorrect_datum" incorrectPoolSwapDatum
  , HH.testProperty "incorrect_final_pool_tokens_qty" incorrectPoolTokensQtyInFinalValue
  , HH.testProperty "pool_swap_insufficient_lq_for_bound" poolSwapInsufficientLiqudityForBound
  , HH.testProperty "pool_redeem_is_correct" successPoolRedeem
  , HH.testProperty "pool_swap_additional_tokens" incorrectPoolSwapAdditionalTokens
  , HH.testProperty "pool_redeem_too_much_liquidity_removed" (poolRedeemLqCheck 9 9 9223372036854775797)
  , HH.testProperty "pool_redeem_liquidity_removed_lq_intact" (poolRedeemLqCheck 19 19 9223372036854775787)
  , HH.testProperty "pool_redeem_liquidity_intact_lq_removed" (poolRedeemLqCheck 20 20 9223372036854775786)
  , HH.testProperty "pool_destroy_lq_burnLqInitial" (poolDestroyCheck (Pool.maxLqCap - Pool.burnLqInitial) (Right ()))
  , HH.testProperty "pool_destroy_lq_burnLqInitial-1" (poolDestroyCheck (Pool.maxLqCap - Pool.burnLqInitial + 1) (Right ()))
  , HH.testProperty "pool_destroy_lq_burnLqInitial+1" (poolDestroyCheck (Pool.maxLqCap - Pool.burnLqInitial - 1) (Left ()))
  , HH.testProperty "pool_destroy_incorrect_pool_input" (poolDestroyCheckIncorrectPoolInput (Pool.maxLqCap - Pool.burnLqInitial))
  ]

checkPoolRedeemer = testGroup "CheckPoolRedeemer"
  [ HH.testProperty "fail_if_pool_ix_is_incorrect_deposit" poolDepositRedeemerIncorrectIx
  , HH.testProperty "fail_if_pool_action_is_incorrect_deposit_to_swap" (poolDepositRedeemerIncorrectAction Pool.Swap)
  , HH.testProperty "fail_if_pool_ix_is_incorrect_swap" poolSwapRedeemerIncorrectIx
  , HH.testProperty "fail_if_pool_action_is_incorrect_swap_to_deposit" (poolSwapRedeemerIncorrectAction Pool.Deposit)
  , HH.testProperty "fail_if_pool_action_is_incorrect_swap_to_redeem" (poolSwapRedeemerIncorrectAction Pool.Redeem)
  , HH.testProperty "fail_if_pool_ix_is_incorrect_redeem" poolRedeemRedeemerIncorrectIx
  , HH.testProperty "fail_if_pool_action_is_incorrect_redeem_to_swap" (poolRedeemRedeemerIncorrectAction Pool.Swap)
  ]

validPoolHash :: Property
validPoolHash = withTests 1 $ property $ do
  let
    actualPoolValidatorHash = PV2.validatorHash poolValidator
  actualPoolValidatorHash === poolValidatorHash

poolDestroyCheck :: Integer -> Either () () -> Property
poolDestroyCheck lqQty expected = withTests 1 $ property $ do
  (x, y, nft, lq) <- forAll genRandomAssetClasses

  treasuryAddress <- forAll genValidatorHash
  poolTxRef <- forAll genTxOutRef
  let
    (pcfg, pdh) = genPConfig x y nft lq 9950 1 [] 0 treasuryAddress
    poolTxIn    = genDTxIn poolTxRef pdh lq lqQty nft 1 5000000
  
    txInfo  = mkDTxInfo [poolTxIn]
    purpose = mkPurpose poolTxRef

    cxtToData        = toData $ mkContext txInfo purpose
    poolRedeemToData = toData $ mkPoolRedeemer 0 Pool.Destroy

    result = eraseBoth $ evalWithArgs (wrapValidator PPool.poolValidatorT) [pcfg, poolRedeemToData, cxtToData]

  result === expected

poolDestroyCheckIncorrectPoolInput :: Integer -> Property
poolDestroyCheckIncorrectPoolInput lqQty = withTests 1 $ property $ do
  (x, y, nft, lq) <- forAll genRandomAssetClasses

  (_, _, fakeNft, _) <- forAll genRandomAssetClasses
  treasuryAddress <- forAll genValidatorHash
  poolTxRef      <- forAll genTxOutRef
  incorrectTxRef <- forAll genTxOutRef
  let
    (pcfg, pdh) = genPConfig x y nft lq 9950 1 [] 0 treasuryAddress
    poolTxIn    = genPTxIn poolTxRef pdh x 20 y 20 lq 9223372036854775787 nft 1 5000000
    incorrectPoolTxIn = genDTxIn incorrectTxRef pdh lq lqQty fakeNft 1 5000000 

    txInfo  = mkDTxInfo [incorrectPoolTxIn, poolTxIn]
    purpose = mkPurpose poolTxRef

    cxtToData        = toData $ mkContext txInfo purpose
    poolRedeemToData = toData $ mkPoolRedeemer 0 Pool.Destroy

    result = eraseLeft $ evalWithArgs (wrapValidator PPool.poolValidatorT) [pcfg, poolRedeemToData, cxtToData]

  result === Left ()

successPoolRedeem :: Property
successPoolRedeem = withTests 1 $ property $ do
  (x, y, nft, lq) <- forAll genRandomAssetClasses
  pkh             <- forAll genPkh
  treasuryAddress <- forAll genValidatorHash      
  orderTxRef      <- forAll genTxOutRef
  let
    (cfgData, dh) = genRConfig x y lq nft 100 pkh
    orderTxIn     = genRTxIn orderTxRef dh lq 10 5000000
    orderTxOut    = genRTxOut dh x 10 y 10 5000000 pkh
  
  poolTxRef <- forAll genTxOutRef
  let
    (pcfg, pdh) = genPConfig x y nft lq 9950 1 [] 0 treasuryAddress
    poolTxIn    = genPTxIn poolTxRef pdh x 20 y 20 lq 9223372036854775787 nft 1 5000000
    poolTxOut   = genPTxOut pdh x 10 y 10 lq 9223372036854775797 nft 1 3000000
  
  let
    txInfo  = mkTxInfo poolTxIn orderTxIn poolTxOut orderTxOut
    purpose = mkPurpose poolTxRef

    cxtToData        = toData $ mkContext txInfo purpose
    poolRedeemToData = toData $ mkPoolRedeemer 0 Pool.Redeem

    result = eraseRight $ evalWithArgs (wrapValidator PPool.poolValidatorT) [pcfg, poolRedeemToData, cxtToData]

  result === Right ()

poolRedeemLqCheck :: Integer -> Integer -> Integer -> Property
poolRedeemLqCheck xQty yQty lqOutQty = withTests 1 $ property $ do
  (x, y, nft, lq) <- forAll genRandomAssetClasses
  pkh             <- forAll genPkh
  treasuryAddress <- forAll genValidatorHash      
  orderTxRef      <- forAll genTxOutRef
  let
    (cfgData, dh) = genRConfig x y lq nft 100 pkh
    orderTxIn     = genRTxIn orderTxRef dh lq 10 5000000
    orderTxOut    = genRTxOut dh x 10 y 10 5000000 pkh
  
  poolTxRef <- forAll genTxOutRef
  let
    (pcfg, pdh) = genPConfig x y nft lq 9950 1 [] 0 treasuryAddress
    poolTxIn    = genPTxIn poolTxRef pdh x 20 y 20 lq 9223372036854775787 nft 1 5000000
    poolTxOut   = genPTxOut pdh x xQty y yQty lq lqOutQty nft 1 3000000
  
  let
    txInfo  = mkTxInfo poolTxIn orderTxIn poolTxOut orderTxOut
    purpose = mkPurpose poolTxRef

    cxtToData        = toData $ mkContext txInfo purpose
    poolRedeemToData = toData $ mkPoolRedeemer 0 Pool.Redeem

    result = eraseBoth $ evalWithArgs (wrapValidator PPool.poolValidatorT) [pcfg, poolRedeemToData, cxtToData]

  result === Left ()

poolRedeemRedeemerIncorrectIx :: Property
poolRedeemRedeemerIncorrectIx = withTests 1 $ property $ do
  (x, y, nft, lq) <- forAll genRandomAssetClasses
  pkh             <- forAll genPkh
  treasuryAddress <- forAll genValidatorHash      
  orderTxRef      <- forAll genTxOutRef
  let
    (cfgData, dh) = genRConfig x y lq nft 100 pkh
    orderTxIn     = genRTxIn orderTxRef dh lq 10 5000000
    orderTxOut    = genRTxOut dh x 10 y 10 5000000 pkh
  
  poolTxRef <- forAll genTxOutRef
  let
    (pcfg, pdh) = genPConfig x y nft lq 9950 1 [] 0 treasuryAddress
    poolTxIn    = genPTxIn poolTxRef pdh x 20 y 20 lq 9223372036854775787 nft 1 5000000
    poolTxOut   = genPTxOut pdh x 10 y 10 lq 9223372036854775797 nft 1 3000000
  
  let
    txInfo  = mkTxInfo poolTxIn orderTxIn poolTxOut orderTxOut
    purpose = mkPurpose poolTxRef

    cxtToData        = toData $ mkContext txInfo purpose
    poolRedeemToData = toData $ mkPoolRedeemer 1 Pool.Redeem

    result = eraseBoth $ evalWithArgs (wrapValidator PPool.poolValidatorT) [pcfg, poolRedeemToData, cxtToData]

  result === Left ()

poolRedeemRedeemerIncorrectAction :: Pool.PoolAction -> Property
poolRedeemRedeemerIncorrectAction action = withTests 1 $ property $ do
  (x, y, nft, lq) <- forAll genRandomAssetClasses
  pkh             <- forAll genPkh
  treasuryAddress <- forAll genValidatorHash      
  orderTxRef      <- forAll genTxOutRef
  let
    (cfgData, dh) = genRConfig x y lq nft 100 pkh
    orderTxIn     = genRTxIn orderTxRef dh lq 10 5000000
    orderTxOut    = genRTxOut dh x 10 y 10 5000000 pkh
  
  poolTxRef <- forAll genTxOutRef
  let
    (pcfg, pdh) = genPConfig x y nft lq 9950 1 [] 0 treasuryAddress
    poolTxIn    = genPTxIn poolTxRef pdh x 20 y 20 lq 9223372036854775787 nft 1 5000000
    poolTxOut   = genPTxOut pdh x 10 y 10 lq 9223372036854775797 nft 1 3000000
  
  let
    txInfo  = mkTxInfo poolTxIn orderTxIn poolTxOut orderTxOut
    purpose = mkPurpose poolTxRef

    cxtToData        = toData $ mkContext txInfo purpose
    poolRedeemToData = toData $ mkPoolRedeemer 0 action

    result = eraseBoth $ evalWithArgs (wrapValidator PPool.poolValidatorT) [pcfg, poolRedeemToData, cxtToData]

  result === Left ()

successPoolSwap :: Property
successPoolSwap = withShrinks 1 $ withTests 10 $ property $ do
  let (x, y, nft, lq) = genAssetClasses
  pkh             <- forAll genPkh
  treasuryAddress <- forAll genValidatorHash      
  orderTxRef      <- forAll genTxOutRef
  prevPoolX       <- forAll $ integral (Range.constant 10000000000 10000000000000)
  prevPoolY       <- forAll $ integral (Range.constant 10000000000 10000000000000)
  xToSwap         <- forAll $ integral (Range.constant 1 ((prevPoolX `div` 2) - 1))
  feeNum          <- forAll $ integral (Range.constant 3000 99999)
  treasuryNum     <- forAll $ integral (Range.constant 1 3000)
  let
    (cfgData, dh) = genSConfig x y nft 9950 500000 1 pkh 10 4
    orderTxIn     = genSTxIn orderTxRef dh x 10 3813762
    orderTxOut    = genSTxOut dh y 4 1813762 pkh
  
  poolTxRef <- forAll genTxOutRef
  let
    yToSwap  = calculateY prevPoolX prevPoolY xToSwap feeNum treasuryNum 
    treasury = calculateTreasury xToSwap feeNum treasuryNum

    (pcfg, pdh)       = genPConfig x y nft lq feeNum treasuryNum [] 0 treasuryAddress
    (newPcfg, newPdh) = genPConfigWithUpdatedTreasury x y nft lq feeNum treasuryNum treasury 0 [] 0 treasuryAddress

    poolTxIn  = genPTxIn poolTxRef pdh x prevPoolX y prevPoolY lq 9223372036854775797 nft 1 0
    poolTxOut = genPTxOut newPdh x (prevPoolX + xToSwap) y (prevPoolY - yToSwap) lq 9223372036854775797 nft 1 0
  
    txInfo  = mkTxInfo poolTxIn orderTxIn poolTxOut orderTxOut
    purpose = mkPurpose poolTxRef

    cxtToData        = toData $ mkContext txInfo purpose
    poolRedeemToData = toData $ mkPoolRedeemer 0 Pool.Swap

    result = eraseRight $ evalWithArgs (wrapValidator PPool.poolValidatorT) [pcfg, poolRedeemToData, cxtToData]

  result === Right ()

incorrectPoolYTreasury :: Property
incorrectPoolYTreasury = withShrinks 1 $ withTests 10 $ property $ do
  (x, y, nft, lq) <- forAll genRandomAssetClasses
  pkh             <- forAll genPkh
  treasuryAddress <- forAll genValidatorHash      
  orderTxRef      <- forAll genTxOutRef
  prevPoolX       <- forAll $ integral (Range.constant 10000000000 10000000000000)
  prevPoolY       <- forAll $ integral (Range.constant 10000000000 10000000000000)
  xToSwap         <- forAll $ integral (Range.constant 1 ((prevPoolX `div` 2) - 1))
  feeNum          <- forAll $ integral (Range.constant 3000 9999)
  treasuryNum     <- forAll $ integral (Range.constant 1 3000)
  let
    (cfgData, dh) = genSConfig x y nft 9950 500000 1 pkh 10 4
    orderTxIn     = genSTxIn orderTxRef dh x 10 3813762
    orderTxOut    = genSTxOut dh y 4 1813762 pkh
  
  poolTxRef <- forAll genTxOutRef
  let
    yToSwap  = calculateY prevPoolX prevPoolY xToSwap feeNum treasuryNum 
    treasury = calculateTreasury xToSwap feeNum treasuryNum

    (pcfg, pdh)       = genPConfig x y nft lq feeNum treasuryNum [] 0 treasuryAddress
    (newPcfg, newPdh) = genPConfigWithUpdatedTreasury x y nft lq feeNum treasuryNum (treasury + 100) 0 [] 0 treasuryAddress

    poolTxIn  = genPTxIn poolTxRef pdh x prevPoolX y prevPoolY lq 9223372036854775797 nft 1 0
    poolTxOut = genPTxOut newPdh x (prevPoolX + xToSwap) y (prevPoolY - yToSwap) lq 9223372036854775797 nft 1 0
  
    txInfo  = mkTxInfo poolTxIn orderTxIn poolTxOut orderTxOut
    purpose = mkPurpose poolTxRef

    cxtToData        = toData $ mkContext txInfo purpose
    poolRedeemToData = toData $ mkPoolRedeemer 0 Pool.Swap

    result = eraseLeft $ evalWithArgs (wrapValidator PPool.poolValidatorT) [pcfg, poolRedeemToData, cxtToData]

  result === Left ()

incorrectPoolSwapDatum :: Property
incorrectPoolSwapDatum = withShrinks 1 $ withTests 1 $ property $ do
  (x, y, nft, lq)     <- forAll genRandomAssetClasses
  (x1, y1, nft1, lq1) <- forAll genRandomAssetClasses

  pkh             <- forAll genPkh
  treasuryAddress <- forAll genValidatorHash      
  orderTxRef      <- forAll genTxOutRef
  prevPoolX       <- forAll $ integral (Range.constant 10000 100000000)
  prevPoolY       <- forAll $ integral (Range.constant 10000 100000000)
  xToSwap         <- forAll $ integral (Range.constant 1 (prevPoolY - 1))
  let
    (cfgData, dh) = genSConfig x y nft 9950 500000 1 pkh 10 4
    orderTxIn     = genSTxIn orderTxRef dh x 10 3813762
    orderTxOut    = genSTxOut dh y 4 1813762 pkh
  
  poolTxRef <- forAll genTxOutRef
  let
    (newPoolX, newPoolY) = calculateXtoYSwap prevPoolX prevPoolY xToSwap

    yToSwap  = calculateY prevPoolX prevPoolY xToSwap
    yWithFee = calculateWithFee prevPoolX prevPoolY xToSwap

    lqFee = calculateLqFee prevPoolX prevPoolY xToSwap
    treasuryYFee = calculateLqFeeXtoYSwap prevPoolX prevPoolY xToSwap

    (pcfg, pdh)       = genPConfig x y nft lq 9950 1 [] 0 treasuryAddress
    (newPcfg, newPdh) = genPConfigWithUpdatedTreasury x1 y1 nft1 lq1 9950 1 0 treasuryYFee [] 0 treasuryAddress

    poolTxIn  = genPTxIn poolTxRef pdh x prevPoolX y prevPoolY lq 9223372036854775797 nft 1 1000000000
    poolTxOut = genPTxOut newPdh x newPoolX y newPoolY lq 9223372036854775797 nft 1 3000000
  
    txInfo  = mkTxInfo poolTxIn orderTxIn poolTxOut orderTxOut
    purpose = mkPurpose poolTxRef

    cxtToData        = toData $ mkContext txInfo purpose
    poolRedeemToData = toData $ mkPoolRedeemer 0 Pool.Swap

    result = eraseLeft $ evalWithArgs (wrapValidator PPool.poolValidatorT) [pcfg, poolRedeemToData, cxtToData]

  result === Left ()

incorrectPoolTokensQtyInFinalValue :: Property
incorrectPoolTokensQtyInFinalValue = withShrinks 1 $ withTests 100 $ property $ do
  let 
    (x, y, nft, lq) = genAssetClasses
    fakeToken       = genFakeToken
  pkh             <- forAll genPkh
  treasuryAddress <- forAll genValidatorHash      
  orderTxRef      <- forAll genTxOutRef
  let
    (cfgData, dh) = genSConfig x y nft 9950 500000 1 pkh 10 4
    orderTxIn     = genSTxIn orderTxRef dh x 10 3813762
    orderTxOut    = genSTxOut dh y 4 1813762 pkh
  
  poolTxRef <- forAll genTxOutRef
  let
    (pcfg, pdh) = genPConfig x y nft lq 9950 1 [] 0 treasuryAddress
    poolTxIn    = genPTxIn poolTxRef pdh x 10 y 10 lq 9223372036854775797 nft 1 1000000000
    poolTxOut   = genPTxOutWithIncorrectTokensQty pdh fakeToken 100 x 20 y 6 lq 9223372036854775797 nft 1 3000000
  
  let
    txInfo  = mkTxInfo poolTxIn orderTxIn poolTxOut orderTxOut
    purpose = mkPurpose poolTxRef

    cxtToData        = toData $ mkContext txInfo purpose
    poolRedeemToData = toData $ mkPoolRedeemer 0 Pool.Swap

    result = eraseLeft $ evalWithArgs (wrapValidator PPool.poolValidatorT) [pcfg, poolRedeemToData, cxtToData]

  result === Left ()

incorrectPoolSwapAdditionalTokens :: Property
incorrectPoolSwapAdditionalTokens = withTests 1 $ property $ do
  (x, y, nft, lq) <- forAll genRandomAssetClasses
  (tokenA, _, _, _) <- forAll genRandomAssetClasses
  pkh             <- forAll genPkh
  treasuryAddress <- forAll genValidatorHash      
  orderTxRef      <- forAll genTxOutRef

  let
    (cfgData, dh) = genSConfig x y nft 9950 500000 1 pkh 10 4
    orderTxIn     = genSTxIn orderTxRef dh x 10 3813762
    orderTxOut    = genSTxOut dh y 4 1813762 pkh
  
  poolTxRef <- forAll genTxOutRef
  let
    (pcfg, pdh) = genPConfig x y nft lq 9950 1 [] 0 treasuryAddress
    poolTxIn    = genPTxIn poolTxRef pdh x 10 y 10 lq 9223372036854775797 nft 1 1000000000
    poolTxOut   = genPTxOutWithAdditionalTokens pdh x 20 y 6 lq 9223372036854775797 nft 1 3000000 tokenA 10
  
  let
    txInfo  = mkTxInfo poolTxIn orderTxIn poolTxOut orderTxOut
    purpose = mkPurpose poolTxRef

    cxtToData        = toData $ mkContext txInfo purpose
    poolRedeemToData = toData $ mkPoolRedeemer 0 Pool.Swap

    result = eraseLeft $ evalWithArgs (wrapValidator PPool.poolValidatorT) [pcfg, poolRedeemToData, cxtToData]

  result === Left ()

poolSwapInsufficientLiqudityForBound :: Property
poolSwapInsufficientLiqudityForBound = withTests 1 $ property $ do
  (x, y, nft, lq) <- forAll genRandomAssetClasses
  pkh             <- forAll genPkh
  treasuryAddress <- forAll genValidatorHash      
  orderTxRef      <- forAll genTxOutRef
  let
    (cfgData, dh) = genSConfig x y nft 9950 500000 1 pkh 10 4
    orderTxIn     = genSTxIn orderTxRef dh x 10 3813762
    orderTxOut    = genSTxOut dh y 4 1813762 pkh
  
  poolTxRef <- forAll genTxOutRef
  let
    (pcfg, pdh) = genPConfig x y nft lq 9950 1 [] 21 treasuryAddress
    poolTxIn    = genPTxIn poolTxRef pdh x 10 y 10 lq 9223372036854775797 nft 1 1000000000
    poolTxOut   = genPTxOut pdh x 20 y 6 lq 9223372036854775797 nft 1 3000000
  
  let
    txInfo  = mkTxInfo poolTxIn orderTxIn poolTxOut orderTxOut
    purpose = mkPurpose poolTxRef

    cxtToData        = toData $ mkContext txInfo purpose
    poolRedeemToData = toData $ mkPoolRedeemer 0 Pool.Swap

    result = eraseLeft $ evalWithArgs (wrapValidator PPool.poolValidatorT) [pcfg, poolRedeemToData, cxtToData]

  result === Left ()

poolSwapRedeemerIncorrectIx :: Property
poolSwapRedeemerIncorrectIx = withTests 1 $ property $ do
  (x, y, nft, lq) <- forAll genRandomAssetClasses
  pkh             <- forAll genPkh
  treasuryAddress <- forAll genValidatorHash      
  orderTxRef      <- forAll genTxOutRef
  let
    (cfgData, dh) = genSConfig x y nft 100 100 100 pkh 10 1
    orderTxIn     = genSTxIn orderTxRef dh x 10 5000000
    orderTxOut    = genSTxOut dh y 10 5000000 pkh
  
  poolTxRef <- forAll genTxOutRef
  let
    (pcfg, pdh) = genPConfig x y nft lq 9950 1000 [] 0 treasuryAddress
    poolTxIn    = genPTxIn poolTxRef pdh x 100 y 10000 lq 9223372036854775797 nft 1 5000000
    poolTxOut   = genPTxOut pdh x 11000 y 9000 lq 9223372036854775787 nft 1 3000000
  
  let
    txInfo  = mkTxInfo poolTxIn orderTxIn poolTxOut orderTxOut
    purpose = mkPurpose poolTxRef

    cxtToData        = toData $ mkContext txInfo purpose
    poolRedeemToData = toData $ mkPoolRedeemer 1 Pool.Swap

    result = eraseBoth $ evalWithArgs (wrapValidator PPool.poolValidatorT) [pcfg, poolRedeemToData, cxtToData]

  result === Left ()

poolSwapRedeemerIncorrectAction :: Pool.PoolAction -> Property
poolSwapRedeemerIncorrectAction action = withTests 1 $ property $ do
  (x, y, nft, lq) <- forAll genRandomAssetClasses
  pkh             <- forAll genPkh
  treasuryAddress <- forAll genValidatorHash      
  orderTxRef      <- forAll genTxOutRef
  let
    (cfgData, dh) = genSConfig x y nft 100 100 100 pkh 10 1
    orderTxIn     = genSTxIn orderTxRef dh x 10 5000000
    orderTxOut    = genSTxOut dh y 10 5000000 pkh
  
  poolTxRef <- forAll genTxOutRef
  let
    (pcfg, pdh) = genPConfig x y nft lq 9950 1000 [] 0 treasuryAddress
    poolTxIn    = genPTxIn poolTxRef pdh x 100 y 10000 lq 9223372036854775797 nft 1 5000000
    poolTxOut   = genPTxOut pdh x 11000 y 9000 lq 9223372036854775787 nft 1 3000000
  
  let
    txInfo  = mkTxInfo poolTxIn orderTxIn poolTxOut orderTxOut
    purpose = mkPurpose poolTxRef

    cxtToData        = toData $ mkContext txInfo purpose
    poolRedeemToData = toData $ mkPoolRedeemer 0 action

    result = eraseBoth $ evalWithArgs (wrapValidator PPool.poolValidatorT) [pcfg, poolRedeemToData, cxtToData]

  result === Left ()

successPoolDeposit :: Property
successPoolDeposit = withTests 1 $ property $ do
  (x, y, nft, lq) <- forAll genRandomAssetClasses
  pkh             <- forAll genPkh
  treasuryAddress <- forAll genValidatorHash      
  orderTxRef      <- forAll genTxOutRef
  let
    (cfgData, dh) = genDConfig x y nft lq 2 pkh 10000
    orderTxIn     = genTxIn orderTxRef dh x 10 y 10 10004
    orderTxOut    = genTxOut dh lq 10 10000 pkh
  
  poolTxRef <- forAll genTxOutRef
  let
    (pcfg, pdh) = genPConfig x y nft lq 9950 1 [] 0 treasuryAddress
    poolTxIn    = genPTxIn poolTxRef pdh x 10 y 10 lq 9223372036854775797 nft 1 10000
    poolTxOut   = genPTxOut pdh x 20 y 20 lq 9223372036854775787 nft 1 10000
  
  let
    txInfo  = mkTxInfo poolTxIn orderTxIn poolTxOut orderTxOut
    purpose = mkPurpose poolTxRef

    cxtToData        = toData $ mkContext txInfo purpose
    poolRedeemToData = toData $ mkPoolRedeemer 0 Pool.Deposit

    result = eraseRight $ evalWithArgs (wrapValidator PPool.poolValidatorT) [pcfg, poolRedeemToData, cxtToData]

  result === Right ()

successPoolChangeStakePartCorrectMinting :: Property
successPoolChangeStakePartCorrectMinting = withTests 1 $ property $ do
  (x, y, nft, lq) <- forAll genRandomAssetClasses
  
  stakeAdminPkh   <- forAll genPkh
  treasuryAddress <- forAll genValidatorHash
  newPkhForSC     <- forAll genPkh
  let
    previousSc = Just $ StakingHash (PubKeyCredential stakeAdminPkh)
    newSc      = Just $ StakingHash (PubKeyCredential newPkhForSC)
    daoSC      = StakingHash $ ScriptCredential $ ValidatorHash $ getScriptHash $ scriptHash (unMintingPolicyScript (daoMintPolicyValidator [stakeAdminPkh] 1 True))

  poolTxRef <- forAll genTxOutRef
  let
    (pcfg, pdh) = genPConfig x y nft lq 9950 1 [daoSC] 0 treasuryAddress
    poolTxIn    = genPTxInWithSC poolTxRef previousSc pdh x 10 y 10 lq 9223372036854775797 nft 1 10000
    poolTxOut   = genPTxOutWithSC pdh newSc x 10 y 10 lq 9223372036854775797 nft 1 10000

    txInfo  = mkTxInfoWithSignaturesAndMinting [poolTxIn] poolTxOut [stakeAdminPkh] daoSC
    purpose = mkPurpose poolTxRef

    cxtToData        = toData $ mkContext txInfo purpose
    poolRedeemToData = toData $ mkPoolRedeemer 0 Pool.ChangeStakingPool

    result = eraseRight $ evalWithArgs (wrapValidator PPool.poolValidatorT) [pcfg, poolRedeemToData, cxtToData]

  result === Right ()

failedPoolChangeStakePartIncorrectMinting :: Property
failedPoolChangeStakePartIncorrectMinting = withTests 1 $ property $ do
  (x, y, nft, lq) <- forAll genRandomAssetClasses
  
  treasuryAddress <- forAll genValidatorHash
  stakeAdminPkh   <- forAll genPkh
  newPkhForSC     <- forAll genPkh
  let 
    previousSc = Just $ StakingHash (PubKeyCredential stakeAdminPkh)
    newSc      = Just $ StakingHash (PubKeyCredential newPkhForSC)
    daoSC      = StakingHash $ ScriptCredential $ ValidatorHash $ getScriptHash $ scriptHash (unMintingPolicyScript (daoMintPolicyValidator [stakeAdminPkh] 1 True))
    incorrectDaoSC = StakingHash $ ScriptCredential $ ValidatorHash $ getScriptHash $ scriptHash (unMintingPolicyScript (daoMintPolicyValidator [stakeAdminPkh] 4 True))
  
  poolTxRef <- forAll genTxOutRef
  let
    (pcfg, previousPdh) = genPConfig x y nft lq 9950 1 [daoSC] 0 treasuryAddress

    (_, newPdh) = genPConfig x y nft lq 9950 1 [daoSC] 0 treasuryAddress
    poolTxIn    = genPTxInWithSC poolTxRef previousSc previousPdh x 10 y 10 lq 9223372036854775797 nft 1 10000
    poolTxOut   = genPTxOutWithSC newPdh newSc x 10 y 10 lq 9223372036854775797 nft 1 10000
  
    txInfo  = mkTxInfoWithSignaturesAndMinting [poolTxIn] poolTxOut [stakeAdminPkh] incorrectDaoSC
    purpose = mkPurpose poolTxRef

    cxtToData        = toData $ mkContext txInfo purpose
    poolRedeemToData = toData $ mkPoolRedeemer 0 Pool.ChangeStakingPool

    result = eraseLeft $ evalWithArgs (wrapValidator PPool.poolValidatorT) [pcfg, poolRedeemToData, cxtToData]

  result === Left ()

poolDepositRedeemerIncorrectIx :: Property
poolDepositRedeemerIncorrectIx = withTests 1 $ property $ do
  (x, y, nft, lq) <- forAll genRandomAssetClasses
  pkh             <- forAll genPkh
  treasuryAddress <- forAll genValidatorHash      
  orderTxRef      <- forAll genTxOutRef
  let
    (cfgData, dh) = genDConfig x y nft lq 2 pkh 1
    orderTxIn     = genTxIn orderTxRef dh x 10 y 10 1000000
    orderTxOut    = genTxOut dh lq 10 (1000000 - 300) pkh
  
  poolTxRef <- forAll genTxOutRef
  let (pcfg, pdh) = genPConfig x y nft lq 9950 1 [] 0 treasuryAddress

  let
    poolTxIn    = genPTxIn poolTxRef pdh x 10 y 10 lq 9223372036854775797 nft 1 1000000
    poolTxOut   = genPTxOut pdh x 20 y 20 lq 9223372036854775787 nft 1 1000000
  
  let
    txInfo  = mkTxInfo poolTxIn orderTxIn poolTxOut orderTxOut
    purpose = mkPurpose poolTxRef

    cxtToData        = toData $ mkContext txInfo purpose
    poolRedeemToData = toData $ mkPoolRedeemer 1 Pool.Deposit

    result = eraseBoth $ evalWithArgs (wrapValidator PPool.poolValidatorT) [pcfg, poolRedeemToData, cxtToData]

  result === Left ()

poolDepositRedeemerIncorrectAction :: Pool.PoolAction -> Property
poolDepositRedeemerIncorrectAction action = withTests 1 $ property $ do
  (x, y, nft, lq) <- forAll genRandomAssetClasses
  pkh             <- forAll genPkh
  treasuryAddress <- forAll genValidatorHash      
  orderTxRef      <- forAll genTxOutRef
  let
    (cfgData, dh) = genDConfig x y nft lq 2 pkh 1
    orderTxIn     = genTxIn orderTxRef dh x 10 y 10 1000000
    orderTxOut    = genTxOut dh lq 10 (1000000 - 300) pkh
  
  poolTxRef <- forAll genTxOutRef
  let (pcfg, pdh) = genPConfig x y nft lq 9950 1 [] 0 treasuryAddress

  let
    poolTxIn    = genPTxIn poolTxRef pdh x 10 y 10 lq 9223372036854775797 nft 1 1000000
    poolTxOut   = genPTxOut pdh x 20 y 20 lq 9223372036854775787 nft 1 1000000
  
  let
    txInfo  = mkTxInfo poolTxIn orderTxIn poolTxOut orderTxOut
    purpose = mkPurpose poolTxRef

    cxtToData        = toData $ mkContext txInfo purpose
    poolRedeemToData = toData $ mkPoolRedeemer 0 action

    result = eraseBoth $ evalWithArgs (wrapValidator PPool.poolValidatorT) [pcfg, poolRedeemToData, cxtToData]

  result === Left ()
