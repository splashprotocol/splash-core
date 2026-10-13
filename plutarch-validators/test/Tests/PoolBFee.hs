{-# LANGUAGE OverloadedStrings #-}

module Tests.PoolBFee where

import qualified WhalePoolsDex.PContracts.PPoolBFee as PPool
import qualified WhalePoolsDex.Contracts.PoolBFee   as Pool
import qualified WhalePoolsDex.Contracts.Pool       as Pool hiding (PoolConfig(..))
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

checkPoolBFee = testGroup "CheckPoolBFeeContract"
  [ HH.testProperty "pool_change_stake_part_is_correct (correct minting)" successPoolChangeStakePartCorrectMinting
  , HH.testProperty "pool_change_stake_part_is_incorrect (incorrect minting)" failedPoolChangeStakePartIncorrectMinting
  , HH.testProperty "pool_deposit_is_correct" successPoolDeposit
  , HH.testProperty "pool_swap_is_correct" successPoolSwap
  , HH.testProperty "pool_swap_incorrect_treasury" incorrectPoolYTreasury
  , HH.testProperty "pool_swap_incorrect_datum" incorrectPoolSwapDatum
  , HH.testProperty "incorrect_final_pool_tokens_qty" incorrectPoolTokensQtyInFinalValue
  , HH.testProperty "pool_swap_insufficient_lq_for_bound" poolSwapInsufficientLiqudityForBound
  , HH.testProperty "pool_redeem_is_correct" successPoolRedeem
  , HH.testProperty "pool_swap_additional_tokens" incorrectPoolSwapAdditionalTokens
  , HH.testProperty "pool_redeem_too_much_liquidity_removed" (poolRedeemLqCheck 999 1000 (Pool.maxLqCap - Pool.burnLqInitial))
  , HH.testProperty "pool_redeem_liquidity_removed_lq_intact" (poolRedeemLqCheck 1000 1000 (Pool.maxLqCap - Pool.burnLqInitial - 10))
  , HH.testProperty "pool_redeem_wrong_lq_direction" (poolDepositRedeemerIncorrectAction Pool.Redeem)
  , HH.testProperty "pool_destroy_lq_burnLqInitial" (poolDestroyCheck (Pool.maxLqCap - Pool.burnLqInitial) (Left ()))
  , HH.testProperty "pool_destroy_lq_burnLqInitial-1" (poolDestroyCheck (Pool.maxLqCap - Pool.burnLqInitial + 1) (Left ()))
  , HH.testProperty "pool_destroy_lq_burnLqInitial+1" (poolDestroyCheck (Pool.maxLqCap - Pool.burnLqInitial - 1) (Left ()))
  , HH.testProperty "pool_destroy_incorrect_pool_input" (poolDestroyCheckIncorrectPoolInput (Pool.maxLqCap - Pool.burnLqInitial))
  ]

checkPoolBFeeRedeemer = testGroup "CheckPoolBFeeRedeemer"
  [ HH.testProperty "fail_if_pool_ix_is_incorrect_deposit" poolDepositRedeemerIncorrectIx
  , HH.testProperty "fail_if_pool_action_is_incorrect_deposit_to_swap" (poolDepositRedeemerIncorrectAction Pool.Swap)
  , HH.testProperty "fail_if_pool_ix_is_incorrect_swap" poolSwapRedeemerIncorrectIx
  , HH.testProperty "fail_if_pool_action_is_incorrect_swap_to_deposit" (poolSwapRedeemerIncorrectAction Pool.Deposit)
  , HH.testProperty "fail_if_pool_action_is_incorrect_swap_to_redeem" (poolSwapRedeemerIncorrectAction Pool.Redeem)
  , HH.testProperty "fail_if_pool_ix_is_incorrect_redeem" poolRedeemRedeemerIncorrectIx
  , HH.testProperty "fail_if_pool_action_is_incorrect_redeem_to_swap" (poolRedeemRedeemerIncorrectAction Pool.Swap)
  ]

poolDestroyCheck :: Integer -> Either () () -> Property
poolDestroyCheck lqQty expected = withTests 1 $ property $ do
  (x, y, nft, lq) <- forAll genRandomAssetClasses

  treasuryAddress <- forAll genValidatorHash
  poolTxRef <- forAll genTxOutRef
  let
    (pcfg, pdh) = genPBConfig x y nft lq 1 [] 0 treasuryAddress
    poolTxIn    = genDTxIn poolTxRef pdh lq lqQty nft 1 5000000
  
    txInfo  = mkDTxInfo [poolTxIn]
    purpose = mkPurpose poolTxRef

    cxtToData        = toData $ mkContext txInfo purpose
    poolRedeemToData = toData $ mkPoolRedeemer 0 Pool.Destroy

    result = eraseBoth $ evalWithArgs (wrapValidator PPool.poolBFeeValidatorT) [pcfg, poolRedeemToData, cxtToData]

  result === expected

poolDestroyCheckIncorrectPoolInput :: Integer -> Property
poolDestroyCheckIncorrectPoolInput lqQty = withTests 1 $ property $ do
  (x, y, nft, lq) <- forAll genRandomAssetClasses

  treasuryAddress <- forAll genValidatorHash
  poolTxRef      <- forAll genTxOutRef
  incorrectTxRef <- forAll genTxOutRef
  let
    (pcfg, pdh) = genPBConfig x y nft lq 1 [] 0 treasuryAddress
    poolTxIn    = genPTxIn poolTxRef pdh x 20 y 20 lq 9223372036854775787 nft 1 5000000
    incorrectPoolTxIn = genDTxIn incorrectTxRef pdh lq lqQty genFakeToken 1 5000000

    txInfo  = mkDTxInfo [incorrectPoolTxIn, poolTxIn]
    purpose = mkPurpose poolTxRef

    cxtToData        = toData $ mkContext txInfo purpose
    poolRedeemToData = toData $ mkPoolRedeemer 0 Pool.Destroy

    result = eraseLeft $ evalWithArgs (wrapValidator PPool.poolBFeeValidatorT) [pcfg, poolRedeemToData, cxtToData]

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
    (pcfg, pdh) = genPBConfig x y nft lq 1 [] 0 treasuryAddress
    poolTxIn    = genPTxIn poolTxRef pdh x 1010 y 1010 lq (Pool.maxLqCap - Pool.burnLqInitial - 10) nft 1 5000000
    poolTxOut   = genPTxOut pdh x 1000 y 1000 lq (Pool.maxLqCap - Pool.burnLqInitial) nft 1 5000000
  
  let
    txInfo  = mkTxInfo poolTxIn orderTxIn poolTxOut orderTxOut
    purpose = mkPurpose poolTxRef

    cxtToData        = toData $ mkContext txInfo purpose
    poolRedeemToData = toData $ mkPoolRedeemer 0 Pool.Redeem

    result = eraseRight $ evalWithArgs (wrapValidator PPool.poolBFeeValidatorT) [pcfg, poolRedeemToData, cxtToData]

  result === Right ()

poolRedeemLqCheck :: Integer -> Integer -> Integer -> Property
poolRedeemLqCheck xQty yQty lqOutQty = withTests 1 $ property $ do
  (x, y, nft, lq) <- forAll genRandomAssetClasses
  treasuryAddress <- forAll genValidatorHash
  poolTxRef <- forAll genTxOutRef
  let
    (pcfg, pdh) = genPBConfig x y nft lq 9950 [] 0 treasuryAddress
    poolTxIn = genPTxIn poolTxRef pdh x 1010 y 1010
      lq (Pool.maxLqCap - Pool.burnLqInitial - 10) nft 1 3000000
    validPoolTxOut = genPTxOut pdh x 1000 y 1000
      lq (Pool.maxLqCap - Pool.burnLqInitial) nft 1 3000000
    invalidPoolTxOut = genPTxOut pdh x xQty y yQty lq lqOutQty nft 1 3000000
    evaluate output = eraseBoth $ evalWithArgs (wrapValidator PPool.poolBFeeValidatorT)
      [ pcfg, toData $ mkPoolRedeemer 0 Pool.Redeem
      , toData $ mkContext (mkPoolTxInfo poolTxIn output) (mkPurpose poolTxRef)
      ]
  evaluate validPoolTxOut === Right ()
  evaluate invalidPoolTxOut === Left ()

poolRedeemRedeemerIncorrectIx :: Property
poolRedeemRedeemerIncorrectIx = withTests 1 $ property $ do
  (x, y, nft, lq) <- forAll genRandomAssetClasses
  treasuryAddress <- forAll genValidatorHash
  poolTxRef <- forAll genTxOutRef
  let
    (pcfg, pdh) = genPBConfig x y nft lq 9950 [] 0 treasuryAddress
    poolTxIn = genPTxIn poolTxRef pdh x 1010 y 1010 lq (Pool.maxLqCap - Pool.burnLqInitial - 10) nft 1 3000000
    poolTxOut = genPTxOut pdh x 1000 y 1000 lq (Pool.maxLqCap - Pool.burnLqInitial) nft 1 3000000
    -- Deliberately duplicate the pool Value in a validator-local context:
    -- the wrong index must fail selfIdentity, not NFT/backing/datum parsing.
    otherRef = poolTxRef { txOutRefIdx = txOutRefIdx poolTxRef + 1 }
    otherPoolTxIn = poolTxIn { txInInfoOutRef = otherRef }
    txInfo = (mkPoolTxInfo poolTxIn poolTxOut) { txInfoInputs = [poolTxIn, otherPoolTxIn] }
    context = toData $ mkContext txInfo (mkPurpose poolTxRef)
    evaluate index = eraseBoth $ evalWithArgs (wrapValidator PPool.poolBFeeValidatorT)
      [pcfg, toData $ mkPoolRedeemer index Pool.Redeem, context]
  evaluate 0 === Right ()
  evaluate 1 === Left ()

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
    (pcfg, pdh) = genPBConfig x y nft lq 1 [] 0 treasuryAddress
    poolTxIn    = genPTxIn poolTxRef pdh x 1010 y 1010 lq (Pool.maxLqCap - Pool.burnLqInitial - 10) nft 1 5000000
    poolTxOut   = genPTxOut pdh x 1000 y 1000 lq (Pool.maxLqCap - Pool.burnLqInitial) nft 1 5000000
  
  let
    txInfo  = mkTxInfo poolTxIn orderTxIn poolTxOut orderTxOut
    purpose = mkPurpose poolTxRef

    cxtToData        = toData $ mkContext txInfo purpose
    poolRedeemToData = toData $ mkPoolRedeemer 0 action

    control = eraseBoth $ evalWithArgs (wrapValidator PPool.poolBFeeValidatorT)
      [pcfg, toData $ mkPoolRedeemer 0 Pool.Redeem, cxtToData]

    result = eraseBoth $ evalWithArgs (wrapValidator PPool.poolBFeeValidatorT) [pcfg, poolRedeemToData, cxtToData]

  control === Right ()
  result === Left ()

successPoolSwap :: Property
successPoolSwap = withShrinks 1 $ withTests 1 $ property $ do
  (x, y, nft, lq) <- forAll genRandomAssetClasses
  pkh             <- forAll genPkh
  treasuryAddress <- forAll genValidatorHash      
  orderTxRef      <- forAll genTxOutRef
  prevPoolX       <- forAll $ integral (Range.constant 10000 100000000)
  prevPoolY       <- forAll $ integral (Range.constant 10000 100000000)
  xToSwap         <- forAll $ integral (Range.constant (prevPoolX `div` 100) (prevPoolX `div` 2))
  let
    (cfgData, dh) = genSConfig x y nft 9950 500000 1 pkh 10 4
    orderTxIn     = genSTxIn orderTxRef dh x 10 3813762
    orderTxOut    = genSTxOut dh y 4 1813762 pkh
  
  poolTxRef <- forAll genTxOutRef
  let
    feeNum = 9950
    treasuryFee = 5000
    adjustedIn = xToSwap * feeNum
    reserveYOut = (prevPoolY * adjustedIn) `div` (prevPoolX * 100000 + adjustedIn)
    treasuryYFee = (reserveYOut * treasuryFee * (100000 - feeNum)) `div` (feeNum * 100000)
    -- The validator calculates treasury from the Y that actually leaves the UTxO.
    poolYOut = reserveYOut

    (pcfg, pdh)       = genPBConfig x y nft lq 9950 [] 0 treasuryAddress
    (newPcfg, newPdh) = genPBConfigWithUpdatedTreasury x y nft lq 9950 0 treasuryYFee [] 0 treasuryAddress

    poolTxIn  = genPTxIn poolTxRef pdh x prevPoolX y prevPoolY lq (Pool.maxLqCap - Pool.burnLqInitial) nft 1 3000000
    poolTxOut = genPTxOut newPdh x (prevPoolX + xToSwap) y (prevPoolY - poolYOut) lq (Pool.maxLqCap - Pool.burnLqInitial) nft 1 3000000
  
    txInfo  = mkTxInfo poolTxIn orderTxIn poolTxOut orderTxOut
    purpose = mkPurpose poolTxRef

    cxtToData        = toData $ mkContext txInfo purpose
    poolRedeemToData = toData $ mkPoolRedeemer 0 Pool.Swap

    result = eraseRight $ evalWithArgs (wrapValidator PPool.poolBFeeValidatorT) [pcfg, poolRedeemToData, cxtToData]

  result === Right ()

-- Every negative swap case first executes an otherwise identical valid
-- transition. This prevents backing, ADA or fee errors from masking the
-- predicate the test is intended to exercise.
data SwapMutation
  = IncorrectSwapTreasury
  | IncorrectSwapDatum
  | DecreasedForeignToken
  | AddedForeignToken
  | InsufficientSwapBound
  | IncorrectSwapIndex
  | IncorrectSwapAction Pool.PoolAction

checkSwapMutation :: SwapMutation -> Property
checkSwapMutation mutation = withTests 1 $ property $ do
  (x, y, nft, lq) <- forAll genRandomAssetClasses
  treasuryAddress <- forAll genValidatorHash
  poolTxRef <- forAll genTxOutRef
  pkh <- forAll genPkh
  -- Nonzero treasury accrual, healthy reserves and LP above the locked floor.
  -- Small counter mutations therefore cannot accidentally fail backing.
  let
    prevPoolX = 1000000
    prevPoolY = 1000000
    xToSwap = 100000
    feeNum = 99500
    treasuryNum = 5000
    before = mkPoolBFeeConfig nft x y lq feeNum feeNum treasuryNum 0 0 [] 0 treasuryAddress
    adjustedIn = xToSwap * feeNum
    yToSwap = (prevPoolY * adjustedIn) `div` (prevPoolX * 100000 + adjustedIn)
    treasury = (yToSwap * treasuryNum * (100000 - feeNum)) `div` (feeNum * 100000)
    after = before { Pool.treasuryY = treasury }
    foreignBefore = case mutation of
      DecreasedForeignToken -> 10
      _ -> 0
    poolValue xQty yQty = mkValues
      [ mkValue x xQty, mkValue y yQty, mkValue nft 1
      , mkValue lq (Pool.maxLqCap - 2 * Pool.burnLqInitial)
      , mkValue genFakeToken foreignBefore
      ] (mkAdaValue 3000000)
    input = genPTxIn poolTxRef (OutputDatum $ mkDatum before)
      x prevPoolX y prevPoolY lq (Pool.maxLqCap - 2 * Pool.burnLqInitial) nft 1 3000000
    poolInput = input { txInInfoResolved = (txInInfoResolved input)
      { txOutValue = poolValue prevPoolX prevPoolY } }
    successor = genPTxOut (OutputDatum $ mkDatum after)
      x (prevPoolX + xToSwap) y (prevPoolY - yToSwap)
      lq (Pool.maxLqCap - 2 * Pool.burnLqInitial) nft 1 3000000
    poolOutput = successor { txOutValue = poolValue (prevPoolX + xToSwap) (prevPoolY - yToSwap) }
    otherRef = poolTxRef { txOutRefIdx = txOutRefIdx poolTxRef + 1 }
    otherOutput = mkTxOut' NoOutputDatum (mkAdaValue 3000000) pkh
    -- Validator-local context: the index case uses an identical second pool
    -- input with a different outRef, so only selfIdentity rejects it. The
    -- duplicated NFT is intentional here; this is not a ledger transaction.
    otherInput = case mutation of
      IncorrectSwapIndex -> poolInput { txInInfoOutRef = otherRef }
      _ -> mkTxIn otherRef otherOutput
    evaluate conf poolIn poolOut redeemer =
      eraseBoth $ evalWithArgs (wrapValidator PPool.poolBFeeValidatorT)
        [ toData conf, toData redeemer
        , toData $ mkContext (mkTxInfo poolIn otherInput poolOut otherOutput) (mkPurpose poolTxRef)
        ]
    validRedeemer = mkPoolRedeemer 0 Pool.Swap
    control = evaluate before poolInput poolOutput validRedeemer
    result = case mutation of
      IncorrectSwapTreasury -> evaluate before poolInput
        (poolOutput { txOutDatum = OutputDatum $ mkDatum (after { Pool.treasuryY = treasury + 1 }) }) validRedeemer
      IncorrectSwapDatum -> evaluate before poolInput
        (poolOutput { txOutDatum = OutputDatum $ mkDatum (after { Pool.lqBound = 1 }) }) validRedeemer
      DecreasedForeignToken -> evaluate before poolInput
        (poolOutput { txOutValue = txOutValue poolOutput <> mkValue genFakeToken (-1) }) validRedeemer
      AddedForeignToken -> evaluate before poolInput
        (poolOutput { txOutValue = txOutValue poolOutput <> mkValue genFakeToken 10 }) validRedeemer
      InsufficientSwapBound ->
        let bound = 2 * prevPoolX + 1
            restrictedBefore = before { Pool.lqBound = bound }
            restrictedAfter = after { Pool.lqBound = bound }
            restrictedInput = poolInput { txInInfoResolved = (txInInfoResolved poolInput)
              { txOutDatum = OutputDatum $ mkDatum restrictedBefore } }
            restrictedOutput = poolOutput { txOutDatum = OutputDatum $ mkDatum restrictedAfter }
        in evaluate restrictedBefore restrictedInput restrictedOutput validRedeemer
      IncorrectSwapIndex -> evaluate before poolInput poolOutput (mkPoolRedeemer 1 Pool.Swap)
      IncorrectSwapAction action -> evaluate before poolInput poolOutput (mkPoolRedeemer 0 action)
  control === Right ()
  result === Left ()

incorrectPoolYTreasury :: Property
incorrectPoolYTreasury = checkSwapMutation IncorrectSwapTreasury

incorrectPoolSwapDatum :: Property
incorrectPoolSwapDatum = checkSwapMutation IncorrectSwapDatum

incorrectPoolTokensQtyInFinalValue :: Property
incorrectPoolTokensQtyInFinalValue = checkSwapMutation DecreasedForeignToken

incorrectPoolSwapAdditionalTokens :: Property
incorrectPoolSwapAdditionalTokens = checkSwapMutation AddedForeignToken

poolSwapInsufficientLiqudityForBound :: Property
poolSwapInsufficientLiqudityForBound = checkSwapMutation InsufficientSwapBound

poolSwapRedeemerIncorrectIx :: Property
poolSwapRedeemerIncorrectIx = checkSwapMutation IncorrectSwapIndex

poolSwapRedeemerIncorrectAction :: Pool.PoolAction -> Property
poolSwapRedeemerIncorrectAction action = checkSwapMutation (IncorrectSwapAction action)

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
    (pcfg, pdh) = genPBConfig x y nft lq 1 [] 0 treasuryAddress
    poolTxIn    = genPTxIn poolTxRef pdh x 1000 y 1000 lq (Pool.maxLqCap - Pool.burnLqInitial) nft 1 10000
    poolTxOut   = genPTxOut pdh x 1010 y 1010 lq (Pool.maxLqCap - Pool.burnLqInitial - 10) nft 1 10000
  
  let
    txInfo  = mkTxInfo poolTxIn orderTxIn poolTxOut orderTxOut
    purpose = mkPurpose poolTxRef

    cxtToData        = toData $ mkContext txInfo purpose
    poolRedeemToData = toData $ mkPoolRedeemer 0 Pool.Deposit

    result = eraseRight $ evalWithArgs (wrapValidator PPool.poolBFeeValidatorT) [pcfg, poolRedeemToData, cxtToData]

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
    sc  = StakingHash $ ScriptCredential $ ValidatorHash $ getScriptHash $ scriptHash (unMintingPolicyScript (daoMintPolicyValidator [stakeAdminPkh] 1 True))

  poolTxRef <- forAll genTxOutRef
  let
    (pcfg, pdh) = genPBConfig x y nft lq 1 [sc] 0 treasuryAddress
    poolTxIn    = genPTxInWithSC poolTxRef previousSc pdh x 1000 y 1000 lq (Pool.maxLqCap - Pool.burnLqInitial) nft 1 10000
    poolTxOut   = genPTxOutWithSC pdh newSc x 1000 y 1000 lq (Pool.maxLqCap - Pool.burnLqInitial) nft 1 10000

    txInfo  = mkTxInfoWithSignaturesAndMinting [poolTxIn] poolTxOut [stakeAdminPkh] sc
    purpose = mkPurpose poolTxRef

    cxtToData        = toData $ mkContext txInfo purpose
    poolRedeemToData = toData $ mkPoolRedeemer 0 Pool.ChangeStakingPool

    result = eraseRight $ evalWithArgs (wrapValidator PPool.poolBFeeValidatorT) [pcfg, poolRedeemToData, cxtToData]

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

    correctDAO    = StakingHash $ ScriptCredential $ ValidatorHash $ getScriptHash $ scriptHash (unMintingPolicyScript (daoMintPolicyValidator [stakeAdminPkh] 1 True))
    incorrectDAO  = StakingHash $ ScriptCredential $ ValidatorHash $ getScriptHash $ scriptHash (unMintingPolicyScript (daoMintPolicyValidator [stakeAdminPkh] 4 True))
  
  poolTxRef <- forAll genTxOutRef
  let
    (pcfg, previousPdh) = genPBConfig x y nft lq 1 [correctDAO] 0 treasuryAddress

    (_, newPdh) = genPBConfig x y nft lq 1 [correctDAO] 0 treasuryAddress
    poolTxIn    = genPTxInWithSC poolTxRef previousSc previousPdh x 1000 y 1000 lq (Pool.maxLqCap - Pool.burnLqInitial) nft 1 10000
    poolTxOut   = genPTxOutWithSC newPdh newSc x 1000 y 1000 lq (Pool.maxLqCap - Pool.burnLqInitial) nft 1 10000
  
    txInfo  = mkTxInfoWithSignaturesAndMinting [poolTxIn] poolTxOut [stakeAdminPkh] incorrectDAO
    purpose = mkPurpose poolTxRef

    cxtToData        = toData $ mkContext txInfo purpose
    poolRedeemToData = toData $ mkPoolRedeemer 0 Pool.ChangeStakingPool

    result = eraseLeft $ evalWithArgs (wrapValidator PPool.poolBFeeValidatorT) [pcfg, poolRedeemToData, cxtToData]

  result === Left ()

poolDepositRedeemerIncorrectIx :: Property
poolDepositRedeemerIncorrectIx = withTests 1 $ property $ do
  (x, y, nft, lq) <- forAll genRandomAssetClasses
  treasuryAddress <- forAll genValidatorHash
  poolTxRef <- forAll genTxOutRef
  let
    (pcfg, pdh) = genPBConfig x y nft lq 9950 [] 0 treasuryAddress
    poolTxIn = genPTxIn poolTxRef pdh x 1000 y 1000 lq (Pool.maxLqCap - Pool.burnLqInitial) nft 1 3000000
    poolTxOut = genPTxOut pdh x 1010 y 1010 lq (Pool.maxLqCap - Pool.burnLqInitial - 10) nft 1 3000000
    -- Deliberately duplicate the pool Value in a validator-local context:
    -- the wrong index must fail selfIdentity, not NFT/backing/datum parsing.
    otherRef = poolTxRef { txOutRefIdx = txOutRefIdx poolTxRef + 1 }
    otherPoolTxIn = poolTxIn { txInInfoOutRef = otherRef }
    txInfo = (mkPoolTxInfo poolTxIn poolTxOut) { txInfoInputs = [poolTxIn, otherPoolTxIn] }
    context = toData $ mkContext txInfo (mkPurpose poolTxRef)
    evaluate index = eraseBoth $ evalWithArgs (wrapValidator PPool.poolBFeeValidatorT)
      [pcfg, toData $ mkPoolRedeemer index Pool.Deposit, context]
  evaluate 0 === Right ()
  evaluate 1 === Left ()

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
  let (pcfg, pdh) = genPBConfig x y nft lq 1 [] 0 treasuryAddress

  let
    poolTxIn    = genPTxIn poolTxRef pdh x 1000 y 1000 lq (Pool.maxLqCap - Pool.burnLqInitial) nft 1 1000000
    poolTxOut   = genPTxOut pdh x 1010 y 1010 lq (Pool.maxLqCap - Pool.burnLqInitial - 10) nft 1 1000000
  
  let
    txInfo  = mkTxInfo poolTxIn orderTxIn poolTxOut orderTxOut
    purpose = mkPurpose poolTxRef

    cxtToData        = toData $ mkContext txInfo purpose
    poolRedeemToData = toData $ mkPoolRedeemer 0 action

    control = eraseBoth $ evalWithArgs (wrapValidator PPool.poolBFeeValidatorT)
      [pcfg, toData $ mkPoolRedeemer 0 Pool.Deposit, cxtToData]

    result = eraseBoth $ evalWithArgs (wrapValidator PPool.poolBFeeValidatorT) [pcfg, poolRedeemToData, cxtToData]

  control === Right ()
  result === Left ()
