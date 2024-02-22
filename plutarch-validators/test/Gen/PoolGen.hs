module Gen.PoolGen where

import Hedgehog

import Gen.Models
import Gen.DepositGen

import PlutusLedgerApi.V2
import PlutusLedgerApi.V1.Value
import Hedgehog.Range as Range

import qualified WhalePoolsDex.Contracts.Pool as P

genPConfig :: AssetClass -> AssetClass -> AssetClass -> AssetClass -> Integer -> Integer -> [CurrencySymbol] -> Integer -> ValidatorHash -> (Data, OutputDatum)
genPConfig x y nft lq fee treasuryFeeNum daoPolicy lqBound treasuryAddress =
  let
    -- for tests treasury fee will be 50% of lp fee
    config    = mkPoolConfig nft x y lq fee treasuryFeeNum 0 0 daoPolicy lqBound treasuryAddress
    od        = OutputDatum $ mkDatum config
  in (toData config, od)

genPBConfig :: AssetClass -> AssetClass -> AssetClass -> AssetClass -> Integer -> [CurrencySymbol] -> Integer -> ValidatorHash -> (Data, OutputDatum)
genPBConfig x y nft lq fee daoPolicy lqBound treasuryAddress =
  let
    -- for tests treasury fee will be 50% of lp fee
    config    = mkPoolBFeeConfig nft x y lq fee fee 5000 0 0 daoPolicy lqBound treasuryAddress
    od        = OutputDatum $ mkDatum config
  in (toData config, od)

-- calculate correct value of final x/y in pool, based on input pool x/y and x from user swap
calculateXtoYSwap :: Integer -> Integer -> Integer -> (Integer, Integer)
calculateXtoYSwap prevXPool prevYPool xToSwap =
  let
    yFromSwapWithoutFee = prevYPool - (prevXPool * prevYPool) `div` (prevXPool + xToSwap)
  in ((prevXPool + xToSwap), prevYPool - (yFromSwapWithoutFee * 99500 `div` 100000) + 1)

calculateY :: Integer -> Integer -> Integer -> Integer -> Integer -> Integer
calculateY prevXPool prevYPool xToSwap feeNum treasuryNum =
  ((prevYPool * xToSwap * (feeNum - treasuryNum)) `div` (prevXPool * 100000 + xToSwap * feeNum))

calculateTreasury :: Integer -> Integer -> Integer -> Integer
calculateTreasury token2swap feeNum treasuryFeeNum =
  token2swap * treasuryFeeNum `div` 100000

calculateWithFee :: Integer -> Integer -> Integer -> Integer
calculateWithFee prevXPool prevYPool xToSwap =
  let
    yFromSwapWithoutFee = prevYPool - (prevXPool * prevYPool) `div` (prevXPool + xToSwap)
  in yFromSwapWithoutFee * 99970 `div` 100000

calculateLqFee :: Integer -> Integer -> Integer -> Integer
calculateLqFee prevXPool prevYPool xToSwap =
  let
    yFromSwapWithoutFee = prevYPool - (prevXPool * prevYPool) `div` (prevXPool + xToSwap)
  in (yFromSwapWithoutFee * 30 `div` 100000)

calculateLqFeeXtoYSwap :: Integer -> Integer -> Integer -> Integer
calculateLqFeeXtoYSwap prevXPool prevYPool xToSwap =
  let
    yFromSwapWithoutFee = prevYPool - (prevXPool * prevYPool) `div` (prevXPool + xToSwap)
  in (yFromSwapWithoutFee * 30 `div` 100000) `div` 100000

genPConfigWithUpdatedTreasury :: AssetClass -> AssetClass -> AssetClass -> AssetClass -> Integer -> Integer -> Integer -> Integer -> [CurrencySymbol] -> Integer -> ValidatorHash -> (Data, OutputDatum)
genPConfigWithUpdatedTreasury x y nft lq fee treasuryFeeNum newTreasuryX newTreasuryY daoPolicy lqBound treasuryAddress =
  let
    -- for tests treasury fee will be 50% of lp fee
    config    = mkPoolConfig nft x y lq fee treasuryFeeNum newTreasuryX newTreasuryY daoPolicy lqBound treasuryAddress
    od        = OutputDatum $ mkDatum config
  in (toData config, od)

genPBConfigWithUpdatedTreasury :: AssetClass -> AssetClass -> AssetClass -> AssetClass -> Integer -> Integer -> Integer -> [CurrencySymbol] -> Integer -> ValidatorHash -> (Data, OutputDatum)
genPBConfigWithUpdatedTreasury x y nft lq fee newTreasuryX newTreasuryY daoPolicy lqBound treasuryAddress =
  let
    -- for tests treasury fee will be 50% of lp fee
    config    = mkPoolBFeeConfig nft x y lq fee fee 5000 newTreasuryX newTreasuryY daoPolicy lqBound treasuryAddress
    od        = OutputDatum $ mkDatum config
  in (toData config, od)

genPTxIn :: TxOutRef -> OutputDatum -> AssetClass -> Integer -> AssetClass -> Integer -> AssetClass -> Integer -> AssetClass -> Integer -> Integer -> TxInInfo
genPTxIn ref od x xQty y yQty lq lqQty nft nftQty adaQty =
  let
    value = mkValues [mkValue x xQty, mkValue y yQty, mkValue lq lqQty, mkValue nft nftQty, mkAdaValue adaQty] mempty
    txOut = mkTxOut od value mkPoolValidator 
  in mkTxIn ref txOut

genPTxInWithSC :: TxOutRef -> Maybe StakingCredential -> OutputDatum -> AssetClass -> Integer -> AssetClass -> Integer -> AssetClass -> Integer -> AssetClass -> Integer -> Integer -> TxInInfo
genPTxInWithSC ref sc od x xQty y yQty lq lqQty nft nftQty adaQty =
  let
    value = mkValues [mkValue x xQty, mkValue y yQty, mkValue lq lqQty, mkValue nft nftQty, mkAdaValue adaQty] mempty
    txOut = mkTxOutWithSC od value mkPoolValidator sc
  in mkTxIn ref txOut

genPTxOut :: OutputDatum -> AssetClass -> Integer -> AssetClass -> Integer -> AssetClass -> Integer -> AssetClass -> Integer -> Integer -> TxOut
genPTxOut od x xQty y yQty lq lqQty nft nftQty adaQty =
  let
    value = mkValues [mkValue x xQty, mkValue y yQty, mkValue lq lqQty, mkValue nft nftQty, mkAdaValue adaQty] mempty
  in mkTxOut od value mkPoolValidator

genPTxOutWithIncorrectTokensQty :: OutputDatum -> AssetClass -> Integer -> AssetClass -> Integer -> AssetClass -> Integer -> AssetClass -> Integer -> AssetClass -> Integer -> Integer -> TxOut
genPTxOutWithIncorrectTokensQty od f fQty x xQty y yQty lq lqQty nft nftQty adaQty =
  let
    value = mkValues [mkValue f fQty, mkValue x xQty, mkValue y yQty, mkValue lq lqQty, mkValue nft nftQty, mkAdaValue adaQty] mempty
  in mkTxOut od value mkPoolValidator

genPTxOutWithAdditionalTokens :: OutputDatum -> AssetClass -> Integer -> AssetClass -> Integer -> AssetClass -> Integer -> AssetClass -> Integer -> Integer -> AssetClass -> Integer -> TxOut
genPTxOutWithAdditionalTokens od x xQty y yQty lq lqQty nft nftQty adaQty additionalToken additionalTokenQty =
  let
    value = mkValues [mkValue x xQty, mkValue y yQty, mkValue lq lqQty, mkValue nft nftQty, mkValue additionalToken additionalTokenQty, mkAdaValue adaQty] mempty
  in mkTxOut od value mkPoolValidator

genPTxOutWithSC :: OutputDatum -> Maybe StakingCredential -> AssetClass -> Integer -> AssetClass -> Integer -> AssetClass -> Integer -> AssetClass -> Integer -> Integer -> TxOut
genPTxOutWithSC od sc x xQty y yQty lq lqQty nft nftQty adaQty =
  let
    value = mkValues [mkValue x xQty, mkValue y yQty, mkValue lq lqQty, mkValue nft nftQty, mkAdaValue adaQty] mempty
  in mkTxOutWithSC od value mkPoolValidator sc