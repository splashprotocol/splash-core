{-# LANGUAGE OverloadedStrings #-}

module Tests.PoolSafety (poolSafety) where

import Eval (evalWithArgs)
import Gen.DepositGen (genAssetClasses, genFakeToken, mkByteString)
import Gen.Models
import Gen.Utils (eraseBoth)
import Hedgehog (Property, forAll, property, withTests, (===))
import Plutarch.Api.V2 (validatorHash)
import PlutusLedgerApi.V2
import qualified PlutusTx.AssocMap as AssocMap
import PlutusTx.Builtins.Internal (BuiltinByteString (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)
import WhalePoolsDex.PValidators (wrapValidator, poolBFeeValidator, royaltyPoolValidator, doubleRoyaltyPoolValidator)
import qualified WhalePoolsDex.Contracts.Pool as Pool
import qualified WhalePoolsDex.Contracts.RoyaltyPool as Royalty
import qualified WhalePoolsDex.Contracts.DoubleRoyaltyPool as DoubleRoyalty
import qualified WhalePoolsDex.PContracts.PPool as PPool
import qualified WhalePoolsDex.PContracts.PPoolBFee as PPoolBFee
import qualified WhalePoolsDex.PContracts.PRoyaltyPool as PRoyaltyPool
import qualified WhalePoolsDex.PContracts.PDoubleRoyaltyPool as PDoubleRoyaltyPool

data Family = Ordinary | BFee | Royalty | DoubleRoyalty

data Scenario
  = FairSwap
  | NoTokenPayout
  | AdaAsXFairSwap
  | AdaAsXNoTokenPayout
  | AdaAsYFairSwap
  | AdaAsYNoTokenPayout
  | NegativeInputReserve
  | ZeroInputReserve
  | OneUnitReserveFairSwap
  | OneUnitOutputReserveFairSwap
  | ZeroOutputReserve
  | LostForeignToken
  | AddedForeignToken
  | LostAdaFromTokenPool
  | ExtraInputNft
  | ZeroEffectiveFee
  | ExcessiveSwapFee
  | WrongDepositDirection
  | WrongRedeemDirection
  | ValidDeposit
  | ValidRedeem
  deriving (Eq, Show)

poolSafety :: TestTree
poolSafety = testGroup "protected pool transitions"
  [ testGroup "ordinary" (cases Ordinary)
  , testGroup "bfee" (cases BFee)
  , testGroup "royalty" (cases Royalty)
  , testGroup "double royalty" (cases DoubleRoyalty)
  , testGroup "double royalty withdrawal"
      [ testGroup "first slot" (withdrawalCases FirstRoyalty)
      , testGroup "second slot" (withdrawalCases SecondRoyalty)
      ]
  ]
  where
    cases family =
      [ testProperty "fair swap remains valid" (check family FairSwap (Right ()))
      , testProperty "swap cannot receive X without paying Y" (check family NoTokenPayout (Left ()))
      , testProperty "ADA as X can be swapped for Y" (check family AdaAsXFairSwap (Right ()))
      , testProperty "ADA as X cannot enter without Y leaving" (check family AdaAsXNoTokenPayout (Left ()))
      , testProperty "X can be swapped for ADA as Y" (check family AdaAsYFairSwap (Right ()))
      , testProperty "ADA as Y cannot remain unchanged during a swap" (check family AdaAsYNoTokenPayout (Left ()))
      , testProperty "negative input reserve is rejected" (check family NegativeInputReserve (Left ()))
      , testProperty "zero input reserve is rejected" (check family ZeroInputReserve (Left ()))
      , testProperty "swap remains valid with one unit of input reserve" (check family OneUnitReserveFairSwap (Right ()))
      , testProperty "swap remains valid with one unit of output reserve" (check family OneUnitOutputReserveFairSwap (Right ()))
      , testProperty "zero output reserve is rejected" (check family ZeroOutputReserve (Left ()))
      , testProperty "foreign token quantity cannot decrease" (check family LostForeignToken (Left ()))
      , testProperty "foreign token quantity cannot increase" (check family AddedForeignToken (Left ()))
      , testProperty "ADA cannot leave a token-token pool during a swap" (check family LostAdaFromTokenPool (Left ()))
      , testProperty "input must hold exactly one NFT" (check family ExtraInputNft (Left ()))
      , testProperty "zero effective swap fee is rejected" (check family ZeroEffectiveFee (Left ()))
      , testProperty "swap fee above denominator is rejected" (check family ExcessiveSwapFee (Left ()))
      , testProperty "deposit must mint liquidity" (check family WrongDepositDirection (Left ()))
      , testProperty "redeem must return liquidity" (check family WrongRedeemDirection (Left ()))
      , testProperty "balanced deposit remains valid" (check family ValidDeposit (Right ()))
      , testProperty "balanced redeem remains valid" (check family ValidRedeem (Right ()))
      ]
    withdrawalCases slot =
      [ testProperty "withdrawal preserves foreign tokens" (checkDoubleRoyaltyWithdrawal slot 10 True (Right ()))
      , testProperty "withdrawal rejects foreign token skim from 10 to 9" (checkDoubleRoyaltyWithdrawal slot 9 True (Left ()))
      , testProperty "withdrawal rejects removal of a foreign token" (checkDoubleRoyaltyWithdrawal slot 0 True (Left ()))
      , testProperty "withdrawal rejects increased foreign token quantity" (checkDoubleRoyaltyWithdrawal slot 11 True (Left ()))
      , testProperty "withdrawal still requires its policy credential" (checkDoubleRoyaltyWithdrawal slot 10 False (Left ()))
      ]

data RoyaltySlot = FirstRoyalty | SecondRoyalty

-- Exercise the pool spending validator with the withdrawal policy credential
-- present in the context. The external policy's signature and order validation
-- are not evaluated here. Royalty deltas and the successor datum are identical
-- between the valid withdrawal and each foreign-token mutation.
checkDoubleRoyaltyWithdrawal :: RoyaltySlot -> Integer -> Bool -> Either () () -> Property
checkDoubleRoyaltyWithdrawal slot foreignAfter includeCredential expected = withTests 1 $ property $ do
  poolRef <- forAll genTxOutRef
  treasuryAddress <- forAll genValidatorHash
  firstKey <- forAll random32bs
  secondKey <- forAll random32bs
  let (x, y, nft, lq) = genAssetClasses
      before = DoubleRoyalty.DoubleRoyaltyPoolConfig
        nft x y lq 99000 100 100 100 10 10 20 20 20 20 [] treasuryAddress firstKey secondKey 0
      after = case slot of
        FirstRoyalty -> before
          { DoubleRoyalty.firstRoyaltyX = 15
          , DoubleRoyalty.firstRoyaltyY = 13
          , DoubleRoyalty.nonce = 1
          }
        SecondRoyalty -> before
          { DoubleRoyalty.secondRoyaltyX = 15
          , DoubleRoyalty.secondRoyaltyY = 13
          , DoubleRoyalty.nonce = 1
          }
      poolValue xQty yQty foreignQty = mkValues
        [ mkValue x xQty, mkValue y yQty, mkValue nft 1
        , mkValue lq (Pool.maxLqCap - 2000), mkValue genFakeToken foreignQty
        ] (mkAdaValue 3000000)
      poolHash = validatorHash doubleRoyaltyPoolValidator
      input = mkTxIn poolRef $ mkTxOut (OutputDatum $ mkDatum before)
        (poolValue 100 100 10) poolHash
      successor = mkTxOut (OutputDatum $ mkDatum after)
        (poolValue 95 93 foreignAfter) poolHash
      withdrawalCredential = StakingHash . ScriptCredential . ValidatorHash . BuiltinByteString $
        mkByteString "a062da72f9f3280fbed90b3e95ea943aabd7eff9c82343324a9726ad"
      txInfo = (mkPoolTxInfo input successor)
        { txInfoWdrl = AssocMap.fromList $ if includeCredential then [(withdrawalCredential, 0)] else [] }
      context = toData $ mkContext txInfo (mkPurpose poolRef)
      redeemer = toData $ DoubleRoyalty.DoubleRoyaltyPoolRedeemer DoubleRoyalty.WithdrawRoyalty 0
      result = eraseBoth $ evalWithArgs (wrapValidator PDoubleRoyaltyPool.poolValidatorT)
        [toData before, redeemer, context]
  result === expected

check :: Family -> Scenario -> Either () () -> Property
check family scenario expected = withTests 1 $ property $ do
  poolRef <- forAll genTxOutRef
  treasuryAddress <- forAll genValidatorHash
  royaltyKey <- forAll random28bs
  secondRoyaltyKey <- forAll random28bs
  let (tokenX, tokenY, nft, lq) = genAssetClasses
      adaAsX = scenario == AdaAsXFairSwap || scenario == AdaAsXNoTokenPayout
      adaAsY = scenario == AdaAsYFairSwap || scenario == AdaAsYNoTokenPayout
      x = if adaAsX then mkAdaAssetClass else tokenX
      y = if adaAsY then mkAdaAssetClass else tokenY
      cap = Pool.maxLqCap
      (xIn, yIn, xOut, yOut, lqOut) = case scenario of
        NoTokenPayout -> (100, 100, 110, 100, cap - 2000)
        AdaAsXFairSwap -> (3000000, 1000000000, 3300000, 910000000, cap - 2000)
        AdaAsXNoTokenPayout -> (3000000, 1000000000, 3300000, 1000000000, cap - 2000)
        AdaAsYFairSwap -> (1000000000, 3000000, 1100000000, 2730000, cap - 2000)
        AdaAsYNoTokenPayout -> (1000000000, 3000000, 1100000000, 3000000, cap - 2000)
        OneUnitReserveFairSwap -> (100, 100, 110, 10, cap - 2000)
        OneUnitOutputReserveFairSwap -> (100, 100, 10000, 1, cap - 2000)
        ZeroOutputReserve -> (100, 100, 110, 0, cap - 2000)
        WrongDepositDirection -> (100, 100, 110, 110, cap - 2000)
        WrongRedeemDirection -> (100, 100, 110, 110, cap - 2010)
        ValidDeposit -> (100, 100, 110, 110, cap - 2010)
        ValidRedeem -> (100, 100, 99, 99, cap - 1900)
        _ -> (100, 100, 110, 91, cap - 2000)
      action = case scenario of
        WrongDepositDirection -> Pool.Deposit
        ValidDeposit -> Pool.Deposit
        WrongRedeemDirection -> Pool.Redeem
        ValidRedeem -> Pool.Redeem
        _ -> Pool.Swap
      royaltyAction = case scenario of
        WrongDepositDirection -> Royalty.Deposit
        ValidDeposit -> Royalty.Deposit
        WrongRedeemDirection -> Royalty.Redeem
        ValidRedeem -> Royalty.Redeem
        _ -> Royalty.Swap
      doubleRoyaltyAction = case scenario of
        WrongDepositDirection -> DoubleRoyalty.Deposit
        ValidDeposit -> DoubleRoyalty.Deposit
        WrongRedeemDirection -> DoubleRoyalty.Redeem
        ValidRedeem -> DoubleRoyalty.Redeem
        _ -> DoubleRoyalty.Swap
      treasuryX = case scenario of
        NegativeInputReserve -> 101
        ZeroInputReserve -> 100
        OneUnitReserveFairSwap -> 99
        _ -> 0
      fee = case scenario of
        ZeroEffectiveFee -> 0
        ExcessiveSwapFee -> 100001
        _ -> 100000
      foreignBefore = if scenario == LostForeignToken then 10 else 0
      foreignAfter = case scenario of
        LostForeignToken -> 9
        AddedForeignToken -> 1
        _ -> 0
      nftBefore = if scenario == ExtraInputNft then 2 else 1
      poolValue xQty yQty lqQty nftQty foreignQty adaQty =
        mkValues
          [ mkValue x xQty, mkValue y yQty, mkValue lq lqQty
          , mkValue nft nftQty, mkValue genFakeToken foreignQty
          ] (if adaAsX || adaAsY then mempty else mkAdaValue adaQty)
      ordinary = mkPoolConfig nft x y lq fee 0 treasuryX 0 [] 0 treasuryAddress
      bfee = mkPoolBFeeConfig nft x y lq fee fee 0 treasuryX 0 [] 0 treasuryAddress
      royalty = Royalty.RoyaltyPoolConfig nft x y lq fee 0 0 treasuryX 0 0 0 [] treasuryAddress royaltyKey 0
      doubleRoyalty = DoubleRoyalty.DoubleRoyaltyPoolConfig nft x y lq fee 0 0 0 treasuryX 0 0 0 0 0 [] treasuryAddress royaltyKey secondRoyaltyKey 0
      poolScriptHash = case family of
        Ordinary -> mkPoolValidator
        BFee -> validatorHash poolBFeeValidator
        Royalty -> validatorHash royaltyPoolValidator
        DoubleRoyalty -> validatorHash doubleRoyaltyPoolValidator
      datum = case family of
        Ordinary -> OutputDatum $ mkDatum ordinary
        BFee -> OutputDatum $ mkDatum bfee
        Royalty -> OutputDatum $ mkDatum royalty
        DoubleRoyalty -> OutputDatum $ mkDatum doubleRoyalty
      input = mkTxIn poolRef $ mkTxOut datum
        (poolValue xIn yIn (cap - 2000) nftBefore foreignBefore 3000000) poolScriptHash
      successor = mkTxOut datum
        (poolValue xOut yOut lqOut 1 foreignAfter
          (if scenario == LostAdaFromTokenPool then 2999999 else 3000000)) poolScriptHash
      context = toData $ mkContext (mkPoolTxInfo input successor) (mkPurpose poolRef)
      result = case family of
        Ordinary -> eraseBoth $ evalWithArgs (wrapValidator PPool.poolValidatorT)
          [toData ordinary, toData $ mkPoolRedeemer 0 action, context]
        BFee -> eraseBoth $ evalWithArgs (wrapValidator PPoolBFee.poolBFeeValidatorT)
          [toData bfee, toData $ mkPoolRedeemer 0 action, context]
        Royalty -> eraseBoth $ evalWithArgs (wrapValidator PRoyaltyPool.poolValidatorT)
          [toData royalty, toData $ Royalty.RoyaltyPoolRedeemer royaltyAction 0, context]
        DoubleRoyalty -> eraseBoth $ evalWithArgs (wrapValidator PDoubleRoyaltyPool.poolValidatorT)
          [toData doubleRoyalty, toData $ DoubleRoyalty.DoubleRoyaltyPoolRedeemer doubleRoyaltyAction 0, context]
  result === expected
