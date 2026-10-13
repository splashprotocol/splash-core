{-# LANGUAGE OverloadedStrings #-}

module Tests.FeeSwitchBFee (feeSwitchBFee) where

import Eval (evalWithArgs)
import qualified Data.ByteString as BS
import Gen.DepositGen (genAssetClasses, genFakeToken)
import Gen.Models
import Gen.Utils (eraseBoth)
import Hedgehog (Property, forAll, property, withTests, (===))
import Plutarch
import Plutarch.Api.V2 (scriptHash, validatorHash)
import Plutarch.Prelude
import PlutusLedgerApi.V1.Scripts (getScriptHash, unMintingPolicyScript)
import PlutusLedgerApi.V2
import PlutusTx.Builtins.Internal (BuiltinByteString (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)
import qualified WhalePoolsDex.Contracts.PoolBFee as PoolBFee
import WhalePoolsDex.Contracts.Proxy.FeeSwitch (DAOAction (..), DAORedeemer (..))
import WhalePoolsDex.PMintingValidators (daoBFeeMintPolicyValidator, wrapMintingValidator)
import qualified WhalePoolsDex.PContracts.PFeeSwitchBFee as BFeeDAO
import WhalePoolsDex.PValidators (poolBFeeValidator)

data Case
  = ValidAddressChange
  | ZeroDirectionalFee
  | MissingSignature
  | ValidWithdraw
  | WrongWithdrawAmount
  | WrongWithdrawDestination
  | WithdrawLeaksForeignToken
  | WithdrawMissingInputNft
  | ValidStakeChange
  | WrongStakeScript
  | ValidTreasuryFeeChange
  | ExcessiveTreasuryFee
  | ValidAdminChange
  | AdminChangesPoolValue
  | ValidPoolFeeChange
  | ValidPoolFeeYChange
  | PoolFeeNotEditable
  | ExcessivePoolFee
  | ExcessivePoolFeeY

feeSwitchBFee :: TestTree
feeSwitchBFee = testGroup "BFee DAO policy"
  [ testProperty "authorized address change is valid" (check ValidAddressChange (Right ()))
  , testProperty "unrelated action rejects zero directional fee" (check ZeroDirectionalFee (Left ()))
  , testProperty "address change requires threshold signatures" (check MissingSignature (Left ()))
  , testProperty "treasury withdraw transfers exactly the accrued amounts" (check ValidWithdraw (Right ()))
  , testProperty "treasury withdraw rejects a pool value mismatch" (check WrongWithdrawAmount (Left ()))
  , testProperty "treasury withdraw rejects another destination" (check WrongWithdrawDestination (Left ()))
  , testProperty "treasury withdraw preserves unrelated assets" (check WithdrawLeaksForeignToken (Left ()))
  , testProperty "treasury withdraw requires the input pool NFT" (check WithdrawMissingInputNft (Left ()))
  , testProperty "staking credential can change" (check ValidStakeChange (Right ()))
  , testProperty "staking change preserves script credential" (check WrongStakeScript (Left ()))
  , testProperty "treasury fee can change within bounds" (check ValidTreasuryFeeChange (Right ()))
  , testProperty "treasury fee above bound is rejected" (check ExcessiveTreasuryFee (Left ()))
  , testProperty "DAO administrators can change" (check ValidAdminChange (Right ()))
  , testProperty "administrator change cannot alter pool value" (check AdminChangesPoolValue (Left ()))
  , testProperty "pool fee can change within bounds" (check ValidPoolFeeChange (Right ()))
  , testProperty "Y directional fee can change within bounds" (check ValidPoolFeeYChange (Right ()))
  , testProperty "pool fee cannot change when editing is disabled" (check PoolFeeNotEditable (Left ()))
  , testProperty "pool fee above bound is rejected" (check ExcessivePoolFee (Left ()))
  , testProperty "Y directional fee above bound is rejected" (check ExcessivePoolFeeY (Left ()))
  ]

check :: Case -> Either () () -> Property
check scenario expected = withTests 1 $ property $ do
  poolRef <- forAll genTxOutRef
  let (x, y, nft, lq) = genAssetClasses
      feeRef = poolRef { txOutRefIdx = txOutRefIdx poolRef + 1 }
      keyHash n = PubKeyHash (BuiltinByteString (BS.replicate 28 n))
      scriptCredentialHash n = ValidatorHash (BuiltinByteString (BS.replicate 28 n))
      admin1 = keyHash 1
      admin2 = keyHash 2
      admin3 = keyHash 3
      treasuryBefore = scriptCredentialHash 4
      treasuryAfter = scriptCredentialHash 5
      admins = [admin1, admin2, admin3]
      feeEditable = case scenario of
        PoolFeeNotEditable -> False
        _ -> True
      policy = daoBFeeMintPolicyValidator admins 2 feeEditable
      policyCred = StakingHash . ScriptCredential . ValidatorHash . getScriptHash . scriptHash $ unMintingPolicyScript policy
      newPolicyCred = StakingHash (ScriptCredential (scriptCredentialHash 7))
      directionalFee = case scenario of
        ZeroDirectionalFee -> 0
        _ -> 90000
      before = PoolBFee.PoolConfig nft x y lq directionalFee 90000 2 20 10 [policyCred] 0 treasuryBefore
      after = case scenario of
        ValidWithdraw -> before { PoolBFee.treasuryX = 5, PoolBFee.treasuryY = 3 }
        WrongWithdrawAmount -> before { PoolBFee.treasuryX = 5, PoolBFee.treasuryY = 3 }
        WrongWithdrawDestination -> before { PoolBFee.treasuryX = 5, PoolBFee.treasuryY = 3 }
        WithdrawLeaksForeignToken -> before { PoolBFee.treasuryX = 5, PoolBFee.treasuryY = 3 }
        WithdrawMissingInputNft -> before { PoolBFee.treasuryX = 5, PoolBFee.treasuryY = 3 }
        ValidStakeChange -> before
        WrongStakeScript -> before
        ValidTreasuryFeeChange -> before { PoolBFee.treasuryFee = 3 }
        ExcessiveTreasuryFee -> before { PoolBFee.treasuryFee = 20001 }
        ValidAdminChange -> before { PoolBFee.daoPolicy = [newPolicyCred] }
        AdminChangesPoolValue -> before { PoolBFee.daoPolicy = [newPolicyCred] }
        ValidPoolFeeChange -> before { PoolBFee.poolFeeNumX = 89000 }
        ValidPoolFeeYChange -> before { PoolBFee.poolFeeNumY = 89000 }
        PoolFeeNotEditable -> before { PoolBFee.poolFeeNumX = 89000 }
        ExcessivePoolFee -> before { PoolBFee.poolFeeNumX = 100000 }
        ExcessivePoolFeeY -> before { PoolBFee.poolFeeNumY = 100000 }
        _ -> before { PoolBFee.treasuryAddress = treasuryAfter }
      poolValue xQty yQty nftQty foreignQty = mkValues
        [ mkValue x xQty, mkValue y yQty, mkValue nft nftQty
        , mkValue lq (0x7fffffffffffffff - 2000)
        , mkValue genFakeToken foreignQty, mkAdaValue 3000000
        ] mempty
      oldNftQty = case scenario of
        WithdrawMissingInputNft -> 0
        _ -> 1
      oldValue = poolValue 1000 1000 oldNftQty 10
      (newX, newY, newForeign) = case scenario of
        ValidWithdraw -> (985, 993, 10)
        WrongWithdrawAmount -> (986, 993, 10)
        WrongWithdrawDestination -> (985, 993, 10)
        WithdrawLeaksForeignToken -> (985, 993, 9)
        WithdrawMissingInputNft -> (985, 993, 10)
        AdminChangesPoolValue -> (999, 1000, 10)
        _ -> (1000, 1000, 10)
      newValue = poolValue newX newY 1 newForeign
      action = case scenario of
        ValidWithdraw -> WithdrawTreasury
        WrongWithdrawAmount -> WithdrawTreasury
        WrongWithdrawDestination -> WithdrawTreasury
        WithdrawLeaksForeignToken -> WithdrawTreasury
        WithdrawMissingInputNft -> WithdrawTreasury
        ValidStakeChange -> ChangeStakePart
        WrongStakeScript -> ChangeStakePart
        ValidTreasuryFeeChange -> ChangeTreasuryFee
        ExcessiveTreasuryFee -> ChangeTreasuryFee
        ValidAdminChange -> ChangeAdminAddress
        AdminChangesPoolValue -> ChangeAdminAddress
        ValidPoolFeeChange -> ChangePoolFee
        ValidPoolFeeYChange -> ChangePoolFee
        PoolFeeNotEditable -> ChangePoolFee
        ExcessivePoolFee -> ChangePoolFee
        ExcessivePoolFeeY -> ChangePoolFee
        _ -> ChangeTreasuryAddress
      poolHash = validatorHash poolBFeeValidator
      oldStake = Just (StakingHash (PubKeyCredential admin3))
      newStake = case scenario of
        ValidStakeChange -> Just (StakingHash (PubKeyCredential admin2))
        _ -> oldStake
      successorHash = case scenario of
        WrongStakeScript -> scriptCredentialHash 6
        _ -> poolHash
      poolInput = mkTxIn poolRef $ mkTxOutWithSC (OutputDatum $ mkDatum before) oldValue poolHash oldStake
      feeInput = mkTxIn feeRef $ mkTxOut' NoOutputDatum (mkAdaValue 10000000) admin3
      successor = mkTxOutWithSC (OutputDatum $ mkDatum after) newValue successorHash newStake
      isWithdraw = case scenario of
        ValidWithdraw -> True
        WrongWithdrawAmount -> True
        WrongWithdrawDestination -> True
        WithdrawLeaksForeignToken -> True
        WithdrawMissingInputNft -> True
        _ -> False
      destination = case scenario of
        WrongWithdrawDestination -> treasuryAfter
        _ -> treasuryBefore
      treasuryOutput = mkTxOut NoOutputDatum
        (mkValues [mkValue x 15, mkValue y 7, mkAdaValue 3000000] mempty) destination
      outputs = if isWithdraw then [successor, treasuryOutput] else [successor]
      signers = case scenario of
        MissingSignature -> [admin1]
        _ -> [admin1, admin2]
      txInfo = (mkTxInfoWithSignaturesAndMinting [poolInput, feeInput] successor signers policyCred)
        { txInfoOutputs = outputs }
      context = toData $ mkContext txInfo (mkRewardingPurpose policyCred)
      redeemer = toData $ DAORedeemer action 0 nft
      result = eraseBoth $ evalWithArgs
        (wrapMintingValidator $ BFeeDAO.daoMultisigPolicyValidatorT (pconstant admins) (pconstant 2) (pconstant feeEditable))
        [redeemer, context]
  result === expected
