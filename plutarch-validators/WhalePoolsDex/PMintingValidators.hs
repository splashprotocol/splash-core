{-# LANGUAGE OverloadedStrings #-}

module WhalePoolsDex.PMintingValidators (
    poolNftMiningValidator,
    poolLqMiningValidator,
    daoMintPolicyValidator,
    wrapMintingValidator,
    daoBalanceMintPolicyValidator,
    royaltyPoolDAOV1Validator,
    royaltyWithdrawPoolValidator
) where

import Plutarch
import Plutarch.Api.V2 (mkMintingPolicy)
import Plutarch.Api.V2.Contexts (PScriptContext)
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)

import qualified WhalePoolsDex.PContracts.PAssets as A
import WhalePoolsDex.PContracts.PFeeSwitch
import PlutusLedgerApi.V1.Scripts (MintingPolicy(..))
import PlutusLedgerApi.V1.Value   (TokenName(..), AssetClass(..))
import PlutusLedgerApi.V1.Crypto  (PubKeyHash)
import PlutusLedgerApi.V1.Contexts
import PlutusTx.Builtins.Internal
import qualified WhalePoolsDex.PContracts.PFeeSwitchBalancePool as BDao
import qualified WhalePoolsDex.PContracts.PRoyaltyDAOV1 as PRDAOV1
import qualified WhalePoolsDex.PContracts.PRoyaltyWithdrawPool as PRWP
import Data.ByteString (ByteString)

cfgForMintingValidator :: Config
cfgForMintingValidator = Config NoTracing

wrapMintingValidator ::
    PIsData rdmr =>
    ClosedTerm (rdmr :--> PScriptContext :--> PBool) ->
    ClosedTerm (PData :--> PScriptContext :--> POpaque)
wrapMintingValidator validator = plam $ \rdmr' ctx ->
    let rdmr = pfromData $ punsafeCoerce rdmr'
        result = validator # rdmr # ctx
     in popaque $ pif result (pcon PUnit) (ptraceError "Minting validator reduced to False")

poolNftMiningValidator :: TxOutRef -> TokenName -> MintingPolicy
poolNftMiningValidator oref tn =
    mkMintingPolicy cfgForMintingValidator $
        wrapMintingValidator $
            A.poolNftMintValidatorT (pconstant oref) (pconstant tn)

poolLqMiningValidator :: TxOutRef -> TokenName -> Integer -> MintingPolicy
poolLqMiningValidator oref tn emission =
    mkMintingPolicy cfgForMintingValidator $
        wrapMintingValidator $
            A.poolLqMintValidatorT (pconstant oref) (pconstant tn) (pconstant emission)

daoMintPolicyValidator :: [PubKeyHash] -> Integer -> Bool -> MintingPolicy
daoMintPolicyValidator stakeAdminPkh threshold lpFeeIsEditable = 
    mkMintingPolicy cfgForMintingValidator $ 
        wrapMintingValidator $
            daoMultisigPolicyValidatorT (pconstant stakeAdminPkh) (pconstant threshold) (pconstant lpFeeIsEditable)

daoBalanceMintPolicyValidator :: [PubKeyHash] -> Integer -> Bool -> MintingPolicy
daoBalanceMintPolicyValidator stakeAdminPkh threshold lpFeeIsEditable = 
    mkMintingPolicy cfgForMintingValidator $ 
        wrapMintingValidator $
            BDao.daoMultisigPolicyValidatorT (pconstant stakeAdminPkh) (pconstant threshold) (pconstant lpFeeIsEditable)

royaltyPoolDAOV1Validator :: [ByteString] -> Integer -> Bool -> MintingPolicy
royaltyPoolDAOV1Validator admins threshold lpFeeIsEditable = 
    mkMintingPolicy cfgForMintingValidator $ 
        wrapMintingValidator $ 
            PRDAOV1.daoMultisigPolicyValidatorT (pconstant admins) (pconstant threshold) (pconstant lpFeeIsEditable)

royaltyWithdrawPoolValidator :: MintingPolicy
royaltyWithdrawPoolValidator = 
    mkMintingPolicy cfgForMintingValidator $ 
        wrapMintingValidator $ 
            PRWP.royaltyWithdrawPoolValidatorT

