module WhalePoolsDex.PValidators (
    poolValidator,
    poolBalanceValidator,
    swapValidator,
    depositValidator,
    redeemValidator,
    validatorAddress,
    wrapValidator,
    redeemBalanceValidator,
    depositBalanceValidator,
    royaltyPoolValidator,
    doubleRoyaltyPoolValidator,
    royaltyDepositValidator,
    doubleRoyaltyDepositValidator,
    royaltyRedeemValidator,
    doubleRoyaltyRedeemValidator,
    royaltyWithdrawOrderValidator,
    royaltyPooldaoV1ActionOrderValidator
) where

import PlutusLedgerApi.V1.Scripts (Validator (getValidator))
import PlutusLedgerApi.V1.Address

import qualified WhalePoolsDex.PContracts.PDeposit     as PD
import qualified WhalePoolsDex.PContracts.PPool        as PP
import qualified WhalePoolsDex.PContracts.PBalancePool as PBP
import qualified WhalePoolsDex.PContracts.PPoolBFee    as PPB
import qualified WhalePoolsDex.PContracts.PRedeem      as PR
import qualified WhalePoolsDex.PContracts.PSwap        as PS
import qualified WhalePoolsDex.PContracts.PDepositBalance as PBD
import qualified WhalePoolsDex.PContracts.PRedeemBalance  as PBR
import qualified WhalePoolsDex.PContracts.PRoyaltyPool    as PRP
import qualified WhalePoolsDex.PContracts.PDoubleRoyaltyPool   as PDRP
import qualified WhalePoolsDex.PContracts.PRoyaltyDeposit   as PRD
import qualified WhalePoolsDex.PContracts.PDoubleRoyaltyDeposit   as PDRD
import qualified WhalePoolsDex.PContracts.PRoyaltyRedeem    as PRR
import qualified WhalePoolsDex.PContracts.PDoubleRoyaltyRedeem    as PDRR
import qualified WhalePoolsDex.PContracts.PRoyaltyWithdrawOrder as PRWC
import qualified WhalePoolsDex.PContracts.PRoyaltyDAOV1ActionOrder as PRDAOV1Request

import Plutarch
import Plutarch.Api.V2 (mkValidator, validatorHash)
import PlutusLedgerApi.V1.Crypto (PubKeyHash)
import Plutarch.Api.V2.Contexts (PScriptContext)
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)
import Plutarch.Internal

cfgForValidator :: Config
cfgForValidator = Config NoTracing

wrapValidator ::
    (PIsData dt, PIsData rdmr) =>
    Term s (dt :--> rdmr :--> PScriptContext :--> PBool) ->
    Term s (PData :--> PData :--> PScriptContext :--> POpaque)
wrapValidator validator = plam $ \datum redeemer ctx ->
    let dt = pfromData $ punsafeCoerce datum
        rdmr = pfromData $ punsafeCoerce redeemer
        result = validator # dt # rdmr # ctx
     in popaque $ pif result (pcon PUnit) (ptraceError "Validator reduced to False")

poolValidator :: Validator
poolValidator = mkValidator cfgForValidator $ wrapValidator PP.poolValidatorT

poolBFeeValidator :: Validator
poolBFeeValidator = mkValidator cfgForValidator $ wrapValidator PPB.poolBFeeValidatorT

poolBalanceValidator :: Validator
poolBalanceValidator = mkValidator cfgForValidator $ wrapValidator PBP.balancePoolValidatorT

swapValidator :: Validator
swapValidator = mkValidator cfgForValidator $ wrapValidator PS.swapValidatorT

depositValidator :: Validator
depositValidator = mkValidator cfgForValidator $ wrapValidator PD.depositValidatorT

redeemValidator :: Validator
redeemValidator = mkValidator cfgForValidator $ wrapValidator PR.redeemValidatorT

validatorAddress :: Validator -> Address
validatorAddress = scriptHashAddress . validatorHash

depositBalanceValidator :: Validator
depositBalanceValidator = mkValidator cfgForValidator $ wrapValidator PBD.depositValidatorT

royaltyPoolValidator :: Validator
royaltyPoolValidator = mkValidator cfgForValidator $ wrapValidator PRP.poolValidatorT

royaltyWithdrawOrderValidator :: Validator
royaltyWithdrawOrderValidator = mkValidator cfgForValidator $ wrapValidator PRWC.royaltyWithdrawOrderValidatorT

royaltyDepositValidator :: Validator
royaltyDepositValidator = mkValidator cfgForValidator $ wrapValidator PRD.royaltyDepositValidatorT

royaltyRedeemValidator :: Validator
royaltyRedeemValidator = mkValidator cfgForValidator $ wrapValidator PRR.royaltyRedeemValidatorT

redeemBalanceValidator :: Validator
redeemBalanceValidator = mkValidator cfgForValidator $ wrapValidator PBR.redeemBalanceValidatorT

royaltyPooldaoV1ActionOrderValidator :: Validator
royaltyPooldaoV1ActionOrderValidator = mkValidator cfgForValidator $ wrapValidator PRDAOV1Request.daoV1ActionOrderValidator

doubleRoyaltyPoolValidator :: Validator
doubleRoyaltyPoolValidator = mkValidator cfgForValidator $ wrapValidator PDRP.poolValidatorT

doubleRoyaltyDepositValidator :: Validator
doubleRoyaltyDepositValidator = mkValidator cfgForValidator $ wrapValidator PDRD.doubleRoyaltyDepositValidatorT

doubleRoyaltyRedeemValidator :: Validator
doubleRoyaltyRedeemValidator = mkValidator cfgForValidator $ wrapValidator PDRR.doubleRoyaltyRedeemValidatorT