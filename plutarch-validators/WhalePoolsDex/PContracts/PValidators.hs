module WhalePoolsDex.PValidators (
    poolValidator,
    swapValidator,
    depositValidator,
    redeemValidator,
    validatorAddress,
    wrapValidator,
    depositBalanceValidator,
    redeemBalanceValidator,
    poolBalanceValidator,
    royaltyPoolValidator,
    royaltyWithdrawOrderValidator
) where

import PlutusLedgerApi.V1.Scripts (Validator (getValidator))
import PlutusLedgerApi.V1.Address

import qualified WhalePoolsDex.PContracts.PDeposit      as PD
import qualified WhalePoolsDex.PContracts.PPool         as PP
import qualified WhalePoolsDex.PContracts.PBalancePool  as PBP
import qualified WhalePoolsDex.PContracts.PPoolBFee     as PPB
import qualified WhalePoolsDex.PContracts.PRedeem       as PR
import qualified WhalePoolsDex.PContracts.PSwap         as PS
import qualified WhalePoolsDex.PContracts.PDepositBalance as PBD
import qualified WhalePoolsDex.PContracts.PRedeemBalance  as PBR
import qualified WhalePoolsDex.PContracts.PRoyaltyPool    as PRP
import qualified WhalePoolsDex.PContracts.PRoyaltyWithdrawOrder as PRWC

import Plutarch
import Plutarch.Api.V2 (mkValidator, validatorHash)
import Plutarch.Api.V2.Contexts (PScriptContext)
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)

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
poolValidator = mkValidator $ wrapValidator PP.poolValidatorT

poolBFeeValidator :: Validator
poolBFeeValidator = mkValidator $ wrapValidator PPB.poolValidatorT

poolBalanceValidator :: Validator
poolBalanceValidator = mkValidator cfgForValidator $ wrapValidator PBP.balancePoolValidatorT

swapValidator :: Validator
swapValidator = mkValidator $ wrapValidator PS.swapValidatorT

depositValidator :: Validator
depositValidator = mkValidator $ wrapValidator PD.depositValidatorT

redeemValidator :: Validator
redeemValidator = mkValidator $ wrapValidator PR.redeemValidatorT

depositBalanceValidator :: Validator
depositBalanceValidator = mkValidator $ wrapValidator PBD.depositValidatorT

redeemBalanceValidator :: Validator
redeemBalanceValidator = mkValidator $ wrapValidator PBR.redeemValidatorT

royaltyPoolValidator :: Validator
royaltyPoolValidator = mkValidator $ wrapValidator PRP.poolValidatorT

royaltyWithdrawOrderValidator :: Validator
royaltyWithdrawOrderValidator = mkValidator $ wrapValidator PRWC.royaltyWithdrawOrderValidatorT

validatorAddress :: Validator -> Address
validatorAddress = scriptHashAddress . validatorHash