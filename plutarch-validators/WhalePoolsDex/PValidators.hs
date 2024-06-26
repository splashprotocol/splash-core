module WhalePoolsDex.PValidators (
    poolValidator,
    swapValidator,
    depositValidator,
    redeemValidator,
    validatorAddress,
    wrapValidator,
    stableDepositValidator,
    stableRedeemValidator
) where

import PlutusLedgerApi.V1.Scripts (Validator (getValidator))
import PlutusLedgerApi.V1.Address

import qualified WhalePoolsDex.PContracts.PDeposit  as PD
import qualified WhalePoolsDex.PContracts.PPool     as PP
import qualified WhalePoolsDex.PContracts.PPoolBFee as PPB
import qualified WhalePoolsDex.PContracts.PRedeem   as PR
import qualified WhalePoolsDex.PContracts.PSwap     as PS

import qualified WhalePoolsDex.PContracts.PDepositStable as PSD
import qualified WhalePoolsDex.PContracts.PRedeemStable  as PSR

import Plutarch
import Plutarch.Api.V2 (mkValidator, validatorHash)
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

swapValidator :: Validator
swapValidator = mkValidator cfgForValidator $ wrapValidator PS.swapValidatorT

depositValidator :: Validator
depositValidator = mkValidator cfgForValidator $ wrapValidator PD.depositValidatorT

redeemValidator :: Validator
redeemValidator = mkValidator cfgForValidator $ wrapValidator PR.redeemValidatorT

stableDepositValidator :: Validator
stableDepositValidator = mkValidator cfgForValidator $ wrapValidator PSD.stableDepositValidatorT

stableRedeemValidator :: Validator
stableRedeemValidator = mkValidator cfgForValidator $ wrapValidator PSR.stableRedeemValidatorT

validatorAddress :: Validator -> Address
validatorAddress = scriptHashAddress . validatorHash
