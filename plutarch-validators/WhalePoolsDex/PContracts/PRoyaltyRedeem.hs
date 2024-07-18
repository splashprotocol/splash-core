{-# LANGUAGE UndecidableInstances #-}

module WhalePoolsDex.PContracts.PRoyaltyRedeem (
    royaltyRedeemValidatorT,
) where

import Plutarch
import Plutarch.Api.V2
import Plutarch.Api.V1.Value
import Plutarch.DataRepr
import Plutarch.Prelude
import Plutarch.Extra.TermCont

import PExtra.API
import PExtra.Monadic (tlet, tmatch)

import WhalePoolsDex.PContracts.PApi       (containsSignature, getRewardValue', maxLqCap)
import WhalePoolsDex.PContracts.POrder     (OrderAction (Apply, Refund), OrderRedeemer)
import WhalePoolsDex.PContracts.PRedeem    (RedeemConfig(..), calcMinReturn, calcOutput)
import WhalePoolsDex.PContracts.PRoyaltyFeeSwitch (extractPoolConfig)

royaltyRedeemValidatorT :: ClosedTerm (RedeemConfig :--> OrderRedeemer :--> PScriptContext :--> PBool)
royaltyRedeemValidatorT = plam $ \conf' redeemer' ctx' -> unTermCont $ do
    ctx      <- pletFieldsC @'["txInfo", "purpose"] ctx'
    conf     <- pletFieldsC @'["x", "y", "lq", "poolNft", "exFee", "rewardPkh", "stakePkh"] conf'
    redeemer <- pletFieldsC @'["poolInIx", "orderInIx", "rewardOutIx", "action"] redeemer'
    let
        rewardPkh = getField @"rewardPkh" conf
        txInfo' = getField @"txInfo" ctx

    txInfo  <- pletFieldsC @'["inputs", "outputs", "signatories"] txInfo'
    action <- tletUnwrap $ getField @"action" redeemer
    pure $
        pmatch action $ \case
            Apply -> unTermCont $ do
                inputs  <- tletUnwrap $ getField @"inputs"  txInfo
                outputs <- tletUnwrap $ getField @"outputs" txInfo
                let
                    poolInIx    = getField @"poolInIx" redeemer
                    orderInIx   = getField @"orderInIx" redeemer
                    rewardOutIx = getField @"rewardOutIx" redeemer

                    stakePkh  = getField @"stakePkh"  conf

                    x  = getField @"x"  conf
                    y  = getField @"y"  conf
                    lq = getField @"lq" conf

                    exFee   = getField @"exFee"  conf

                rewardOut   <- tlet $ pelemAt # rewardOutIx # outputs
                rewardValue <- tlet $ getRewardValue' # rewardOut # rewardPkh # stakePkh

                poolIn'   <- tlet $ pelemAt # poolInIx # inputs
                poolIn    <- pletFieldsC @'["outRef", "resolved"] poolIn'
                let 
                    pool         = getField @"resolved" poolIn
                    poolValue    = pfield @"value" # pool
                    poolIdentity = -- operation is performed with the pool selected by the user 
                        let 
                            requiredNft = pfromData $ getField @"poolNft" conf
                            nftAmount   = assetClassValueOf # poolValue # requiredNft
                        in nftAmount #== 1

                poolInputDatum <- tlet $ extractPoolConfig # pool
                poolConf       <- pletFieldsC @'["treasuryX", "treasuryY", "royaltyX", "royaltyY"] poolInputDatum
                let
                    treasuryX = getField @"treasuryX" poolConf
                    treasuryY = getField @"treasuryY" poolConf

                    royaltyX = getField @"royaltyX" poolConf
                    royaltyY = getField @"royaltyY" poolConf

                selfIn' <- tlet $ pelemAt # orderInIx # inputs
                selfIn  <- pletFieldsC @'["outRef", "resolved"] selfIn'
                let selfValue = pfield @"value" # (getField @"resolved" selfIn)

                PSpending selfRef' <- tmatch (pfromData $ getField @"purpose" ctx)
                let 
                    selfIdentity =
                        let 
                            selfRef = pfromData $ pfield @"_0" # selfRef'
                            selfInRef = pfromData $ getField @"outRef" selfIn
                        in selfRef #== selfInRef
                    
                collateralAda <- -- we reserve a small amount of ADA to put it into user output later
                    let inAda = plovelaceValueOf # selfValue
                    in tlet $ inAda - exFee

                let strictInputs = -- ensure double satisfaction attack is not possible
                        let inputsLength = plength # inputs
                        in inputsLength #== 2

                liquidity <-
                    let lqNegative = assetClassValueOf # poolValue # lq
                    in tlet $ maxLqCap - lqNegative

                outs <- tlet $ calcOutput # rewardValue # x # y # collateralAda
                inLq <- tlet $ assetClassValueOf # selfValue # lq

                let 
                    outAda = plovelaceValueOf # rewardValue
                    
                    minReturnX = calcMinReturn # liquidity # inLq # poolValue # x # (treasuryX + royaltyX)
                    minReturnY = calcMinReturn # liquidity # inLq # poolValue # y # (treasuryY + royaltyY)

                    outX  = pfromData $ pfield @"_0" # outs
                    outY  = pfromData $ pfield @"_1" # outs
                    opAda = pfromData $ pfield @"_2" # outs

                    fairShare = minReturnX #<= outX #&& minReturnY #<= outY -- output shares are proportional to the total LQ and LQ returned by the user
                    fairFee = opAda + collateralAda #<= outAda -- output ADA (if present) plus collateral ADA is returned in full to the user

                pure $ poolIdentity #&& selfIdentity #&& strictInputs #&& fairShare #&& fairFee
            Refund ->
                let sigs = pfromData $ getField @"signatories" txInfo
                 in containsSignature # sigs # rewardPkh -- user signed the refund
