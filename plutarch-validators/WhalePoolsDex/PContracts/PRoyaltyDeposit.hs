{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module WhalePoolsDex.PContracts.PRoyaltyDeposit (
    royaltyDepositValidatorT,
) where

import qualified GHC.Generics as GHC
import Generics.SOP (Generic, I (I))

import Plutarch
import Plutarch.Api.V2.Contexts
import Plutarch.DataRepr
import Plutarch.Lift
import Plutarch.Prelude
import Plutarch.Api.V1 (PMaybeData, PPubKeyHash, PValue)
import Plutarch.Extra.TermCont

import PExtra.API
import PExtra.Ada
import PExtra.List (pelemAt)
import PExtra.Monadic (tlet, tletField, tmatch)

import WhalePoolsDex.PContracts.PApi       (containsSignature, getRewardValue', maxLqCap, pmin, tletUnwrap)
import WhalePoolsDex.PContracts.POrder     (OrderAction (Apply, Refund), OrderRedeemer)
import WhalePoolsDex.PContracts.PDeposit   (DepositConfig(..), validChange', minAssetReward)
import WhalePoolsDex.PContracts.PRoyaltyPool (extractPoolConfig)

import qualified WhalePoolsDex.Contracts.Proxy.Deposit as D

royaltyDepositValidatorT :: ClosedTerm (DepositConfig :--> OrderRedeemer :--> PScriptContext :--> PBool)
royaltyDepositValidatorT = plam $ \conf' redeemer' ctx' -> unTermCont $ do
    ctx  <- pletFieldsC @'["txInfo", "purpose"] ctx'
    conf <- pletFieldsC @'["x", "y", "lq", "poolNft", "exFee", "rewardPkh", "stakePkh", "collateralAda"] conf'
    redeemer <- pletFieldsC @'["poolInIx", "orderInIx", "rewardOutIx", "action"] redeemer'
    let 
        collateralAda = getField @"collateralAda" conf

        rewardPkh = getField @"rewardPkh" conf
        txInfo'   = getField @"txInfo" ctx
        action    = getField @"action" redeemer
    
    txInfo  <- pletFieldsC @'["inputs", "outputs", "signatories"] txInfo'
    
    pure $
        pmatch action $ \case
            Apply -> unTermCont $ do
                inputs  <- tletUnwrap $ getField @"inputs"  txInfo
                outputs <- tletUnwrap $ getField @"outputs" txInfo
                let
                    poolInIx    = getField @"poolInIx"    redeemer
                    orderInIx   = getField @"orderInIx"   redeemer
                    rewardOutIx = getField @"rewardOutIx" redeemer

                    stakePkh  = getField @"stakePkh"  conf
                    exFee     = getField @"exFee" conf

                    x  = getField @"x"  conf
                    y  = getField @"y"  conf
                    lq = getField @"lq" conf

                rewardOut   <- tlet $ pelemAt # rewardOutIx # outputs
                rewardValue <- tlet $ getRewardValue' # rewardOut # rewardPkh # stakePkh

                poolIn'   <- tlet $ pelemAt # poolInIx # inputs
                poolIn    <- pletFieldsC @'["outRef", "resolved"] poolIn'
                let pool  = getField @"resolved" poolIn

                poolValue <- tletField @"value" pool
                let poolIdentity = -- operation is performed with the pool selected by the user 
                        let requiredNft = pfromData $ getField @"poolNft" conf
                            nftAmount = assetClassValueOf # poolValue # requiredNft
                        in nftAmount #== 1
                
                poolInputDatum <- tlet $ extractPoolConfig # pool
                poolConf       <- pletFieldsC @'["treasuryX", "treasuryY", "royaltyX", "royaltyY"] poolInputDatum
                let
                    treasuryX = getField @"treasuryX" poolConf
                    treasuryY = getField @"treasuryY" poolConf

                    royaltyX = getField @"royaltyX" poolConf
                    royaltyY = getField @"royaltyY" poolConf

                selfIn'   <- tlet $ pelemAt # orderInIx # inputs
                selfIn    <- pletFieldsC @'["outRef", "resolved"] selfIn'
                selfValue <-
                    let self = pfromData $ getField @"resolved" selfIn
                    in tletField @"value" self

                PSpending selfRef' <- pmatchC $ getField @"purpose" ctx
                let 
                    selfIdentity =
                        let selfRef   = pfield @"_0" # selfRef'
                            selfInRef = getField @"outRef" selfIn
                        in selfRef #== selfInRef -- check that orderInIx points to the actual order

                    strictInputs = -- ensure double satisfaction attack is not possible
                        let inputsLength = plength # inputs
                        in inputsLength #== 2

                liquidity <-
                    let lqNegative = assetClassValueOf # poolValue # lq
                    in tlet $ maxLqCap - lqNegative

                reservesX <- tlet $ (assetClassValueOf # poolValue # x) - treasuryX - royaltyX
                reservesY <- tlet $ (assetClassValueOf # poolValue # y) - treasuryY - royaltyY

                minRewardByX <- tlet $ minAssetReward # selfValue # x # reservesX # liquidity # exFee # collateralAda
                minRewardByY <- tlet $ minAssetReward # selfValue # y # reservesY # liquidity # exFee # collateralAda
                let validChange = -- pair excess is returned to user
                        pif
                            (minRewardByX #== minRewardByY)
                            (pcon PTrue)
                            ( pif
                                (minRewardByX #< minRewardByY)
                                (validChange' # rewardValue # y # minRewardByY # minRewardByX # reservesY # liquidity)
                                (validChange' # rewardValue # x # minRewardByX # minRewardByY # reservesX # liquidity)
                            )
                    minReward = pmin # minRewardByX # minRewardByY
                    validReward = -- calculated minimal output of LQ tokens is satisfied
                        let actualReward = assetClassValueOf # rewardValue # lq
                        in minReward #<= actualReward
                pure $ poolIdentity #&& selfIdentity #&& strictInputs #&& validChange #&& validReward
            Refund ->
                let sigs = pfromData $ getField @"signatories" txInfo
                 in containsSignature # sigs # rewardPkh -- user signed the refund
