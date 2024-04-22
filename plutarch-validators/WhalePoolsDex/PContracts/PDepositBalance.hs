{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module WhalePoolsDex.PContracts.PDepositBalance (
    DepositBalanceConfig (..),
    depositValidatorT,
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

import WhalePoolsDex.PContracts.PApi          (containsSignature, getRewardValue', maxLqCap, pmin, tletUnwrap)
import WhalePoolsDex.PContracts.POrder        (OrderAction (Apply, Refund), OrderRedeemer)
import WhalePoolsDex.PContracts.PFeeSwitchBalancePool (extractBalancePoolConfig)

import qualified WhalePoolsDex.Contracts.Proxy.DepositBalance as D

newtype DepositBalanceConfig (s :: S)
    = DepositBalanceConfig
        ( Term
            s
            ( PDataRecord
                '[ "poolNft" ':= PAssetClass
                 , "x" ':= PAssetClass
                 , "y" ':= PAssetClass
                 , "lq" ':= PAssetClass
                 , "exFee" ':= PInteger -- execution fee specified by the user
                 , "rewardPkh" ':= PPubKeyHash -- PublicKeyHash of the user
                 , "stakePkh" ':= PMaybeData PPubKeyHash
                 , "collateralAda" ':= PInteger -- we reserve a small amount of ADA to put it into user output later
                 ]
            )
        )
    deriving stock (GHC.Generic)
    deriving
        (PIsData, PDataFields, PlutusType)

instance DerivePlutusType DepositBalanceConfig where type DPTStrat _ = PlutusTypeData

instance PUnsafeLiftDecl DepositBalanceConfig where type PLifted DepositBalanceConfig = D.DepositBalanceConfig
deriving via (DerivePConstantViaData D.DepositBalanceConfig DepositBalanceConfig) instance (PConstantDecl D.DepositBalanceConfig)

depositValidatorT :: ClosedTerm (DepositBalanceConfig :--> OrderRedeemer :--> PScriptContext :--> PBool)
depositValidatorT = plam $ \conf' redeemer' ctx' -> unTermCont $ do
    ctx  <- pletFieldsC @'["txInfo", "purpose"] ctx'
    conf <- pletFieldsC @'["x", "y", "lq", "poolNft", "exFee", "rewardPkh", "stakePkh", "collateralAda"] conf'
    redeemer <- pletFieldsC @'["poolInIx", "orderInIx", "rewardOutIx", "action"] redeemer'
    let 
        collateralAda = getField @"collateralAda" conf

        rewardPkh = getField @"rewardPkh" conf
        txInfo'   = getField @"txInfo" ctx
        action = getField @"action" redeemer
    
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
                
                poolInputDatum <- tlet $ extractBalancePoolConfig # pool
                poolConf       <- pletFieldsC @'["treasuryX", "treasuryY"] poolInputDatum
                let
                    treasuryX = getField @"treasuryX" poolConf
                    treasuryY = getField @"treasuryY" poolConf

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

                reservesX <- tlet $ (assetClassValueOf # poolValue # x) - treasuryX
                reservesY <- tlet $ (assetClassValueOf # poolValue # y) - treasuryY

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

-- Checks whether an asset overflow is returned back to user
validChange' :: Term s (PValue _ _ :--> PAssetClass :--> PInteger :--> PInteger :--> PInteger :--> PInteger :--> PBool)
validChange' =
    phoistAcyclic $
        plam $ \rewardValue overflowAsset overflowAssetInput otherAssetInput overflowAssetReserves liquidity ->
            let diff = overflowAssetInput - otherAssetInput
                excess = pdiv # (diff * overflowAssetReserves) # liquidity
                change = assetClassValueOf # rewardValue # overflowAsset
             in excess #<= change

minAssetReward :: Term s (PValue _ _ :--> PAssetClass :--> PInteger :--> PInteger :--> PInteger :--> PInteger :--> PInteger)
minAssetReward =
    phoistAcyclic $
        plam $ \selfValue asset assetReserves liquidity exFee collateralAda ->
            unTermCont $ do
                assetInput <- tlet $ assetClassValueOf # selfValue # asset
                let depositInput = pif (pIsAda # asset) (assetInput - exFee - collateralAda) assetInput
                pure $ pdiv # (depositInput * liquidity) # assetReserves
