{-# LANGUAGE UndecidableInstances #-}

module WhalePoolsDex.PContracts.PDoubleRoyaltyWithdrawOrder (
    RoyaltyWithdrawConfig (..),
    WithdrawData(..),
    WithdrawRoyaltyDataToSign(..),
    doubleRoyaltyWithdrawOrderValidatorT,
    extractDoubleRoyaltyWithdrawConfigFromDatum,
    extractDoubleRoyaltyPoolConfigFromDatum
) where

import qualified GHC.Generics as GHC

import Plutarch
import Plutarch.Builtin
import Plutarch.Api.V2
import Plutarch.DataRepr
import Plutarch.Prelude
import Plutarch.Extra.TermCont

import PExtra.API
import PExtra.Monadic (tlet, tletField, tmatch)
import Plutarch.Extra.Maybe
import PExtra.Ada

import WhalePoolsDex.PContracts.PApi
import WhalePoolsDex.PContracts.POrder
import qualified WhalePoolsDex.PContracts.PDoubleRoyaltyPool as PDRoyalty (DoubleRoyaltyPoolConfig(..), parseDatum)
import WhalePoolsDex.PContracts.PRoyaltyWithdrawOrder       (RoyaltyWithdrawConfig (..), WithdrawData(..), WithdrawRoyaltyDataToSign(..))
import Data.Text                    as T
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16  as Hex
import qualified Data.Text.Encoding      as E

extractDoubleRoyaltyPoolConfigFromDatum :: PMemberFields PTxOut '["datum"] s as => HRec as -> Term s PDRoyalty.DoubleRoyaltyPoolConfig
extractDoubleRoyaltyPoolConfigFromDatum outputDatum = unTermCont $ do
    inputDatum  <- tletUnwrap $ getField @"datum" outputDatum
    POutputDatum inputDatum' <- pmatchC inputDatum
    inputParsedDatum <- tletField @"outputDatum" inputDatum'
    pure $ PDRoyalty.parseDatum # inputParsedDatum

parseDoubleRoyaltyWithdrawDatum :: ClosedTerm (PDatum :--> PDRoyalty.DoubleRoyaltyPoolConfig)
parseDoubleRoyaltyWithdrawDatum = plam $ \newDatum -> unTermCont $ do
  PDatum poolDatum <- pmatchC newDatum
  tletUnwrap $ ptryFromData @(PDRoyalty.DoubleRoyaltyPoolConfig) $ poolDatum

extractDoubleRoyaltyWithdrawConfigFromDatum :: PMemberFields PTxOut '["datum"] s as => HRec as -> Term s PDRoyalty.DoubleRoyaltyPoolConfig
extractDoubleRoyaltyWithdrawConfigFromDatum outputDatum = unTermCont $ do
    inputDatum  <- tletUnwrap $ getField @"datum" outputDatum
    POutputDatum inputDatum' <- pmatchC inputDatum
    inputParsedDatum <- tletField @"outputDatum" inputDatum'
    pure $ parseDoubleRoyaltyWithdrawDatum # inputParsedDatum

doubleRoyaltyWithdrawOrderValidatorT :: ClosedTerm (RoyaltyWithdrawConfig :--> OrderRedeemer :--> PScriptContext :--> PBool)
doubleRoyaltyWithdrawOrderValidatorT = plam $ \conf' redeemer' ctx' -> unTermCont $ do
    ctx          <- pletFieldsC @'["txInfo", "purpose"] ctx'
    conf         <- pletFieldsC @'["withdrawData", "signature"] conf'
    let
        withdrawDataRaw = getField @"withdrawData" conf
        withdrawData'   = pfromData withdrawDataRaw
        
    withdrawData <- pletFieldsC @'["poolNft", "withdrawRoyaltyX", "withdrawRoyaltyY", "royaltyAddress", "royaltyPubKey", "exFee"] withdrawData'
    let
        poolNft          = getField @"poolNft"          withdrawData
        withdrawRoyaltyX = getField @"withdrawRoyaltyX" withdrawData
        withdrawRoyaltyY = getField @"withdrawRoyaltyY" withdrawData
        royaltyAddress   = getField @"royaltyAddress"   withdrawData
        exFee            = getField @"exFee"            withdrawData
    
    txInfo  <- pletFieldsC @'["inputs", "outputs", "signatories"] $ getField @"txInfo" ctx

    redeemer <- pletFieldsC @'["orderInIx", "poolInIx", "rewardOutIx", "action"] redeemer'
    let
        selfIdx     = getField @"orderInIx"    redeemer
        poolInIx    = getField @"poolInIx"   redeemer
        rewardOutIx = getField @"rewardOutIx" redeemer
        action      = getField @"action" redeemer
    
    inputs  <- tletUnwrap $ getField @"inputs"  txInfo
    outputs <- tletUnwrap $ getField @"outputs" txInfo

    -- Extract input pool
    poolIn'   <- tlet $ pelemAt # poolInIx # inputs
    poolIn    <- pletFieldsC @'["outRef", "resolved"] poolIn'
    let
        poolInput  = getField @"resolved" poolIn

    pure $
        pmatch action $ \case
            Refund ->
                let sigs = pfromData $ getField @"signatories" txInfo
                 in containsSignature # sigs # royaltyAddress -- user signed the refund
            Apply -> unTermCont $ do
                withdrawRequestIn' <- tlet $ pelemAt # selfIdx # inputs
                withdrawRequestIn  <- pletFieldsC @'["outRef", "resolved"] withdrawRequestIn'
                selfValue <-
                    let self = pfromData $ getField @"resolved" withdrawRequestIn
                    in tletField @"value" self

                inputAdaValue <- tlet $ assetClassValueOf # selfValue # pAdaAssetClass

                PSpending selfRef' <- tmatch (pfromData $ getField @"purpose" ctx)

                parsedPoolInput <- pletFieldsC @'["datum"] poolInput
                inputDatum  <- tlet $ extractDoubleRoyaltyPoolConfigFromDatum parsedPoolInput
                prevConfig  <- pletFieldsC @'["poolNft", "poolX", "poolY"] inputDatum
                let
                    {-|
                        -- To ensure the immutability of the pool datum, all fields should be extracted from the input pool datum, 
                        -- except for the fields related to royalties and the nonce, which are allowed to change. 
                        -- The `royaltyX` and `royaltyY` fields will be utilized to verify the correctness of the withdrawal process.
                    -}
                    prevPoolNft = getField @"poolNft" prevConfig
                    prevPoolX   = getField @"poolX"  prevConfig
                    prevPoolY   = getField @"poolY"  prevConfig

                -- Royalty withdraw output
                withdrawOut   <- tlet $ pelemAt # rewardOutIx # outputs
                withdrawValue <- tlet $ getRewardValue' # withdrawOut # royaltyAddress # pdnothing
                let
                    xIsAda = (prevPoolX #== pAdaAssetClass)
                    yIsAda = (prevPoolY #== pAdaAssetClass)

                    xValueInWithdrawOut = assetClassValueOf # withdrawValue # prevPoolX
                    yValueInWithdrawOut = assetClassValueOf # withdrawValue # prevPoolY

                    -- used to verify correct exFee 
                    nonWithdrawADAValueInOut = 
                        pif 
                            (xIsAda)
                            (xValueInWithdrawOut - withdrawRoyaltyX)
                            (pif (yIsAda) (yValueInWithdrawOut - withdrawRoyaltyY) (assetClassValueOf # withdrawValue # pAdaAssetClass))
                -- Verifications:
                let 
                    -- Withdraw utxo should contains x/y tokens gte toWithdrawX/toWithdrawY
                    correctFinalWithdrawOutValue = (withdrawRoyaltyX #<= xValueInWithdrawOut) #&& (withdrawRoyaltyY #<= yValueInWithdrawOut)
 
                    correctExFee = (inputAdaValue - nonWithdrawADAValueInOut) #== exFee

                    -- Tx should contains only two inputs: pool, withdraw request
                    strictInputs = (plength # inputs) #== 2

                    -- We should verify that we are running script on correct request box
                    selfIdentity =
                        let selfRef   = pfromData $ pfield @"_0" # selfRef'
                            withdrawRequestInRef = pfromData $ getField @"outRef" withdrawRequestIn
                        in selfRef #== withdrawRequestInRef

                    -- Correct pool
                    correctPool = prevPoolNft #== poolNft
                
                let
                    withdrawIsCorrect = 
                        correctFinalWithdrawOutValue #&&
                        strictInputs #&&
                        correctExFee #&&
                        selfIdentity #&&
                        correctPool

                pure withdrawIsCorrect
