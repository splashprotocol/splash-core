{-# LANGUAGE UndecidableInstances #-}

module WhalePoolsDex.PContracts.PRoyaltyWithdrawOrder (
    RoyaltyWithdrawConfig (..),
    WithdrawData(..),
    WithdrawRoyaltyDataToSign(..),
    royaltyWithdrawOrderValidatorT,
    parseRoyaltyWithdrawDatum,
    extractPoolConfigFromDatum,
    extractRoyaltyWithdrawConfigFromDatum,
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
import WhalePoolsDex.PContracts.PRoyaltyPool       (RoyaltyPoolConfig(..), parseDatum)
import WhalePoolsDex.PContracts.POrder
import Data.Text                    as T
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16  as Hex
import qualified Data.Text.Encoding      as E

{-|
    -- The purpose of the royalty withdrawal contract is to ensure the validity and integrity of the withdrawal process. 
    -- The script defines two main actions:
    --
    -- 1. **Apply** - Initiates the withdrawal process and must satisfy the following conditions:
    --    
    --    * The difference in non-royalty-related ADA between the request input and the royalty output must be equal to the `exFee` field, 
    --      verifying that the transaction fee is properly accounted for.
    --    
    --    * The royalty UTXO (which contains the withdrawn royalty) must be sent to the `royaltyAddress`, 
    --      specified as the public key hash of the recipient.
    --
    -- 2. **Refund** - Allows the user to cancel the withdrawal request. In this case, the only requirement 
    --    is the user's signature, which must match the `royaltyAddress` to authorize the refund.


    -- `WithdrawData` is a data structure representing the details of a royalty withdrawal request.
    --
    -- +--------------------+--------------------------------+
    -- | Field Name         | Description                    |
    -- +--------------------+--------------------------------+
    -- | `poolNft`          | The NFT identifying the target |
    -- |                    | pool from which the royalties  |
    -- |                    | are being withdrawn.           |
    -- +--------------------+--------------------------------+
    -- | `withdrawRoyaltyX` | The amount of TokenX to be     |
    -- |                    | withdrawn from the royalty     |
    -- |                    | pool.                          |
    -- +--------------------+--------------------------------+
    -- | `withdrawRoyaltyY` | The amount of TokenY to be     |
    -- |                    | withdrawn from the royalty     |
    -- |                    | pool.                          |
    -- +--------------------+--------------------------------+
    -- | `royaltyAddress`   | The public key hash of the     |
    -- |                    | royalty recipient's address.   |
    -- +--------------------+--------------------------------+
    -- | `exFee`            | The fee to be paid for the     |
    -- |                    | transaction processing (batcher|
    -- |                    | fee).                          |
    -- +--------------------+--------------------------------+
-}
newtype WithdrawData (s :: S)
    = WithdrawData
        ( Term
            s
            ( PDataRecord
                '[ "poolNft"          ':= PAssetClass
                 , "withdrawRoyaltyX" ':= PInteger
                 , "withdrawRoyaltyY" ':= PInteger
                 , "royaltyAddress"   ':= PPubKeyHash
                 , "exFee"            ':= PInteger        
                 ]
            )
        )
    deriving stock (GHC.Generic)
    deriving
        (PIsData, PDataFields, PlutusType)

instance DerivePlutusType WithdrawData where type DPTStrat _ = PlutusTypeData

instance PTryFrom PData WithdrawData

--   `additionalBytes` is a byte array representing data that must be added 
--   at the start of the raw `DataToSign` version. The necessity of 
--   `additionalBytes` arises from the combination of CIP-30 and CIP-0008, 
--   which introduce user-related data into `messageToSign`.
--   To ensure the creation of a correct `messageToSign`, this data is 
--   incorporated within the contract when constructing the final 
--   `messageToSign`, combining `additionalBytes` with `OperationRelatedData`.

newtype RoyaltyWithdrawConfig (s :: S)
    = RoyaltyWithdrawConfig
        ( Term
            s
            ( PDataRecord
                '[ "withdrawData"    ':= WithdrawData      
                 , "signature"       ':= PByteString
                 , "additionalBytes" ':= PByteString
                 ]
            )
        )
    deriving stock (GHC.Generic)
    deriving
        (PIsData, PDataFields, PlutusType)

instance DerivePlutusType RoyaltyWithdrawConfig where type DPTStrat _ = PlutusTypeData

instance PTryFrom PData (PAsData RoyaltyWithdrawConfig)

{-|
    -- `WithdrawRoyaltyDataToSign` is a data structure that encapsulates the fields required to be signed 
    -- for validating a royalty withdrawal request. The table below provides descriptions 
    -- for each field in the structure:
    --
    -- +----------------+---------------------------------------------------+
    -- | Field Name     | Description                                       |
    -- +----------------+---------------------------------------------------+
    -- | `withdrawData` | An instance of `WithdrawData` containing details  |
    -- |                | about the withdrawal request, such as the amount  |
    -- |                | to be withdrawn and the recipient's information.  |
    -- +----------------+---------------------------------------------------+
    -- | `poolNonce`    | An integer representing the current nonce value   |
    -- |                | from the royalty pool datum. It ensures the       |
    -- |                | uniqueness of the transaction, helping to prevent |
    -- |                | spam or replay attacks on the royalty withdrawal  |
    -- |                | process.                                          |
    -- +----------------+---------------------------------------------------+
-}
newtype WithdrawRoyaltyDataToSign (s :: S)
    = WithdrawRoyaltyDataToSign
        ( Term
            s
            ( PDataRecord
                '[ "withdrawData" ':= WithdrawData      
                 , "poolNonce"    ':= PInteger
                 ]
            )
        )
    deriving stock (GHC.Generic)
    deriving
        (PIsData, PDataFields, PlutusType)

instance DerivePlutusType WithdrawRoyaltyDataToSign where type DPTStrat _ = PlutusTypeData

instance PTryFrom PData (PAsData WithdrawRoyaltyDataToSign)

newtype RoyaltyWithdrawRedeemer (s :: S)
    = RoyaltyWithdrawRedeemer
        ( Term
            s
            ( PDataRecord
                '[ "poolInIx"    ':= PInteger
                 , "selfIdx"     ':= PInteger
                 , "rewardOutIx" ':= PInteger
                 ]
            )
        )
    deriving stock (GHC.Generic)
    deriving
        (PIsData, PDataFields, PlutusType)

instance DerivePlutusType RoyaltyWithdrawRedeemer where type DPTStrat _ = PlutusTypeData

extractPoolConfigFromDatum :: PMemberFields PTxOut '["datum"] s as => HRec as -> Term s RoyaltyPoolConfig
extractPoolConfigFromDatum outputDatum = unTermCont $ do
    inputDatum  <- tletUnwrap $ getField @"datum" outputDatum
    POutputDatum inputDatum' <- pmatchC inputDatum
    inputParsedDatum <- tletField @"outputDatum" inputDatum'
    pure $ parseDatum # inputParsedDatum

parseRoyaltyWithdrawDatum :: ClosedTerm (PDatum :--> RoyaltyWithdrawConfig)
parseRoyaltyWithdrawDatum = plam $ \newDatum -> unTermCont $ do
  PDatum poolDatum <- pmatchC newDatum
  tletUnwrap $ ptryFromData @(RoyaltyWithdrawConfig) $ poolDatum

extractRoyaltyWithdrawConfigFromDatum :: PMemberFields PTxOut '["datum"] s as => HRec as -> Term s RoyaltyWithdrawConfig
extractRoyaltyWithdrawConfigFromDatum outputDatum = unTermCont $ do
    inputDatum  <- tletUnwrap $ getField @"datum" outputDatum
    POutputDatum inputDatum' <- pmatchC inputDatum
    inputParsedDatum <- tletField @"outputDatum" inputDatum'
    pure $ parseRoyaltyWithdrawDatum # inputParsedDatum

royaltyWithdrawOrderValidatorT :: ClosedTerm (RoyaltyWithdrawConfig :--> OrderRedeemer :--> PScriptContext :--> PBool)
royaltyWithdrawOrderValidatorT = plam $ \conf' redeemer' ctx' -> unTermCont $ do
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
                inputDatum  <- tlet $ extractPoolConfigFromDatum parsedPoolInput
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
