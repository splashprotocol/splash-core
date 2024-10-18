{-# LANGUAGE UndecidableInstances #-}

module WhalePoolsDex.PContracts.PRoyaltyWithdrawContract (
    RoyaltyWithdrawConfig (..),
    royaltyWithdrawValidatorT,
) where

import qualified GHC.Generics as GHC

import Plutarch
import Plutarch.Builtin
import Plutarch.Api.V2
import Plutarch.Api.V1.Tuple
import Plutarch.Api.V1.Value
import Plutarch.Api.V2.Contexts
import Plutarch.DataRepr
import Plutarch.Lift
import Plutarch.Prelude
import Plutarch.Extra.TermCont
import Plutarch.Crypto (pverifyEd25519Signature, pblake2b_256)
import Plutarch.Unsafe (punsafeCoerce)

import PExtra.API
import PExtra.Ada (pIsAda)
import PExtra.Monadic (tlet, tletField, tmatch, tcon)
import Plutarch.Extra.Maybe
import PExtra.Ada

import WhalePoolsDex.PContracts.PApi
import WhalePoolsDex.PContracts.PRoyaltyFeeSwitch (extractPoolConfig)
import WhalePoolsDex.PContracts.PRoyaltyPool      (RoyaltyPoolConfig(..), findPoolOutput, parseDatum, readPoolState)
import WhalePoolsDex.PContracts.POrder
import Plutarch.Api.V1.Scripts                    (PValidatorHash)

{-|
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
    -- | `royaltyPubKey`    | The raw public key associated  |
    -- |                    | with the royalty withdrawal.   |
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
                 , "royaltyPubKey"    ':= PByteString
                 , "exFee"            ':= PInteger        
                 ]
            )
        )
    deriving stock (GHC.Generic)
    deriving
        (PIsData, PDataFields, PlutusType)

instance DerivePlutusType WithdrawData where type DPTStrat _ = PlutusTypeData

instance PTryFrom PData WithdrawData

newtype RoyaltyWithdrawConfig (s :: S)
    = RoyaltyWithdrawConfig
        ( Term
            s
            ( PDataRecord
                '[ "withdrawData" ':= WithdrawData      
                 , "signature"    ':= PByteString
                 ]
            )
        )
    deriving stock (GHC.Generic)
    deriving
        (PIsData, PDataFields, PlutusType)

instance DerivePlutusType RoyaltyWithdrawConfig where type DPTStrat _ = PlutusTypeData

instance PTryFrom PData (PAsData RoyaltyWithdrawConfig)

{-|
    -- `DataToSign` is a data structure that encapsulates the fields required to be signed 
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
newtype DataToSign (s :: S)
    = DataToSign
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

instance DerivePlutusType DataToSign where type DPTStrat _ = PlutusTypeData

instance PTryFrom PData (PAsData DataToSign)

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

{-|
    -- The purpose of the royalty withdrawal contract is to ensure the validity and integrity of the withdrawal process. 
    -- The script defines two main actions:
    --
    -- 1. **Apply** - Initiates the withdrawal process and must satisfy the following conditions:
    --    
    --    * The withdrawal amounts for `TokenX` and `TokenY` must be less than or equal to the corresponding values specified 
    --      in the pool datum, ensuring that the requested withdrawal does not exceed the available royalties.
    --    
    --    * The request signature must be correctly verified, using the private key associated with the `royaltyPubKeyHash256` 
    --      present in the royalty pool datum. This ensures that the request is authorized by the rightful owner.
    --    
    --    * The difference in non-royalty-related ADA between the request input and the royalty output must be equal to the `exFee` field, 
    --      verifying that the transaction fee is properly accounted for.
    --    
    --    * The pool datum must remain unchanged except for the fields `royaltyX`, `royaltyY`, and `royaltyNonce`. 
    --      The `royaltyX` and `royaltyY` fields should be decreased by the amounts specified in the withdrawal request, 
    --      and the `royaltyNonce` should be incremented to maintain data integrity. All other fields must remain the same.
    --    
    --    * The change in the pool's token balances must exactly match the `withdrawRoyaltyX` and `withdrawRoyaltyY` values, 
    --      ensuring that the pool's new state accurately reflects the withdrawal.
    --    
    --    * The royalty UTXO (which contains the withdrawn royalty) must be sent to the `royaltyAddress`, 
    --      specified as the public key hash of the recipient.
    --
    -- 2. **Refund** - Allows the user to cancel the withdrawal request. In this case, the only requirement 
    --    is the user's signature, which must match the `royaltyAddress` to authorize the refund.
-}
royaltyWithdrawValidatorT :: ClosedTerm (RoyaltyWithdrawConfig :--> OrderRedeemer :--> PScriptContext :--> PBool)
royaltyWithdrawValidatorT = plam $ \conf' redeemer' ctx' -> unTermCont $ do
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
        royaltyPubKey    = getField @"royaltyPubKey"    withdrawData
        exFee            = getField @"exFee"            withdrawData
        signature        = getField @"signature"    conf
    
    txInfo  <- pletFieldsC @'["inputs", "outputs", "signatories"] $ getField @"txInfo" ctx

    redeemer <- pletFieldsC @'["orderInIx", "poolInIx", "rewardOutIx", "action"] redeemer'
    let
        selfIdx     = getField @"orderInIx"    redeemer
        poolInIx    = getField @"poolInIx"   redeemer
        rewardOutIx = getField @"rewardOutIx" redeemer
        action      = getField @"action" redeemer

    pure $
        pmatch action $ \case
            Refund ->
                let sigs = pfromData $ getField @"signatories" txInfo
                 in containsSignature # sigs # royaltyAddress -- user signed the refund
            Apply -> unTermCont $ do
                inputs  <- tletUnwrap $ getField @"inputs"  txInfo
                outputs <- tletUnwrap $ getField @"outputs" txInfo
                withdrawRequestIn' <- tlet $ pelemAt # selfIdx # inputs
                withdrawRequestIn  <- pletFieldsC @'["outRef", "resolved"] withdrawRequestIn'
                selfValue <-
                    let self = pfromData $ getField @"resolved" withdrawRequestIn
                    in tletField @"value" self

                inputAdaValue <- tlet $ assetClassValueOf # selfValue # pAdaAssetClass

                PSpending selfRef' <- tmatch (pfromData $ getField @"purpose" ctx)

                -- Extract info about input pool
                poolIn'   <- tlet $ pelemAt # poolInIx # inputs
                poolIn    <- pletFieldsC @'["outRef", "resolved"] poolIn'
                let
                    poolInput  = getField @"resolved" poolIn

                parsedPoolInput <- pletFieldsC @'["datum", "address", "value"] poolInput
                inputDatum  <- tlet $ extractPoolConfigFromDatum parsedPoolInput
                prevConfig  <- pletFieldsC @'["poolNft", "poolX", "poolY", "poolLq", "feeNum", "treasuryFee", "royaltyFee", "treasuryX", "treasuryY", "royaltyX", "royaltyY", "DAOPolicy", "treasuryAddress", "royaltyPubKeyHash256", "royaltyNonce"] inputDatum
                let
                    {-|
                        -- To ensure the immutability of the pool datum, all fields should be extracted from the input pool datum, 
                        -- except for the fields related to royalties and the nonce, which are allowed to change. 
                        -- The `royaltyX` and `royaltyY` fields will be utilized to verify the correctness of the withdrawal process.
                    -}
                    prevPoolNft = getField @"poolNft" prevConfig
                    prevPoolX   = getField @"poolX"  prevConfig
                    prevPoolY   = getField @"poolY"  prevConfig
                    prevPoolLq  = getField @"poolLq" prevConfig
                    prevFeeNum  = getField @"feeNum" prevConfig
                    prevTreasuryFeeNum = getField @"treasuryFee" prevConfig
                    prevRoyaltyFeeNum  = getField @"royaltyFee" prevConfig
                    prevTreasuryX = getField @"treasuryX" prevConfig
                    prevTreasuryY = getField @"treasuryY" prevConfig
                    prevRoyaltyX  = getField @"royaltyX"  prevConfig
                    prevRoyaltyY  = getField @"royaltyY"  prevConfig
                    prevDAOPolicy = getField @"DAOPolicy" prevConfig
                    prevTreasuryAddress       = getField @"treasuryAddress" prevConfig
                    prevroyaltyPubKeyHash256  = getField @"royaltyPubKeyHash256" prevConfig
                    prevRoyaltyNonce          = getField @"royaltyNonce" prevConfig

                    -- Input value related fields
                    prevPoolValue = getField @"value" parsedPoolInput
                    prevXValue    = assetClassValueOf # prevPoolValue # prevPoolX
                    prevYValue    = assetClassValueOf # prevPoolValue # prevPoolY
                    prevLqValue   = assetClassValueOf # prevPoolValue # prevPoolLq

                    prevPoolValueLength = pValueLength # prevPoolValue

                    -- Input pool address
                    prevPoolAddr = getField @"address" parsedPoolInput

                -- Extract info about output pool
                poolOutput        <- tlet $ findPoolOutput # poolNft # outputs
                parsedPoolOutput  <- pletFieldsC @'["datum", "address", "value"] poolOutput
                outputPoolDatum   <- tlet $ extractPoolConfigFromDatum parsedPoolOutput
                newPoolConfig     <- pletFieldsC @'["royaltyX", "royaltyY"] outputPoolDatum
                let
                    outputPoolValue = getField @"value" parsedPoolOutput

                    {-|
                        -- In the new pool datum, only the updated values of `royaltyX` and `royaltyY` should be verified. 
                        -- All other fields must remain unchanged from the previous pool datum.
                    -}
                    newRoyaltyX = getField @"royaltyX" newPoolConfig
                    newRoyaltyY = getField @"royaltyY" newPoolConfig

                    -- Output pool value
                    newPoolValue = getField @"value" parsedPoolOutput
                    newXValue    = assetClassValueOf # newPoolValue # prevPoolX
                    newYValue    = assetClassValueOf # newPoolValue # prevPoolY
                    newLqValue   = assetClassValueOf # newPoolValue # prevPoolLq

                    newPoolValueLength = pValueLength # newPoolValue

                    -- Output pool address
                    newPoolAddr = getField @"address" parsedPoolInput

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
                            (pif (yIsAda) (yValueInWithdrawOut - withdrawRoyaltyY) (assetClassValueOf # withdrawValue # prevPoolY))
                -- Verifications:
                let 
                    -- in t2t case adaToken2TokenDiff should be 0
                    adaToken2TokenDiff =
                        pif
                            (xIsAda #|| yIsAda)
                            (pconstant 0)
                            ((assetClassValueOf # prevPoolValue # pAdaAssetClass) - (assetClassValueOf # newPoolValue # pAdaAssetClass))

                    correctAdaT2TDiff = adaToken2TokenDiff #== 0

                    -- Pool address is the same
                    correctFinalPoolAddress = prevPoolAddr #== newPoolAddr

                    -- Pool value length (tokens qty) is the same
                    correctTokensQtyInPool = prevPoolValueLength #== newPoolValueLength

                    -- Pool output should contains poolNft 
                    poolIdentity = (assetClassValueOf # outputPoolValue # prevPoolNft) #== 1

                    {-|
                        -- The withdrawal must be validated according to the following rules:
                        -- 
                        -- * The withdrawal amount should be less than or equal to the current `royaltyX` and `royaltyY` values.
                        -- * The new royalty values (in the configuration) should be reduced by the withdrawn `withdrawX` and `withdrawY` amounts.
                        -- * The new `X` and `Y` values should also be reduced by the corresponding `withdrawX` and `withdrawY` amounts.
                        -- * The quantity of liquidity (LQ) should remain unchanged.
                    -}
                    royaltyWithdrawIsCorrect =
                        withdrawRoyaltyX #<= prevRoyaltyX #&&
                        withdrawRoyaltyY #<= prevRoyaltyY #&&
                        newRoyaltyX #== (prevRoyaltyX - withdrawRoyaltyX) #&&
                        newRoyaltyY #== (prevRoyaltyY - withdrawRoyaltyY) #&&
                        (prevXValue - withdrawRoyaltyX) #== newXValue #&&
                        (prevYValue - withdrawRoyaltyY) #== newYValue #&&
                        prevLqValue #== newLqValue

                    -- Withdraw utxo should contains x/y tokens gte toWithdrawX/toWithdrawY
                    correctFinalWithdrawOutValue = (withdrawRoyaltyX #<= xValueInWithdrawOut) #&& (withdrawRoyaltyY #<= yValueInWithdrawOut)

                    -- Signature verification
                dataToSign <- 
                    tcon $ (DataToSign $
                        pdcons @"withdrawData" @WithdrawData # pdata withdrawData'
                            #$ pdcons @"poolNonce" @PInteger # pdata prevRoyaltyNonce
                                # pdnil) 

                let                    
                    dataToSignRaw = pserialiseData # (punsafeCoerce dataToSign)

                    correctExFee = (inputAdaValue - nonWithdrawADAValueInOut) #== exFee

                    signatureIsCorrect = pverifyEd25519Signature # royaltyPubKey # dataToSignRaw # signature

                    correctPubKey = (pblake2b_256 # royaltyPubKey) #== prevroyaltyPubKeyHash256

                    -- Tx should contains only two inputs: pool, withdraw request
                    strictInputs = (plength # inputs) #== 2

                    -- We should verify that we are running script on correct request box
                    selfIdentity =
                        let selfRef   = pfromData $ pfield @"_0" # selfRef'
                            withdrawRequestInRef = pfromData $ getField @"outRef" withdrawRequestIn
                        in selfRef #== withdrawRequestInRef
                
                -- New pool config should be the same, except of royalty fields and nonce
                expectedConfig <-
                    tcon $ (RoyaltyPoolConfig $
                        pdcons @"poolNft" @PAssetClass # pdata prevPoolNft
                            #$ pdcons @"poolX" @PAssetClass # pdata prevPoolX
                            #$ pdcons @"poolY" @PAssetClass # pdata prevPoolY
                            #$ pdcons @"poolLq" @PAssetClass # pdata prevPoolLq
                            #$ pdcons @"feeNum" @PInteger # pdata prevFeeNum
                            #$ pdcons @"treasuryFee" @PInteger # pdata prevTreasuryFeeNum
                            #$ pdcons @"royaltyFee" @PInteger # pdata prevRoyaltyFeeNum
                            #$ pdcons @"treasuryX" @PInteger # pdata prevTreasuryX
                            #$ pdcons @"treasuryY" @PInteger # pdata prevTreasuryY
                            #$ pdcons @"royaltyX" @PInteger # pdata newRoyaltyX
                            #$ pdcons @"royaltyY" @PInteger # pdata newRoyaltyY
                            #$ pdcons @"DAOPolicy" @(PBuiltinList (PAsData PStakingCredential)) # pdata prevDAOPolicy
                            #$ pdcons @"treasuryAddress" @PValidatorHash # pdata prevTreasuryAddress
                            #$ pdcons @"royaltyPubKeyHash256" @PByteString # pdata prevroyaltyPubKeyHash256
                            #$ pdcons @"royaltyNonce" @PInteger # pdata (prevRoyaltyNonce + 1)
                                # pdnil)
                
                let
                    finalPoolConfigIsCorrect = expectedConfig #== outputPoolDatum

                    withdrawIsCorrect = 
                        correctAdaT2TDiff #&&
                        correctFinalPoolAddress #&&
                        correctTokensQtyInPool #&&
                        poolIdentity #&&
                        royaltyWithdrawIsCorrect #&&
                        correctFinalWithdrawOutValue #&&
                        signatureIsCorrect #&&
                        strictInputs #&&
                        correctExFee #&&
                        selfIdentity #&&
                        finalPoolConfigIsCorrect #&&
                        correctPubKey

                pure withdrawIsCorrect
