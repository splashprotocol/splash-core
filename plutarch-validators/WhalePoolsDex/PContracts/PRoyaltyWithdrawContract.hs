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
        signature     = getField @"signature"    conf
    
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
                    -- We should extract all fields from input pool datum, to verify immutability 
                    -- of all fields except of royalty related and nonce. RoyaltyX/Y fields will be used
                    -- in verification of withdraw process
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

                    -- In new pool datum we should onlye verify new value of royaltyX/royaltyY.
                    -- Another fields should be the same
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

                    -- Pool value lengt (tokens qty) is the same
                    correctTokensQtyInPool = prevPoolValueLength #== newPoolValueLength

                    -- Pool output should contains poolNft 
                    poolIdentity = (assetClassValueOf # outputPoolValue # prevPoolNft) #== 1

                    -- Withdraw should be validated by next rules:
                    -- * Withdraw should be lte actual royaltyX/Y values
                    -- * New royalty value (in config) should be less than the previous value by withdrawX/Y values
                    -- * New X/Y value should be less than the previous value by withdrawX/Y values
                    -- * LQ qty should't be changed
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
                    dataToSign = pserialiseData # (punsafeCoerce withdrawDataRaw)

                    correctExFee = (inputAdaValue - nonWithdrawADAValueInOut) #== exFee

                    signatureIsCorrect = pverifyEd25519Signature # royaltyPubKey # dataToSign # signature

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
