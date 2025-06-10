module WhalePoolsDex.PContracts.PDoubleRoyaltyWithdrawPool (
    doubleRoyaltyWithdrawPoolValidatorT,
) where

import qualified GHC.Generics       as GHC

import Plutarch
import Plutarch.Builtin
import Plutarch.Api.V2
import Plutarch.DataRepr
import Plutarch.Prelude
import Plutarch.Extra.TermCont
import Plutarch.Crypto (pverifyEd25519Signature, pblake2b_256)
import Plutarch.Unsafe (punsafeCoerce)

import PExtra.API
import PExtra.Monadic (tlet, tletField, tmatch, tcon)
import PExtra.Ada

import WhalePoolsDex.PContracts.PDoubleRoyaltyWithdrawOrder
import WhalePoolsDex.PContracts.PRoyaltyWithdrawOrder       (extractRoyaltyWithdrawConfigFromDatum)
import WhalePoolsDex.PContracts.PDoubleRoyaltyPool          (DoubleRoyaltyPoolConfig(..), findPoolOutput)
import Plutarch.Api.V1.Scripts
import Plutarch.Api.V1.Address
import Data.Text                    as T
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16  as Hex
import qualified Data.Text.Encoding      as E
import Plutarch.Trace

mkByteString :: T.Text -> BS.ByteString
mkByteString input = unsafeFromEither (Hex.decode . E.encodeUtf8 $ input)

unsafeFromEither :: (Show b) => Either b a -> a
unsafeFromEither (Left err)    = Prelude.error ("Err:" ++ show err)
unsafeFromEither (Right value) = value

royaltyWithdrawRequestScriptHash :: String
royaltyWithdrawRequestScriptHash = "6710f3002759d028a90a08e1538b25d4eaf48c72d88157f71d68dcc0"

royaltyWithdrawRequestScriptHashP :: Term s PValidatorHash
royaltyWithdrawRequestScriptHashP = pcon $ PValidatorHash $ phexByteStr royaltyWithdrawRequestScriptHash

newtype DoubleRoyaltyWithdrawRedeemer (s :: S)
    = DoubleRoyaltyWithdrawRedeemer
        ( Term
            s
            ( PDataRecord
                '[ "poolInIx"  ':= PInteger
                 , "orderInIx" ':= PInteger
                 , "signer"    ':= PInteger
                 ]
            )
        )
    deriving stock (GHC.Generic)
    deriving
        (PIsData, PDataFields, PlutusType)

instance DerivePlutusType DoubleRoyaltyWithdrawRedeemer where type DPTStrat _ = PlutusTypeData

{-|
  Pool Royalty Withdrawal Contract (v2)

  The purpose of the Pool Royalty Withdrawal Contract is to securely manage and validate
  royalty withdrawal requests from a liquidity pool that maintains two separate royalty slots.
  This contract is triggered by the PRewarding action when a royalty withdrawal is initiated.
  It ensures that only legitimate requests are processed, and protects against unauthorized
  changes to the pool's state.

  The contract achieves this by enforcing the following rules:

  1. Restricted Field Modifications:
     - Only the fields `firstRoyaltyX`, `firstRoyaltyY`, `secondRoyaltyX`, `secondRoyaltyY`, and `nonce` can be modified.
     - The royalty slot being withdrawn from must have its `X` and `Y` values reduced by the corresponding withdrawal amounts.
     - The royalty slot not being withdrawn from must remain unchanged.
     - The `nonce` must be incremented, ensuring each withdrawal is unique.

  2. Dual Royalty Slots:
     - The pool maintains two separate royalty slots (first and second), each holding `X` and `Y` token values.
     - The signer determines which royalty slot they are allowed to withdraw from:
         - Signer `0` can withdraw only from the first royalty slot.
         - Other signers can withdraw only from the second royalty slot.

  3. Withdrawal Limits:
     - The requested withdrawal amounts for tokens `X` and `Y` must not exceed the current values
       of the selected royalty slot (`firstRoyaltyX/Y` or `secondRoyaltyX/Y`).
     - The overall `X` and `Y` values in the pool must be reduced accordingly.

  4. Input Validation:
     - A UTXO associated with the User Royalty Withdrawal Contract must be present in the transaction inputs,
       ensuring that the request is linked to a legitimate withdrawal process.
     - This protects against attempts to redirect royalties to unauthorized addresses by copying the original request.

  5. Consistency Checks:
     - The differences in token values before and after the transaction must match the requested withdrawal amounts.
     - The pool's total liquidity (`LQ`) value must remain unchanged.
     - The Lovelace amount used in token-to-token pools must remain unchanged.

  6. Address Integrity:
     - The pool address must remain unchanged throughout the process, ensuring that the withdrawal does not
       inadvertently or maliciously alter the pool destination.

  7. Signature Verification:
     - The withdrawal request must be signed using the appropriate public key for the selected royalty slot:
         - `firstRoyaltyPubKey` for signer `0`.
         - `secondRoyaltyPubKey` for other signers.
     - The signature must validate over the serialized withdrawal data and current pool nonce.

  8. Operation Data Representation:
     - The `operationRelatedData` is represented by the `WithdrawRoyaltyDataToSign` structure,
       which contains the necessary information for validating the transaction.
-}

doubleRoyaltyWithdrawPoolValidatorT :: ClosedTerm (DoubleRoyaltyWithdrawRedeemer :--> PScriptContext :--> PBool)
doubleRoyaltyWithdrawPoolValidatorT = plam $ \redeemer' ctx' -> unTermCont $ do
    ctx      <- pletFieldsC @'["txInfo", "purpose"] ctx'
    redeemer <- pletFieldsC @'["orderInIx", "poolInIx", "signer"] redeemer'

    PRewarding _ <- tmatch (pfromData $ getField @"purpose" ctx)

    txInfo  <- pletFieldsC @'["inputs", "outputs"] $ getField @"txInfo" ctx
    inputs  <- tletUnwrap $ getField @"inputs"  txInfo
    outputs <- tletUnwrap $ getField @"outputs" txInfo
    let
        orderInIx  = getField @"orderInIx" redeemer
        poolInIx   = getField @"poolInIx"  redeemer
        signer     = pfromData $ getField @"signer" redeemer

    -- Extract withdraw request utxo
    orderIn'   <- tlet $ pelemAt # orderInIx # inputs
    orderIn    <- pletFieldsC @'["outRef", "resolved"] orderIn'
    let
        orderInput = getField @"resolved" orderIn
        inputsQtyIsCorrect = (plength # inputs) #== 2

    parsedOrderInput <- pletFieldsC @'["datum", "address"] orderInput
    
    -- Validate royalty withdraw request address
    let
        royaltyRequestAddr  = getField @"address" parsedOrderInput
    cred <- tletField @"credential" royaltyRequestAddr
    correctOrderInputAddress <- tletUnwrap $
        pmatch cred $ \case
            PScriptCredential pcred ->
                let skh   = pfield @"_0" # pcred
                    in pif (skh #== royaltyWithdrawRequestScriptHashP) (pdata inputsQtyIsCorrect) perror
            _ -> perror

    conf' <- tlet $ extractRoyaltyWithdrawConfigFromDatum parsedOrderInput
    conf  <- pletFieldsC @'["withdrawData", "signature", "additionalBytes"] conf'
    let
        signature       = getField @"signature"       conf
        withdrawDataRaw = getField @"withdrawData"    conf
        additionalBytes = getField @"additionalBytes" conf
        withdrawData'   = pfromData withdrawDataRaw
        
    withdrawData <- pletFieldsC @'["poolNft", "withdrawRoyaltyX", "withdrawRoyaltyY", "exFee"] withdrawData'
    let
        poolNft          = getField @"poolNft"          withdrawData
        withdrawRoyaltyX = getField @"withdrawRoyaltyX" withdrawData
        withdrawRoyaltyY = getField @"withdrawRoyaltyY" withdrawData
    
    -- Extract input pool
    poolIn' <- tlet $ pelemAt # poolInIx # inputs
    poolIn  <- pletFieldsC @'["outRef", "resolved"] poolIn'
    let
        poolInput  = getField @"resolved" poolIn

    parsedPoolInput <- pletFieldsC @'["datum", "address", "value"] poolInput
    inputDatum  <- tlet $ extractDoubleRoyaltyPoolConfigFromDatum parsedPoolInput
    prevConfig  <- pletFieldsC @'["poolNft", "poolX", "poolY", "poolLq", "feeNum", "treasuryFee", "firstRoyaltyFee", "secondRoyaltyFee", "treasuryX", "treasuryY", "firstRoyaltyX", "firstRoyaltyY", "secondRoyaltyX", "secondRoyaltyY", "DAOPolicy", "treasuryAddress", "firstRoyaltyPubKey", "secondRoyaltyPubKey", "nonce"] inputDatum
    let
        {-|
            -- To ensure the immutability of the pool datum, all fields should be extracted from the input pool datum, 
            -- except for the fields related to royalties and the nonce, which are allowed to change. 
            -- The `firstRoyaltyX`, `firstRoyaltyY`, `secondRoyaltyX`, and `secondRoyaltyY` fields will be utilized 
            -- to verify the correctness of the withdrawal process.
        -}
        prevPoolNft  = getField @"poolNft" prevConfig  
        prevPoolX    = getField @"poolX"   prevConfig
        prevPoolY    = getField @"poolY"   prevConfig
        prevPoolLq   = getField @"poolLq"  prevConfig
        prevFeeNum   = getField @"feeNum"  prevConfig
        prevTreasuryFeeNum      = getField @"treasuryFee"         prevConfig
        prevFirstRoyaltyFeeNum  = getField @"firstRoyaltyFee"     prevConfig
        prevSecondRoyaltyFeeNum = getField @"secondRoyaltyFee"    prevConfig
        prevTreasuryX           = getField @"treasuryX"           prevConfig
        prevTreasuryY           = getField @"treasuryY"           prevConfig
        prevFirstRoyaltyX       = getField @"firstRoyaltyX"       prevConfig
        prevFirstRoyaltyY       = getField @"firstRoyaltyY"       prevConfig
        prevSecondRoyaltyX      = getField @"secondRoyaltyX"      prevConfig
        prevSecondRoyaltyY      = getField @"secondRoyaltyY"      prevConfig
        prevDAOPolicy           = getField @"DAOPolicy"           prevConfig
        prevTreasuryAddress     = getField @"treasuryAddress"     prevConfig
        prevFirstRoyaltyPubKey  = getField @"firstRoyaltyPubKey"  prevConfig
        prevSecondRoyaltyPubKey = getField @"secondRoyaltyPubKey" prevConfig
        prevNonce               = getField @"nonce"               prevConfig

        -- Input value related fields
        prevPoolValue = getField @"value" parsedPoolInput
        prevXValue    = assetClassValueOf # prevPoolValue # prevPoolX
        prevYValue    = assetClassValueOf # prevPoolValue # prevPoolY
        prevLqValue   = assetClassValueOf # prevPoolValue # prevPoolLq

        prevLovelaceToken2Token =
            pif
                ((prevPoolX #== pAdaAssetClass) #|| (prevPoolY #== pAdaAssetClass))
                (pconstant 0)
                (assetClassValueOf # prevPoolValue # pAdaAssetClass)

        prevPoolValueLength = pValueLength # prevPoolValue

        -- Input pool address
        prevPoolAddr = getField @"address" parsedPoolInput

        -- Pool nft in request equals to pool nft
        correctPool = poolNft #== prevPoolNft

    -- Extract info about output pool
    poolOutput        <- tlet $ findPoolOutput # poolNft # outputs
    parsedPoolOutput  <- pletFieldsC @'["datum", "address", "value"] poolOutput
    outputPoolDatum   <- tlet $ extractDoubleRoyaltyPoolConfigFromDatum parsedPoolOutput
    newPoolConfig     <- pletFieldsC @'["firstRoyaltyX", "firstRoyaltyY", "secondRoyaltyX", "secondRoyaltyY"] outputPoolDatum
    let
        outputPoolValue = getField @"value" parsedPoolOutput

        {-|
            -- In the new pool datum, only the updated values of `royaltyX` and `royaltyY` should be verified. 
            -- All other fields must remain unchanged from the previous pool datum.
        -}
        newFirstRoyaltyX  = getField @"firstRoyaltyX"  newPoolConfig
        newFirstRoyaltyY  = getField @"firstRoyaltyY"  newPoolConfig
        newSecondRoyaltyX = getField @"secondRoyaltyX" newPoolConfig
        newSecondRoyaltyY = getField @"secondRoyaltyY" newPoolConfig

        -- Output pool value
        newPoolValue = getField @"value" parsedPoolOutput
        newXValue    = assetClassValueOf # newPoolValue # prevPoolX
        newYValue    = assetClassValueOf # newPoolValue # prevPoolY
        newLqValue   = assetClassValueOf # newPoolValue # prevPoolLq

        newLovelaceToken2Token =
            pif
                ((prevPoolX #== pAdaAssetClass) #|| (prevPoolY #== pAdaAssetClass))
                (pconstant 0)
                (assetClassValueOf # newPoolValue # pAdaAssetClass)

        newPoolValueLength = pValueLength # newPoolValue

        -- Output pool address
        newPoolAddr = getField @"address" parsedPoolOutput

    -- Verifications:
    let 
        -- Pool address is the same
        correctFinalPoolAddress = prevPoolAddr #== newPoolAddr

        -- Pool value length (tokens qty) is the same
        correctTokensQtyInPool = prevPoolValueLength #== newPoolValueLength

        -- Pool output should contains poolNft 
        poolIdentity = (assetClassValueOf # outputPoolValue # prevPoolNft) #== 1

        {-|
            The withdrawal must be validated according to the following rules:

            * There are two separate royalty slots (first and second), each with `X` and `Y` token values.
            * The signer determines which royalty slot they are allowed to withdraw from:
                - Signer `0` can withdraw only from the first royalty slot.
                - Other signers can withdraw only from the second royalty slot.
            * The withdrawal amount (`withdrawRoyaltyX`, `withdrawRoyaltyY`) must be less than or equal to the corresponding royalty slot value.
            * The royalty slot being withdrawn from must be reduced by the withdrawal amount.
            * The royalty slot not being withdrawn from must remain unchanged.
            * The total liquidity (LQ) value must remain unchanged.
            * The overall `X` and `Y` values must be reduced by the corresponding withdrawal amounts.
        -}
        royaltyWithdrawIsCorrect =
            ( pif 
                (signer #== 0)
                (   withdrawRoyaltyX #<= prevFirstRoyaltyX
                #&& withdrawRoyaltyY #<= prevFirstRoyaltyY
                #&& newFirstRoyaltyX #== (prevFirstRoyaltyX - withdrawRoyaltyX)
                #&& newFirstRoyaltyY #== (prevFirstRoyaltyY - withdrawRoyaltyY)
                #&& prevSecondRoyaltyX #== newSecondRoyaltyX
                #&& prevSecondRoyaltyY #== newSecondRoyaltyY
                )
                (   withdrawRoyaltyX #<= prevSecondRoyaltyX
                #&& withdrawRoyaltyY #<= prevSecondRoyaltyY
                #&& newSecondRoyaltyX #== (prevSecondRoyaltyX - withdrawRoyaltyX)
                #&& newSecondRoyaltyY #== (prevSecondRoyaltyY - withdrawRoyaltyY)
                #&& prevFirstRoyaltyX #== newFirstRoyaltyX
                #&& prevFirstRoyaltyY #== newFirstRoyaltyY
                )
            ) #&& prevLqValue #== newLqValue #&& (prevXValue - withdrawRoyaltyX) #== newXValue #&& (prevYValue - withdrawRoyaltyY) #== newYValue

        correctLovelaceToken2Token = prevLovelaceToken2Token #== newLovelaceToken2Token

        -- Signature verification
    dataToSign <- 
        tcon $ (WithdrawRoyaltyDataToSign $
            pdcons @"withdrawData" @WithdrawData # pdata withdrawData'
                #$ pdcons @"poolNonce" @PInteger # pdata prevNonce
                    # pdnil) 

    let                    
        dataToSignRaw = (additionalBytes <> pserialiseData # (punsafeCoerce dataToSign))

        keyToVerify = pif (signer #== 0) prevFirstRoyaltyPubKey prevSecondRoyaltyPubKey

        signatureIsCorrect = pverifyEd25519Signature # keyToVerify # dataToSignRaw # signature

        -- Tx should contains only two inputs: pool, withdraw request
        strictInputs = (plength # inputs) #== 2
    
    -- New pool config should be the same, except of royalty fields and nonce
    expectedConfig <-
        tcon $ (DoubleRoyaltyPoolConfig $
            pdcons @"poolNft" @PAssetClass # pdata prevPoolNft
                #$ pdcons @"poolX" @PAssetClass # pdata prevPoolX
                #$ pdcons @"poolY" @PAssetClass # pdata prevPoolY
                #$ pdcons @"poolLq" @PAssetClass # pdata prevPoolLq
                #$ pdcons @"feeNum" @PInteger # pdata prevFeeNum
                #$ pdcons @"treasuryFee" @PInteger # pdata prevTreasuryFeeNum
                #$ pdcons @"firstRoyaltyFee" @PInteger # pdata prevFirstRoyaltyFeeNum
                #$ pdcons @"secondRoyaltyFee" @PInteger # pdata prevSecondRoyaltyFeeNum
                #$ pdcons @"treasuryX" @PInteger # pdata prevTreasuryX
                #$ pdcons @"treasuryY" @PInteger # pdata prevTreasuryY
                #$ pdcons @"firstRoyaltyX" @PInteger # pdata newFirstRoyaltyX
                #$ pdcons @"firstRoyaltyY" @PInteger # pdata newFirstRoyaltyY
                #$ pdcons @"secondRoyaltyX" @PInteger # pdata newSecondRoyaltyX
                #$ pdcons @"secondRoyaltyY" @PInteger # pdata newSecondRoyaltyY
                #$ pdcons @"DAOPolicy" @(PBuiltinList (PAsData PStakingCredential)) # pdata prevDAOPolicy
                #$ pdcons @"treasuryAddress" @PValidatorHash # pdata prevTreasuryAddress
                #$ pdcons @"firstRoyaltyPubKey" @PByteString # pdata prevFirstRoyaltyPubKey
                #$ pdcons @"secondRoyaltyPubKey" @PByteString # pdata prevSecondRoyaltyPubKey
                #$ pdcons @"nonce" @PInteger # pdata (prevNonce + 1)
                    # pdnil)
    
    let
        finalPoolConfigIsCorrect = expectedConfig #== outputPoolDatum

        withdrawIsCorrect =
            correctOrderInputAddress #&&
            correctFinalPoolAddress #&&
            correctTokensQtyInPool #&&
            poolIdentity #&&
            royaltyWithdrawIsCorrect #&&
            signatureIsCorrect #&&
            strictInputs #&&
            finalPoolConfigIsCorrect #&&
            correctPool #&&
            correctLovelaceToken2Token

    pure withdrawIsCorrect
