module WhalePoolsDex.PContracts.PRoyaltyWithdrawPool (
    royaltyWithdrawPoolValidatorT,
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

import WhalePoolsDex.PContracts.PRoyaltyWithdrawOrder
import WhalePoolsDex.PContracts.PRoyaltyPool  (RoyaltyPoolConfig(..), findPoolOutput)
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
royaltyWithdrawRequestScriptHash = "92c094b90cf3637a96a13e9bc9a04ce8bb7e48c7ed0b5d1cc5ca7332"

royaltyWithdrawRequestScriptHashP :: Term s PValidatorHash
royaltyWithdrawRequestScriptHashP = pcon $ PValidatorHash $ phexByteStr royaltyWithdrawRequestScriptHash

newtype RoyaltyWithdrawRedeemer (s :: S)
    = RoyaltyWithdrawRedeemer
        ( Term
            s
            ( PDataRecord
                '[ "poolInIx" ':= PInteger
                 , "orderInIx" ':= PInteger
                 ]
            )
        )
    deriving stock (GHC.Generic)
    deriving
        (PIsData, PDataFields, PlutusType)

instance DerivePlutusType RoyaltyWithdrawRedeemer where type DPTStrat _ = PlutusTypeData

{-|
  Pool Royalty Withdrawal Contract

  The purpose of the Pool Royalty Withdrawal Contract is to securely manage and validate
  royalty withdrawal requests from the pool. This contract is triggered by the PRewarding
  action when a royalty withdrawal is initiated. It ensures that only legitimate requests
  are processed, and protects against unauthorized changes to the pool's state.

  The contract achieves this by enforcing the following rules:
  
  1. Restricted Field Modifications:
     - Only the fields `royaltyX`, `royaltyY`, and `nonce` can be modified.
     - The values of `royaltyX` and `royaltyY` must not exceed their previous values
       and cannot fall below zero.
     - The `nonce` must be incremented, ensuring each withdrawal is unique.

  2. Withdrawal Limits:
     - The requested withdrawal amounts for tokens X and Y must not exceed the current
       `royaltyX` and `royaltyY` values stored in the pool's datum. This prevents
       over-withdrawal and maintains the integrity of the royalty funds.

  3. Input Validation:
     - A UTXO associated with the User Royalty Withdrawal Contract must be present
       in the transaction input set, ensuring that the request is linked to a legitimate
       withdrawal process. This also protects against attempts to redirect royalties to
       unauthorized addresses by copying the original request structure.

  4. Consistency Checks:
     - The token values' differences before and after the transaction must match the
       requested withdrawal amounts, ensuring that the changes in the pool state are
       accurate and reflect the withdrawal.

  5. Address Integrity:
     - The pool address must remain unchanged throughout the process, guaranteeing that
       the withdrawal request does not inadvertently or maliciously alter the destination.

  6. Signature Verification:
     - A set of signatures must satisfy the n-of-m threshold requirement, verifying
       that the withdrawal request has been approved by the appropriate parties.

  7. Operation Data Representation:
     - The `operationRelatedData` is represented by the `WithdrawRoyaltyDataToSign` structure,
       which contains the necessary information for validating the transaction.
-}

royaltyWithdrawPoolValidatorT :: ClosedTerm (RoyaltyWithdrawRedeemer :--> PScriptContext :--> PBool)
royaltyWithdrawPoolValidatorT = plam $ \redeemer' ctx' -> unTermCont $ do
    ctx      <- pletFieldsC @'["txInfo", "purpose"] ctx'
    redeemer <- pletFieldsC @'["orderInIx", "poolInIx"] redeemer'

    PRewarding _ <- tmatch (pfromData $ getField @"purpose" ctx)

    txInfo  <- pletFieldsC @'["inputs", "outputs"] $ getField @"txInfo" ctx
    inputs  <- tletUnwrap $ getField @"inputs"  txInfo
    outputs <- tletUnwrap $ getField @"outputs" txInfo
    let
        orderInIx   = getField @"orderInIx"   redeemer
        poolInIx    = getField @"poolInIx"    redeemer

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
        signature       = getField @"signature"    conf
        withdrawDataRaw = getField @"withdrawData" conf
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
    inputDatum  <- tlet $ extractPoolConfigFromDatum parsedPoolInput
    prevConfig  <- pletFieldsC @'["poolNft", "poolX", "poolY", "poolLq", "feeNum", "treasuryFee", "royaltyFee", "treasuryX", "treasuryY", "royaltyX", "royaltyY", "DAOPolicy", "treasuryAddress", "royaltyPubKey", "nonce"] inputDatum
    let
        {-|
            -- To ensure the immutability of the pool datum, all fields should be extracted from the input pool datum, 
            -- except for the fields related to royalties and the nonce, which are allowed to change. 
            -- The `royaltyX` and `royaltyY` fields will be utilized to verify the correctness of the withdrawal process.
        -}
        prevPoolNft           = getField @"poolNft" prevConfig  
        prevPoolX             = getField @"poolX" prevConfig
        prevPoolY             = getField @"poolY" prevConfig
        prevPoolLq            = getField @"poolLq" prevConfig
        prevFeeNum            = getField @"feeNum" prevConfig
        prevTreasuryFeeNum    = getField @"treasuryFee" prevConfig
        prevRoyaltyFeeNum     = getField @"royaltyFee" prevConfig
        prevTreasuryX         = getField @"treasuryX" prevConfig
        prevTreasuryY         = getField @"treasuryY" prevConfig
        prevRoyaltyX          = getField @"royaltyX" prevConfig
        prevRoyaltyY          = getField @"royaltyY" prevConfig
        prevDAOPolicy         = getField @"DAOPolicy" prevConfig
        prevTreasuryAddress   = getField @"treasuryAddress" prevConfig
        prevRoyaltyPubKey     = getField @"royaltyPubKey" prevConfig
        prevNonce             = getField @"nonce" prevConfig

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

        correctLovelaceToken2Token = prevLovelaceToken2Token #== newLovelaceToken2Token

        -- Signature verification
    dataToSign <- 
        tcon $ (WithdrawRoyaltyDataToSign $
            pdcons @"withdrawData" @WithdrawData # pdata withdrawData'
                #$ pdcons @"poolNonce" @PInteger # pdata prevNonce
                    # pdnil) 

    let                    
        dataToSignRaw = (additionalBytes <> pserialiseData # (punsafeCoerce dataToSign))

        signatureIsCorrect = pverifyEd25519Signature # prevRoyaltyPubKey # dataToSignRaw # signature

        -- Tx should contains only two inputs: pool, withdraw request
        strictInputs = (plength # inputs) #== 2
    
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
                #$ pdcons @"royaltyPubKey" @PByteString # pdata prevRoyaltyPubKey
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
