module WhalePoolsDex.PContracts.PDoubleRoyaltyDAOV1 (
  doubleRoyaltyDaoMultisigPolicyValidatorT
) where

import qualified GHC.Generics as GHC

import WhalePoolsDex.PContracts.PApi         (tletUnwrap, treasuryFeeNumLowerLimit, treasuryFeeNumUpperLimit, poolFeeNumUpperLimit, poolFeeNumLowerLimit)
import WhalePoolsDex.PContracts.PFeeSwitch   (findOutput)
import WhalePoolsDex.PContracts.PDoubleRoyaltyPool
import WhalePoolsDex.PContracts.PRoyaltyDAOV1ActionOrder
import WhalePoolsDex.PContracts.PRoyaltyDAOV1

import PExtra.Monadic
import PExtra.Pair
import PExtra.Ada
import Plutarch
import Plutarch.Api.V2 
import Plutarch.DataRepr
import Plutarch.Prelude
import Plutarch.Extra.TermCont
import Plutarch.Api.V1.Scripts      (PValidatorHash)
import PExtra.API                   (assetClassValueOf, PAssetClass(..), pValueLength)
import Plutarch.Builtin             (pserialiseData, ppairDataBuiltin)
import Plutarch.Crypto              (pverifyEd25519Signature)
import Plutarch.Num                 ((#+))
import Plutarch.Unsafe              (punsafeCoerce)
import Plutarch.Api.V1.Value        (pisAdaOnlyValue)
import Plutarch.Trace

{-|
  DAO Multisig Policy Contract for Double Royalty Pool (v2)

  This contract validates DAO-level actions on a liquidity pool that supports two royalty slots (Double Royalty Pool).
  It implements multisignature authorization for sensitive actions that modify pool configuration or withdraw funds.

  The contract enforces the following core principles:

  1. Field Protection:
     - The following poolConfig fields must remain unchanged unless explicitly allowed by the action:
         poolNft, poolX, poolY, poolLq,
         firstRoyaltyFee, secondRoyaltyFee,
         firstRoyaltyPubKey, secondRoyaltyPubKey,
         firstRoyaltyX, firstRoyaltyY, secondRoyaltyX, secondRoyaltyY.

  2. Action-specific Validation:
     - Each DAO action type defines which poolConfig fields are allowed to change:
         * WithdrawTreasury: allows treasury token withdrawal (treasuryX/treasuryY), other fields must be unchanged.
         * ChangeStakePart: allows changing the stake credential of the pool address, other fields must be unchanged.
         * ChangeTreasuryFee: allows changing treasuryFee within protocol bounds.
         * ChangeTreasuryAddress: allows changing treasuryAddress.
         * ChangeAdminAddress: allows changing DAOPolicy (admin address).
         * ChangePoolFee: allows changing feeNum within protocol bounds (if lpFeeIsEditable is enabled).

  3. Multisignature Verification:
     - The action must be signed by a sufficient number of DAO signers:
         * Signatures are validated against serialized action data including current poolNonce.
         * The number of valid signatures must meet or exceed the provided threshold.

  4. Transaction Shape Requirements:
     - The transaction must include exactly two inputs: the pool UTXO and a fee-covering UTXO (ADA-only).
     - The poolNft must be preserved in the output pool UTXO.
     - The pool value and address must remain consistent unless explicitly allowed by the action.

  This contract ensures that DAO-driven governance of the pool is both controlled and transparent,
  while fully supporting the dual-slot royalty structure of the Double Royalty Pool (v2).
-}

-- All actions must not modify the following main poolConfig elements:
--   poolNft, poolX, poolY, poolLq,
--   firstRoyaltyFee, secondRoyaltyFee,
--   firstRoyaltyPubKey, secondRoyaltyPubKey,
--   firstRoyaltyX, firstRoyaltyY, secondRoyaltyX, secondRoyaltyY
validateCommonFieldsAndSignatures 
  :: PMemberFields DoubleRoyaltyPoolConfig '["poolNft", "poolX", "poolY", "poolLq", "firstRoyaltyFee", "secondRoyaltyFee", "treasuryX", "treasuryY", "firstRoyaltyX", "firstRoyaltyY", "secondRoyaltyX", "secondRoyaltyY", "firstRoyaltyPubKey", "secondRoyaltyPubKey", "nonce"] s as
  => HRec as 
  -> HRec as 
  -> Term s 
    (    PBuiltinList PByteString 
    :--> PBuiltinList (PAsData PByteString)
    :--> PBuiltinList (PAsData PByteString)
    :--> PInteger
    :--> PInteger
    :--> PInteger
    :--> PBuiltinList (PAsData PStakingCredential)
    :--> PAddress
    :--> PValidatorHash
    :--> DAOAction
    :--> PBool
    )
validateCommonFieldsAndSignatures prevConfig newConfig = plam $ \publicKeys signatures additionalBytesList threshold newPoolFee newTreasuryFee newAdminAddress newPoolAddress newTreasuryAddress action -> unTermCont $ do
  let
    prevPoolNft = getField @"poolNft" prevConfig
    prevPoolX   = getField @"poolX"   prevConfig
    prevPoolY   = getField @"poolY"   prevConfig
    prevPoolLq  = getField @"poolLq"  prevConfig
    prevPoolTreasuryX = getField @"treasuryX" prevConfig
    prevPoolTreasuryY = getField @"treasuryY" prevConfig
    prevPoolFirstRoyaltyX     = getField @"firstRoyaltyX"       prevConfig
    prevPoolFirstRoyaltyY     = getField @"firstRoyaltyY"       prevConfig
    prevPoolSecondRoyaltyX    = getField @"secondRoyaltyX"      prevConfig
    prevPoolSecondRoyaltyY    = getField @"secondRoyaltyY"      prevConfig
    prevPoolFirstRoyaltyFee   = getField @"firstRoyaltyFee"     prevConfig
    prevPoolSecondRoyaltyFee  = getField @"secondRoyaltyFee"    prevConfig
    prevPoolRoyaltyFirstAddr  = getField @"firstRoyaltyPubKey"  prevConfig
    prevPoolRoyaltySecondAddr = getField @"secondRoyaltyPubKey" prevConfig
    prevPoolNonce = pfromData $ getField @"nonce"               prevConfig

    newPoolNft  = getField @"poolNft" newConfig
    newPoolX    = getField @"poolX"   newConfig
    newPoolY    = getField @"poolY"   newConfig
    newPoolLq   = getField @"poolLq"  newConfig
    newPoolTreasuryX = getField @"treasuryX" newConfig
    newPoolTreasuryY = getField @"treasuryY" newConfig
    newPoolFirstRoyaltyX     = getField @"firstRoyaltyX"       newConfig
    newPoolFirstRoyaltyY     = getField @"firstRoyaltyY"       newConfig
    newPoolSecondRoyaltyX    = getField @"secondRoyaltyX"      newConfig
    newPoolSecondRoyaltyY    = getField @"secondRoyaltyY"      newConfig
    newPoolFirstRoyaltyFee   = getField @"firstRoyaltyFee"     newConfig
    newPoolSecondRoyaltyFee  = getField @"secondRoyaltyFee"    newConfig
    newPoolRoyaltyFirstAddr  = getField @"firstRoyaltyPubKey"  newConfig
    newPoolRoyaltySecondAddr = getField @"secondRoyaltyPubKey" newConfig
    newPoolNonce = pfromData $ getField @"nonce" newConfig

    commonFieldsValid = 
      prevPoolNft    #== newPoolNft  #&&
      prevPoolX      #== newPoolX    #&&
      prevPoolY      #== newPoolY    #&&
      prevPoolLq     #== newPoolLq   #&&
      prevPoolFirstRoyaltyX     #== newPoolFirstRoyaltyX      #&&
      prevPoolFirstRoyaltyY     #== newPoolFirstRoyaltyY      #&&
      prevPoolSecondRoyaltyX    #== newPoolSecondRoyaltyX      #&&
      prevPoolSecondRoyaltyY    #== newPoolSecondRoyaltyY      #&&
      prevPoolFirstRoyaltyFee   #== newPoolFirstRoyaltyFee    #&&
      prevPoolSecondRoyaltyFee  #== newPoolSecondRoyaltyFee    #&&
      prevPoolRoyaltyFirstAddr  #== newPoolRoyaltyFirstAddr   #&&
      prevPoolRoyaltySecondAddr #== newPoolRoyaltySecondAddr   #&&
      (prevPoolNonce #+ 1) #== newPoolNonce

  requestData <-
    tcon $ (DAOV1RequestDataToSign $
        pdcons @"daoAction" @DAOAction # pdata action
            #$ pdcons @"poolNft" @PAssetClass # pdata newPoolNft
            #$ pdcons @"poolFee" @PInteger # pdata newPoolFee
            #$ pdcons @"treasuryFee" @PInteger # pdata newTreasuryFee
            #$ pdcons @"adminAddress" @(PBuiltinList (PAsData PStakingCredential)) # pdata newAdminAddress
            #$ pdcons @"poolAddress" @PAddress # pdata newPoolAddress
            #$ pdcons @"treasuryAddress" @PValidatorHash # pdata newTreasuryAddress
            #$ pdcons @"treasuryXWithdraw" @PInteger # pdata (newPoolTreasuryX - prevPoolTreasuryX)
            #$ pdcons @"treasuryYWithdraw" @PInteger # pdata (newPoolTreasuryY - prevPoolTreasuryY)
            #$ pdcons @"nonce" @PInteger # pdata prevPoolNonce
                # pdnil)
  
  let
    dataToSignRaw = pserialiseData # (punsafeCoerce requestData)

    dataToSignList = pmap # plam (\additionalBytes -> (pfromData additionalBytes) <> dataToSignRaw) # additionalBytesList

    signaturesAndPublicKeys = pzipWith # plam (\siganture publicKey -> ppairDataBuiltin # siganture # (pdata publicKey)) # signatures # publicKeys

    verificationResults =
      pzipWith # 
        plam (\signatureAndPublicKey dataToSign ->
          pverifyEd25519Signature # (pfromData (psndBuiltin # signatureAndPublicKey)) # dataToSign # (pfromData (pfstBuiltin # signatureAndPublicKey))
        ) #
        signaturesAndPublicKeys #
        dataToSignList

    validSignaturesQty =
      pfoldl # plam (\acc sigantureIsCorrect -> pif sigantureIsCorrect (acc + 1) acc) # 0 # verificationResults

    validThreshold = threshold #<= validSignaturesQty

  pure $ commonFieldsValid #&& validThreshold

-- Validates that treasuryX and treasuryY fields in poolConfig remain unchanged.
treasuryIsTheSame :: PMemberFields DoubleRoyaltyPoolConfig '["treasuryX", "treasuryY"] s as => HRec as -> HRec as -> Term s PBool
treasuryIsTheSame prevConfig newConfig =
  let
    prevTreasuryX = getField @"treasuryX" prevConfig
    prevTreasuryY = getField @"treasuryY" prevConfig

    newTreasuryX = getField @"treasuryX" newConfig
    newTreasuryY = getField @"treasuryY" newConfig

    commonFieldsValid = 
      prevTreasuryX  #== newTreasuryX  #&&
      prevTreasuryY  #== newTreasuryY 

  in commonFieldsValid

validateTreasuryWithdraw 
  :: PMemberFields DoubleRoyaltyPoolConfig '["treasuryX", "treasuryY", "poolX", "poolY", "poolLq", "treasuryAddress"] s as 
  => HRec as 
  -> HRec as
  -> Term s (PBuiltinList PTxOut :--> PValue _ _ :--> PValue _ _ :--> PAssetClass :--> PBool)
validateTreasuryWithdraw prevConfig newConfig = plam $ \ outputs prevPoolValue newPoolValue poolNft -> unTermCont $ do
  let
    poolX  = getField @"poolX"    prevConfig
    poolY  = getField @"poolY"    prevConfig
    poolLq = getField @"poolLq"   prevConfig

    prevTreasuryX  = getField @"treasuryX" prevConfig
    prevTreasuryY  = getField @"treasuryY" prevConfig
    prevTreasuryAddress = getField @"treasuryAddress" prevConfig

    newTreasuryX  = getField @"treasuryX" newConfig
    newTreasuryY  = getField @"treasuryY" newConfig
    newTreasuryAddress = getField @"treasuryAddress" newConfig

  treasuryBox   <- tlet $ findOutput # prevTreasuryAddress # outputs
  treasuryValue <- tletField @"value" treasuryBox
  let
    xValueInTreasury = assetClassValueOf # treasuryValue # poolX
    yValueInTreasury = assetClassValueOf # treasuryValue # poolY

    nftQtyInPrevValue = assetClassValueOf # prevPoolValue # poolNft

    prevPoolXValue   = assetClassValueOf # prevPoolValue # poolX
    prevPoolYValue   = assetClassValueOf # prevPoolValue # poolY
    prevPoolLqValue  = assetClassValueOf # prevPoolValue # poolLq

    prevLovelaceToken2Token =
      pif
          ((poolX #== pAdaAssetClass) #|| (poolY #== pAdaAssetClass))
          (pconstant 0)
          (assetClassValueOf # prevPoolValue # pAdaAssetClass)

    newPoolXValue   = assetClassValueOf # newPoolValue # poolX
    newPoolYValue   = assetClassValueOf # newPoolValue # poolY
    newPoolLqValue  = assetClassValueOf # newPoolValue # poolLq

    newLovelaceToken2Token =
      pif
          ((poolX #== pAdaAssetClass) #|| (poolY #== pAdaAssetClass))
          (pconstant 0)
          (assetClassValueOf # newPoolValue # pAdaAssetClass)

    -- xDiffInValue, yDiffInValue, xDiffInDatum and yDiffInDatum will be negative values, because we are withdrawing treasury  
    xDiffInValue = newPoolXValue - prevPoolXValue
    yDiffInValue = newPoolYValue - prevPoolYValue

    newTreasuryXValue = pfromData newTreasuryX
    newTreasuryYValue = pfromData newTreasuryY

    xDiffInDatum = newTreasuryXValue - (pfromData prevTreasuryX)
    yDiffInDatum = newTreasuryYValue - (pfromData prevTreasuryY)

    validFinalTreasuryXValue = 0 #<= newTreasuryXValue
    validFinalTreasuryYValue = 0 #<= newTreasuryYValue

    correctPoolDiff = prevPoolLqValue #== newPoolLqValue #&& xDiffInValue #== xDiffInDatum #&& yDiffInValue #== yDiffInDatum

    correctTreasuryWithdraw = xValueInTreasury #== (negate xDiffInDatum) #&& yValueInTreasury #== (negate yDiffInDatum)

    treasuryAddrIsTheSame = prevTreasuryAddress #== newTreasuryAddress

    correctLovelaceToken2Token = prevLovelaceToken2Token #== newLovelaceToken2Token

    selfValueLength     = pValueLength # prevPoolValue
    succesorValueLength = pValueLength # newPoolValue

    correctTokensQty = selfValueLength #== succesorValueLength

  pure $ correctPoolDiff #&& correctTreasuryWithdraw #&& treasuryAddrIsTheSame #&& (nftQtyInPrevValue #== 1) #&& validFinalTreasuryXValue #&& validFinalTreasuryYValue #&& correctLovelaceToken2Token #&& correctTokensQty

doubleRoyaltyDaoMultisigPolicyValidatorT :: Term s (PBuiltinList PByteString) -> Term s PInteger -> Term s PBool -> Term s (DAOV1Redeemer :--> PScriptContext :--> PBool)
doubleRoyaltyDaoMultisigPolicyValidatorT daoPhs threshold lpFeeIsEditable = plam $ \redeemer' ctx' -> unTermCont $ do
  redeemer <- pletFieldsC @'["action", "poolInIdx", "poolOutIdx", "signatures", "additionalBytes"] redeemer'
  let
    action     = getField @"action"     redeemer
    poolInIdx  = getField @"poolInIdx"  redeemer
    poolOutIdx = getField @"poolOutIdx" redeemer
    signatures = getField @"signatures" redeemer
    additionalBytesList = getField @"additionalBytes" redeemer
    -- Utxo to cover tx fee. Tx should contains only 2 inputs: Pool + Fee Utxo
    feeUtxoIdx = 1 - poolInIdx

  ctx <- pletFieldsC @'["txInfo", "purpose"] ctx'

  PRewarding _ <- pmatchC $ getField @"purpose" ctx
  let txinfo' = getField @"txInfo" ctx

  txInfo  <- pletFieldsC @'["inputs", "outputs"] txinfo'
  inputs  <- tletUnwrap $ getField @"inputs" txInfo
  outputs <- tletUnwrap $ getField @"outputs" txInfo

  poolInput' <- tlet $ pelemAt # poolInIdx # inputs
  poolInput  <- pletFieldsC @'["outRef", "resolved"] poolInput'
  let
    poolInputResolved = getField @"resolved" poolInput

  poolInputValue <- tletField @"value" poolInputResolved
  poolInputDatum <- tlet $ extractPoolConfig # poolInputResolved

  feeInput <- tlet $ pelemAt # feeUtxoIdx # inputs
  let
    feeInputResolved = pfromData $ pfield @"resolved" # feeInput

  feeInputValue <- tletField @"value" feeInputResolved

  successor       <- tlet $ pelemAt # poolOutIdx # outputs
  poolOutputDatum <- tlet $ extractPoolConfig # successor
  poolOutputValue <- tletField @"value" successor

  newConf  <- pletFieldsC @'["poolNft", "poolX", "poolY", "poolLq", "feeNum", "treasuryFee", "DAOPolicy", "treasuryAddress", "firstRoyaltyFee", "secondRoyaltyFee", "treasuryX", "treasuryY", "firstRoyaltyX", "firstRoyaltyY", "secondRoyaltyX", "secondRoyaltyY", "firstRoyaltyPubKey", "secondRoyaltyPubKey", "nonce"] poolOutputDatum
  let
    newPoolNft = getField @"poolNft" newConf
    correctPoolNftInNewPool = assetClassValueOf # poolOutputValue # newPoolNft

  poolInputAddr  <- tletField @"address" poolInputResolved
  poolOutputAddr <- tletField @"address" successor

  prevConf <- pletFieldsC @'["poolNft", "poolX", "poolY", "poolLq", "feeNum", "treasuryFee", "DAOPolicy", "treasuryAddress", "firstRoyaltyFee", "secondRoyaltyFee", "treasuryX", "treasuryY", "firstRoyaltyX", "firstRoyaltyY", "secondRoyaltyX", "secondRoyaltyY", "firstRoyaltyPubKey", "secondRoyaltyPubKey", "nonce"] poolInputDatum
  let
    prevDAOPolicy = getField @"DAOPolicy" prevConf
    newDAOPolicy  = getField @"DAOPolicy" newConf
    
    prevTreasuryAddress = getField @"treasuryAddress" prevConf
    newTreasuryAddress  = getField @"treasuryAddress" newConf

    prevTreasuryFee = getField @"treasuryFee" prevConf
    newTreasuryFee  = getField @"treasuryFee" newConf

    prevPoolFeeNum  = getField @"feeNum" prevConf
    newPoolFeeNum   = getField @"feeNum" newConf

    --  |              |  --
    -- \|/ Predicates \|/ --

    -- Checks that new treasury fee value satisfy protocol bounds
    updatedTreasuryFeeIsCorrect = pdelay (newTreasuryFee #<= treasuryFeeNumUpperLimit #&& treasuryFeeNumLowerLimit #<= newTreasuryFee)

    -- Checks that new pool fee num value satisfy protocol bounds
    updatedPoolFeeNumIsCorrect = pdelay (newPoolFeeNum #<= poolFeeNumUpperLimit #&& poolFeeNumLowerLimit #<= newPoolFeeNum)
    -- poolFee treasuryFee adminAddress poolAddress treasuryAddress
    -- Checks that main pool properties: tokenX, tokenY, tokenLq, tokenNft, feeNum aren't modified
    validCommonFieldsAndSignatureThreshold = validateCommonFieldsAndSignatures prevConf newConf # daoPhs # signatures # additionalBytesList # threshold # newPoolFeeNum # newTreasuryFee # newDAOPolicy # poolOutputAddr # newTreasuryAddress # action

    -- Checks that pool value and address aren't modified
    poolValueAndAddressAreTheSame = pdelay (poolInputValue #== poolOutputValue #&& poolInputAddr #== poolOutputAddr)

    -- Checks that treasury address is the same
    treasuryAddressIsTheSame = pdelay (prevTreasuryAddress #== newTreasuryAddress)

    -- Checks that treasury fee is the same
    treasuryFeeIsTheSame = pdelay (prevTreasuryFee #== newTreasuryFee)

    -- Checks that pool fee is the same
    poolFeeIsTheSame = pdelay (prevPoolFeeNum #== newPoolFeeNum)

    -- Checks that dao policy is the same
    daoPolicyIsTheSame = pdelay (prevDAOPolicy #== newDAOPolicy)

    -- Checks that pool values are the same
    poolValueIsTheSame = pdelay (poolInputValue #== poolOutputValue)

    -- Checks that inputs qty is equals `2`
    correctTxInputsQty = (plength # inputs) #== 2

    -- Checks that fee utxo contains only ada
    feeUtxoContainsOnlyAda = pisAdaOnlyValue # feeInputValue

    -- /|\ Predicates /|\ --
    --  |              |  --

    validAction = pmatch action $ \case

      -- In case of treasury withdraw we should verify next conditions:
      --    1) Next fields shouldn't be modified:
      --        * treasuryFee
      --        * DAOPolicy
      --        * treasuryAddress
      --        * poolAddress
      --        * feeNum
      --    2) TreasuryX, TreasuryY be modified, but not more than previous values
      WithdrawTreasury ->
        pforce treasuryFeeIsTheSame #&&
        pforce daoPolicyIsTheSame #&&
        pforce treasuryAddressIsTheSame #&&
        (poolInputAddr #== poolOutputAddr) #&&
        (validateTreasuryWithdraw prevConf newConf) # outputs # poolInputValue # poolOutputValue # newPoolNft #&&
        pforce poolFeeIsTheSame

      -- In case of changing pool staking part we should verify next conditions:
      --    1) Next fields shouldn't be modified:
      --        * treasuryFee
      --        * treasuryX
      --        * treasuryY
      --        * DAOPolicy
      --        * treasuryAddress
      --        * poolValue
      --        * feeNum
      --        * pool address script credential
      --    2) Stake part of pool contract address can be modified
      ChangeStakePart  -> unTermCont $ do
        prevCred <- tletField @"credential" poolInputAddr
        newCred  <- tletField @"credential" poolOutputAddr
        let
          correctAction =
            pforce treasuryFeeIsTheSame #&&
            treasuryIsTheSame prevConf newConf #&& 
            pforce daoPolicyIsTheSame #&&
            pforce treasuryAddressIsTheSame #&&
            pforce poolValueIsTheSame #&&
            (prevCred #== newCred) #&&
            pforce poolFeeIsTheSame

        pure correctAction

      -- In case of changing treasury fee we should verify next conditions:
      --    1) Next fields shouldn't be modified:
      --        * treasuryX
      --        * treasuryY
      --        * DAOPolicy
      --        * treasuryAddress
      --        * poolValue
      --        * feeNum
      --        * pool address
      --    2) Treasury fee can be modified, but not more than poolFee
      ChangeTreasuryFee    ->
        treasuryIsTheSame prevConf newConf #&&
        pforce daoPolicyIsTheSame #&&
        pforce treasuryAddressIsTheSame #&&
        pforce poolValueAndAddressAreTheSame #&&
        pforce poolFeeIsTheSame #&&
        pforce updatedTreasuryFeeIsCorrect

      -- In case of changing treasury address we should verify next conditions:
      --    1) Next fields shouldn't be modified:
      --        * treasuryFee
      --        * treasuryX
      --        * treasuryY
      --        * DAOPolicy
      --        * poolValue
      --        * feeNum
      --        * pool address
      --    2) Treasury address can be modified
      ChangeTreasuryAddress ->
        pforce treasuryFeeIsTheSame #&&
        treasuryIsTheSame prevConf newConf #&&
        pforce daoPolicyIsTheSame #&&
        pforce poolValueAndAddressAreTheSame #&&
        pforce poolFeeIsTheSame

      -- In case of changing DAO admin we should verify next conditions:
      --    1) Next fields shouldn't be modified:
      --        * treasuryFee
      --        * treasuryX
      --        * treasuryY
      --        * treasuryAddress
      --        * feeNum
      --        * poolValue
      --        * pool address script credential
      --    2) DAO policy can be modified
      ChangeAdminAddress ->
        pforce treasuryFeeIsTheSame #&&
        treasuryIsTheSame prevConf newConf #&&
        pforce treasuryAddressIsTheSame #&&
        pforce poolValueAndAddressAreTheSame #&&
        pforce poolFeeIsTheSame
      
      -- In case of changing Pool Fee we should verify next conditions:
      --    1) Next fields shouldn't be modified:
      --        * treasuryFee
      --        * treasuryX
      --        * treasuryY
      --        * treasuryAddress
      --        * DAO policy
      --        * poolValue
      --        * pool address script credential
      --    2) FeeNum can be modified
      ChangePoolFee ->
        lpFeeIsEditable #&&
        pforce treasuryFeeIsTheSame #&&
        treasuryIsTheSame prevConf newConf #&&
        pforce treasuryAddressIsTheSame #&&
        pforce daoPolicyIsTheSame #&&
        pforce poolValueAndAddressAreTheSame #&&
        pforce updatedPoolFeeNumIsCorrect

  pure $ correctTxInputsQty #&& feeUtxoContainsOnlyAda #&& validCommonFieldsAndSignatureThreshold #&& validAction