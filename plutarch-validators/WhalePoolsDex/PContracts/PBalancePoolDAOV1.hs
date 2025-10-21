module WhalePoolsDex.PContracts.PBalancePoolDAOV1 where

import qualified GHC.Generics as GHC

import WhalePoolsDex.PContracts.PApi (tletUnwrap, containsSignature, treasuryFeeNumLowerLimit, treasuryFeeNumUpperLimit, poolFeeNumUpperLimit, poolFeeNumLowerLimit)
import PExtra.API (assetClassValueOf, ptryFromData, PAssetClass(..))
import PExtra.Monadic
import Plutarch
import Plutarch.Api.V2 
import Plutarch.Api.V1 (PCredential(..))
import Plutarch.DataRepr
import Plutarch.Prelude
import Plutarch.Extra.TermCont
import Plutarch.Builtin             (pasInt, pforgetData, PIsData(..), ppairDataBuiltin, pserialiseData)
import Plutarch.Unsafe              (punsafeCoerce)
import Plutarch.Internal.PlutusType (pcon', pmatch')
import Plutarch.Crypto              (pverifyEd25519Signature)
import Plutarch.Num                 ((#+))

import WhalePoolsDex.PContracts.PBalancePool 
import WhalePoolsDex.PContracts.PPoolDAOV1   (findOutput)
import WhalePoolsDex.PContracts.PPool        (findPoolOutput)
import Plutarch.Api.V1.Scripts               (PValidatorHash)
import Plutarch.Trace
import Plutarch.Extra.TermCont
import PExtra.PTriple

extractBalancePoolConfig :: Term s (PTxOut :--> BalancePoolConfig)
extractBalancePoolConfig = plam $ \txOut -> unTermCont $ do
  txOutDatum <- tletField @"datum" txOut

  POutputDatum txOutOutputDatum <- pmatchC txOutDatum

  rawDatum <- tletField @"outputDatum" txOutOutputDatum

  PDatum poolDatum <- pmatchC rawDatum

  tletUnwrap $ ptryFromData @(BalancePoolConfig) $ poolDatum

data DAOAction (s :: S) = WithdrawTreasury | ChangeStakePart | ChangeTreasuryFee | ChangeTreasuryAddress | ChangeAdminAddress | ChangePoolFee

instance PIsData DAOAction where
    pfromDataImpl tx =
        let x = pasInt # pforgetData tx
         in pmatch' x pcon
    pdataImpl x = pmatch x (punsafeCoerce . pdata . pcon')

instance PlutusType DAOAction where
    type PInner DAOAction = PInteger

    pcon' WithdrawTreasury = 0
    pcon' ChangeStakePart = 1
    pcon' ChangeTreasuryFee = 2
    pcon' ChangeTreasuryAddress = 3
    pcon' ChangeAdminAddress = 4
    pcon' ChangePoolFee = 5

    pmatch' x f =
        pif
            (x #== 0)
            (f WithdrawTreasury)
            ( pif
                (x #== 1)
                (f ChangeStakePart)
                ( pif
                    (x #== 2)
                    (f ChangeTreasuryFee)
                    ( pif 
                        (x #== 3) 
                        (f ChangeTreasuryAddress)
                        (pif (x #== 4) (f ChangeAdminAddress) (f ChangePoolFee))
                    )
                )
            )

newtype DAOV1Redeemer (s :: S)
    = DAOV1Redeemer
        ( Term
            s
            ( PDataRecord
                '[ "action"     ':= DAOAction      
                 , "poolInIdx"  ':= PInteger
                 , "poolNft"    ':= PAssetClass
                 , "signatures" ':= PBuiltinList (PAsData PByteString)
                 , "additionalBytes" ':= PBuiltinList (PAsData PByteString)
                 ]
            )
        )
    deriving stock (GHC.Generic)
    deriving
        (PIsData, PDataFields, PlutusType)

instance DerivePlutusType DAOV1Redeemer where type DPTStrat _ = PlutusTypeData

newtype DAOV1RequestDataToSign (s :: S)
    = DAOV1RequestDataToSign
        ( Term
            s
            ( PDataRecord
                '[ "daoAction"           ':= DAOAction
                 , "poolNft"             ':= PAssetClass
                 , "poolFee"             ':= PInteger
                 , "treasuryFee"         ':= PInteger
                 , "DAOPolicy"           ':= PBuiltinList (PAsData PStakingCredential)
                 , "poolAddress"         ':= PAddress
                 , "treasuryAddress"     ':= PValidatorHash
                 , "treasuryXWithdraw"   ':= PInteger
                 , "treasuryYWithdraw"   ':= PInteger
                 , "nonce"               ':= PInteger
                 ]
            )
        )
    deriving stock (GHC.Generic)
    deriving
        (PIsData, PDataFields, PlutusType, PEq)

instance DerivePlutusType DAOV1RequestDataToSign where type DPTStrat _ = PlutusTypeData

-- All SwitchFee actions shouldn't modify main poolConfig elements: poolNft, poolX, poolY, poolLq, feeNum
validateCommonFieldsAndSiganture :: PMemberFields BalancePoolConfig '["poolNft", "poolX", "poolY", "poolLq", "treasuryX", "treasuryY", "nonce"] s as 
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
validateCommonFieldsAndSiganture prevConfig newConfig = plam $ \publicKeys signatures additionalBytesList threshold newPoolFee newTreasuryFee newAdminAddress newPoolAddress newTreasuryAddress action -> unTermCont $ do
  let
    prevPoolNft = getField @"poolNft" prevConfig
    prevPoolX   = getField @"poolX"   prevConfig
    prevPoolY   = getField @"poolY"   prevConfig
    prevPoolLq  = getField @"poolLq"  prevConfig
    prevPoolTreasuryX = getField @"treasuryX" prevConfig
    prevPoolTreasuryY = getField @"treasuryY" prevConfig
    prevNonce = pfromData $ getField @"nonce" prevConfig

    newPoolNft  = getField @"poolNft" newConfig
    newPoolX    = getField @"poolX"   newConfig
    newPoolY    = getField @"poolY"   newConfig
    newPoolLq   = getField @"poolLq"  newConfig
    newPoolTreasuryX = getField @"treasuryX" newConfig
    newPoolTreasuryY = getField @"treasuryY" newConfig
    newNonce = pfromData $ getField @"nonce" newConfig

    commonFieldsValid = 
      prevPoolNft    #== newPoolNft  #&&
      prevPoolX      #== newPoolX    #&&
      prevPoolY      #== newPoolY    #&&
      prevPoolLq     #== newPoolLq   #&&
      (prevNonce #+ 1) #== newNonce

  requestData <-
    tcon $ (DAOV1RequestDataToSign $
        pdcons @"daoAction" @DAOAction # pdata action
            #$ pdcons @"poolNft" @PAssetClass # pdata newPoolNft
            #$ pdcons @"poolFee" @PInteger # pdata newPoolFee
            #$ pdcons @"treasuryFee" @PInteger # pdata newTreasuryFee
            #$ pdcons @"DAOPolicy" @(PBuiltinList (PAsData PStakingCredential)) # pdata newAdminAddress
            #$ pdcons @"poolAddress" @PAddress # pdata newPoolAddress
            #$ pdcons @"treasuryAddress" @PValidatorHash # pdata newTreasuryAddress
            #$ pdcons @"treasuryXWithdraw" @PInteger # pdata (newPoolTreasuryX - prevPoolTreasuryX)
            #$ pdcons @"treasuryYWithdraw" @PInteger # pdata (newPoolTreasuryY - prevPoolTreasuryY)
            #$ pdcons @"nonce" @PInteger # pdata prevNonce
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

-- Validates that treasuryX, treasuryY fields from poolConfig hadn't be modified
treasuryIsTheSame :: PMemberFields BalancePoolConfig '["treasuryX", "treasuryY"] s as => HRec as -> HRec as -> Term s PBool
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
  :: PMemberFields BalancePoolConfig '["treasuryX", "treasuryY", "poolX", "poolY", "poolLq", "treasuryAddress"] s as 
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

    newPoolXValue   = assetClassValueOf # newPoolValue # poolX
    newPoolYValue   = assetClassValueOf # newPoolValue # poolY
    newPoolLqValue  = assetClassValueOf # newPoolValue # poolLq

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

  pure $ correctPoolDiff #&& correctTreasuryWithdraw #&& treasuryAddrIsTheSame #&& (nftQtyInPrevValue #== 1) #&& validFinalTreasuryXValue #&& validFinalTreasuryYValue

daoMultisigPolicyValidatorT :: Term s (PBuiltinList PByteString) -> Term s PInteger -> Term s PBool -> Term s (DAOV1Redeemer :--> PScriptContext :--> PBool)
daoMultisigPolicyValidatorT daoPhs threshold lpFeeIsEditable = plam $ \redeemer' ctx' -> unTermCont $ do
  redeemer <- pletFieldsC @'["action", "poolInIdx", "poolNft", "signatures", "additionalBytes"] redeemer'
  let
    action     = getField @"action"     redeemer
    poolInIdx  = getField @"poolInIdx"  redeemer
    poolNft    = getField @"poolNft"    redeemer
    signatures = getField @"signatures" redeemer
    additionalBytes = getField @"additionalBytes" redeemer

  ctx <- pletFieldsC @'["txInfo", "purpose"] ctx'

  PRewarding _ <- pmatchC $ getField @"purpose" ctx
  let txinfo' = getField @"txInfo" ctx

  txInfo  <- pletFieldsC @'["inputs", "outputs", "signatories"] txinfo'
  inputs  <- tletUnwrap $ getField @"inputs" txInfo
  outputs <- tletUnwrap $ getField @"outputs" txInfo

  signatories <- tletUnwrap $ getField @"signatories" txInfo

  poolInput' <- tlet $ pelemAt # poolInIdx # inputs
  poolInput  <- pletFieldsC @'["outRef", "resolved"] poolInput'
  let
    poolInputResolved = getField @"resolved" poolInput

  poolInputValue <- tletField @"value" poolInputResolved
  poolInputDatum <- tlet $ extractBalancePoolConfig # poolInputResolved

  successor       <- tlet $ findPoolOutput # poolNft # outputs
  poolOutputDatum <- tlet $ extractBalancePoolConfig # successor
  poolOutputValue <- tletField @"value" successor

  poolInputAddr  <- tletField @"address" poolInputResolved
  poolOutputAddr <- tletField @"address" successor

  prevConf <- pletFieldsC @'["poolNft", "poolX", "poolY", "poolLq", "feeNum", "treasuryFee", "treasuryX", "treasuryY", "DAOPolicy", "treasuryAddress", "nonce"] poolInputDatum
  newConf  <- pletFieldsC @'["poolNft", "poolX", "poolY", "poolLq", "feeNum", "treasuryFee", "treasuryX", "treasuryY", "DAOPolicy", "treasuryAddress", "nonce"] poolOutputDatum
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
    
    -- Checks that main pool properties: tokenX, tokenY, tokenLq, tokenNft, feeNum aren't modified
    validCommonFieldsAndSigantures = validateCommonFieldsAndSiganture prevConf newConf # daoPhs # signatures # additionalBytes # threshold # newPoolFeeNum # newTreasuryFee # newDAOPolicy # poolOutputAddr # newTreasuryAddress # action

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
        (validateTreasuryWithdraw prevConf newConf) # outputs # poolInputValue # poolOutputValue # poolNft #&&
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

  pure $ validCommonFieldsAndSigantures #&& validAction