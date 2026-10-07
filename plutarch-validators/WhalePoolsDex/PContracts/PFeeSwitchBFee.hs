module WhalePoolsDex.PContracts.PFeeSwitchBFee where

import WhalePoolsDex.PContracts.PApi (tletUnwrap, containsSignature, treasuryFeeNumLowerLimit, treasuryFeeNumUpperLimit, poolFeeNumUpperLimit, poolFeeNumLowerLimit, feeDen, zero)
import PExtra.API (assetClassValueOf, ptryFromData, PAssetClass(..), pPreserveOtherAssets)
import PExtra.List (pelemAt)
import PExtra.PTriple (PTuple3)
import PExtra.Monadic
import Plutarch
import Plutarch.Api.V2 
import Plutarch.Api.V1.Value (pisAdaOnlyValue)
import Plutarch.DataRepr
import Plutarch.Prelude
import Plutarch.Extra.TermCont
import WhalePoolsDex.PContracts.PPool (findPoolOutput)
import WhalePoolsDex.PContracts.PPoolBFee (PoolConfig)
import WhalePoolsDex.PContracts.PFeeSwitch (DAOAction(..), findOutput)

extractPoolConfig :: Term s (PTxOut :--> PoolConfig)
extractPoolConfig = plam $ \txOut -> unTermCont $ do
  txOutDatum <- tletField @"datum" txOut
  POutputDatum txOutOutputDatum <- pmatchC txOutDatum
  rawDatum <- tletField @"outputDatum" txOutOutputDatum
  PDatum poolDatum <- pmatchC rawDatum
  tletUnwrap $ ptryFromData @(PoolConfig) $ poolDatum

validateCommonFields :: PMemberFields PoolConfig '["poolNft", "poolX", "poolY", "poolLq", "lqBound"] s as => HRec as -> HRec as -> Term s PBool
validateCommonFields prevConfig newConfig =
  getField @"poolNft" prevConfig #== getField @"poolNft" newConfig #&&
  getField @"poolX" prevConfig #== getField @"poolX" newConfig #&&
  getField @"poolY" prevConfig #== getField @"poolY" newConfig #&&
  getField @"poolLq" prevConfig #== getField @"poolLq" newConfig #&&
  getField @"lqBound" prevConfig #== getField @"lqBound" newConfig

treasuryIsTheSame :: PMemberFields PoolConfig '["treasuryX", "treasuryY"] s as => HRec as -> HRec as -> Term s PBool
treasuryIsTheSame prevConfig newConfig =
  getField @"treasuryX" prevConfig #== getField @"treasuryX" newConfig #&&
  getField @"treasuryY" prevConfig #== getField @"treasuryY" newConfig

validateTreasuryWithdraw
  :: PMemberFields PoolConfig '["treasuryX", "treasuryY", "poolX", "poolY", "poolLq", "treasuryAddress"] s as
  => HRec as
  -> HRec as
  -> Term s (PBuiltinList PTxOut :--> PValue _ _ :--> PValue _ _ :--> PAssetClass :--> PBool)
validateTreasuryWithdraw prevConfig newConfig = plam $ \outputs prevPoolValue newPoolValue poolNft -> unTermCont $ do
  let poolX = getField @"poolX" prevConfig
      poolY = getField @"poolY" prevConfig
      poolLq = getField @"poolLq" prevConfig
      prevTreasuryX = getField @"treasuryX" prevConfig
      prevTreasuryY = getField @"treasuryY" prevConfig
      prevTreasuryAddress = getField @"treasuryAddress" prevConfig
      newTreasuryX = getField @"treasuryX" newConfig
      newTreasuryY = getField @"treasuryY" newConfig
      newTreasuryAddress = getField @"treasuryAddress" newConfig
  treasuryBox <- tlet $ findOutput # prevTreasuryAddress # outputs
  treasuryValue <- tletField @"value" treasuryBox
  let xValueInTreasury = assetClassValueOf # treasuryValue # poolX
      yValueInTreasury = assetClassValueOf # treasuryValue # poolY
      prevPoolXValue = assetClassValueOf # prevPoolValue # poolX
      prevPoolYValue = assetClassValueOf # prevPoolValue # poolY
      prevPoolLqValue = assetClassValueOf # prevPoolValue # poolLq
      newPoolXValue = assetClassValueOf # newPoolValue # poolX
      newPoolYValue = assetClassValueOf # newPoolValue # poolY
      newPoolLqValue = assetClassValueOf # newPoolValue # poolLq
      xDiffInValue = newPoolXValue - prevPoolXValue
      yDiffInValue = newPoolYValue - prevPoolYValue
      newTreasuryXValue = pfromData newTreasuryX
      newTreasuryYValue = pfromData newTreasuryY
      xDiffInDatum = newTreasuryXValue - pfromData prevTreasuryX
      yDiffInDatum = newTreasuryYValue - pfromData prevTreasuryY
      correctPoolDiff = prevPoolLqValue #== newPoolLqValue #&&
                        xDiffInValue #== xDiffInDatum #&&
                        yDiffInValue #== yDiffInDatum
      correctTreasuryWithdraw = xValueInTreasury #== negate xDiffInDatum #&&
                                yValueInTreasury #== negate yDiffInDatum
  pure $ correctPoolDiff #&& correctTreasuryWithdraw #&&
         prevTreasuryAddress #== newTreasuryAddress #&&
         assetClassValueOf # prevPoolValue # poolNft #== 1 #&&
         zero #<= newTreasuryXValue #&& zero #<= newTreasuryYValue #&&
         pPreserveOtherAssets # prevPoolValue # newPoolValue # poolX # poolY # poolLq # poolNft

daoMultisigPolicyValidatorT :: Term s (PBuiltinList PPubKeyHash) -> Term s PInteger -> Term s PBool -> Term s ((PTuple3 DAOAction PInteger PAssetClass) :--> PScriptContext :--> PBool)
daoMultisigPolicyValidatorT daoPkhs threshold lpFeeIsEditable = plam $ \redeemer ctx' -> unTermCont $ do
  let  
    action     = pfromData $ pfield @"_0" # redeemer
    poolInIdx  = pfromData $ pfield @"_1" # redeemer
    poolNft    = pfromData $ pfield @"_2" # redeemer
    feeUtxoIdx = 1 - poolInIdx

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
  poolInputDatum <- tlet $ extractPoolConfig # poolInputResolved

  feeInput <- tlet $ pelemAt # feeUtxoIdx # inputs
  let feeInputResolved = pfromData $ pfield @"resolved" # feeInput
  feeInputValue <- tletField @"value" feeInputResolved

  successor       <- tlet $ findPoolOutput # poolNft # outputs
  poolOutputDatum <- tlet $ extractPoolConfig # successor
  poolOutputValue <- tletField @"value" successor

  poolInputAddr  <- tletField @"address" poolInputResolved
  poolOutputAddr <- tletField @"address" successor

  prevConf <- pletFieldsC @'["poolNft", "poolX", "poolY", "poolLq", "feeNumX", "feeNumY", "treasuryFee", "treasuryX", "treasuryY", "DAOPolicy", "lqBound", "treasuryAddress"] poolInputDatum
  newConf  <- pletFieldsC @'["poolNft", "poolX", "poolY", "poolLq", "feeNumX", "feeNumY", "treasuryFee", "treasuryX", "treasuryY", "DAOPolicy", "lqBound", "treasuryAddress"] poolOutputDatum
  let
    validSignaturesQty =
      pfoldl # plam (\acc pkh -> pif (containsSignature # signatories # pkh) (acc + 1) acc) # 0 # daoPkhs
  
    prevDAOPolicy = getField @"DAOPolicy" prevConf
    newDAOPolicy  = getField @"DAOPolicy" newConf
    
    prevTreasuryAddress = getField @"treasuryAddress" prevConf
    newTreasuryAddress  = getField @"treasuryAddress" newConf

    prevTreasuryFee = getField @"treasuryFee" prevConf
    newTreasuryFee  = getField @"treasuryFee" newConf

    prevPoolFeeNumX = getField @"feeNumX" prevConf
    prevPoolFeeNumY = getField @"feeNumY" prevConf

    newPoolFeeNumX = getField @"feeNumX" newConf
    newPoolFeeNumY = getField @"feeNumY" newConf

    --  |              |  --
    -- \|/ Predicates \|/ --

    -- Checks that new treasury fee value satisfy protocol bounds
    updatedTreasuryFeeIsCorrect = pdelay (newTreasuryFee #<= treasuryFeeNumUpperLimit #&& treasuryFeeNumLowerLimit #<= newTreasuryFee)

    -- Checks that new pool fee num value satisfy protocol bounds
    validFeeConfiguration = zero #< newPoolFeeNumX #&& newPoolFeeNumX #<= feeDen #&& zero #< newPoolFeeNumY #&& newPoolFeeNumY #<= feeDen

    updatedPoolFeeNumIsCorrect = 
      pdelay (
        (newPoolFeeNumX #<= poolFeeNumUpperLimit #&& poolFeeNumLowerLimit #<= newPoolFeeNumX) #&&
        (newPoolFeeNumY #<= poolFeeNumUpperLimit #&& poolFeeNumLowerLimit #<= newPoolFeeNumY)
      )
    
    -- Checks that correct qty of singers present in transaction
    validThreshold = threshold #<= validSignaturesQty

    -- Checks that main pool properties: tokenX, tokenY, tokenLq, tokenNft, feeNum aren't modified
    validCommonFields = validateCommonFields prevConf newConf

    -- Checks that pool value and address aren't modified
    poolValueAndAddressAreTheSame = pdelay (poolInputValue #== poolOutputValue #&& poolInputAddr #== poolOutputAddr)

    -- Checks that treasury address is the same
    treasuryAddressIsTheSame = pdelay (prevTreasuryAddress #== newTreasuryAddress)

    -- Checks that treasury fee is the same
    treasuryFeeIsTheSame = pdelay (prevTreasuryFee #== newTreasuryFee)

    -- Checks that pool fee is the same
    poolFeeIsTheSame = 
      pdelay (prevPoolFeeNumX #== newPoolFeeNumX #&& prevPoolFeeNumY #== newPoolFeeNumY)

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

  pure $ (plength # inputs) #== 2 #&&
         pisAdaOnlyValue # feeInputValue #&&
         validCommonFields #&& validThreshold #&& validFeeConfiguration #&& validAction
