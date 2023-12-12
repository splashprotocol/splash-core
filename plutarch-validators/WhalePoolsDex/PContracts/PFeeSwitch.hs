module WhalePoolsDex.PContracts.PFeeSwitch where

import WhalePoolsDex.PContracts.PApi (tletUnwrap, containsSignature, treasuryFeeNumLowerLimit, treasuryFeeNumUpperLimit, poolFeeNumUpperLimit, poolFeeNumLowerLimit)
import PExtra.API (assetClassValueOf, ptryFromData, PAssetClass(..))
import PExtra.Monadic
import Plutarch
import Plutarch.Api.V2 
import Plutarch.Api.V1 (PCredential(..))
import Plutarch.DataRepr
import Plutarch.Prelude
import Plutarch.Extra.TermCont
import Plutarch.Builtin             (pasInt, pforgetData, PIsData(..))
import Plutarch.Unsafe              (punsafeCoerce)
import Plutarch.Internal.PlutusType (pcon', pmatch')
import WhalePoolsDex.PContracts.PPool
import Plutarch.Api.V1.Scripts (PValidatorHash)
import Plutarch.Trace
import Plutarch.Extra.TermCont

extractPoolConfig :: Term s (PTxOut :--> PoolConfig)
extractPoolConfig = plam $ \txOut -> unTermCont $ do
  txOutDatum <- tletField @"datum" txOut

  POutputDatum txOutOutputDatum <- pmatchC txOutDatum

  rawDatum <- tletField @"outputDatum" txOutOutputDatum

  PDatum poolDatum <- pmatchC rawDatum

  tletUnwrap $ ptryFromData @(PoolConfig) $ poolDatum

findOutput :: Term s (PValidatorHash :--> PBuiltinList PTxOut :--> PTxOut)
findOutput =
    phoistAcyclic $
        plam $ \credToFind ->
            precList
                ( \self x xs -> unTermCont $ do
                    addr <- tletField @"address" x
                    pure $ pmatch (pfromData $ pfield @"credential" # addr) $ \case
                      PScriptCredential sc -> unTermCont $ do
                          let hashInOutput = pfromData $ pfield @"_0" # sc
                          pure $ pif (hashInOutput #== credToFind) x (self # xs)
                      _ -> unTermCont $ pure (self # xs)
                )
                (const $ ptraceError "User output not found")

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

-- All SwitchFee actions shouldn't modify main poolConfig elements: poolNft, poolX, poolY, poolLq, lqBound, feeNum
validateCommonFields :: PMemberFields PoolConfig '["poolNft", "poolX", "poolY", "poolLq", "lqBound"] s as => HRec as -> HRec as -> Term s PBool
validateCommonFields prevConfig newConfig =
  let
    prevPoolNft = getField @"poolNft" prevConfig
    prevPoolX   = getField @"poolX"   prevConfig
    prevPoolY   = getField @"poolY"   prevConfig
    prevPoolLq  = getField @"poolLq"  prevConfig
    prevLqBound = getField @"lqBound"  prevConfig

    newPoolNft  = getField @"poolNft" newConfig
    newPoolX    = getField @"poolX"   newConfig
    newPoolY    = getField @"poolY"   newConfig
    newPoolLq   = getField @"poolLq"  newConfig
    newLqBound  = getField @"lqBound" newConfig

    commonFieldsValid = 
      prevPoolNft    #== newPoolNft  #&&
      prevPoolX      #== newPoolX    #&&
      prevPoolY      #== newPoolY    #&&
      prevPoolLq     #== newPoolLq   #&&
      prevLqBound    #== newLqBound

  in commonFieldsValid

-- Validates that treasuryX, treasuryY fields from poolConfig hadn't be modified
treasuryIsTheSame :: PMemberFields PoolConfig '["treasuryX", "treasuryY"] s as => HRec as -> HRec as -> Term s PBool
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
  :: PMemberFields PoolConfig '["treasuryX", "treasuryY", "poolX", "poolY", "poolLq", "treasuryAddress"] s as 
  => HRec as 
  -> HRec as 
  -> Term s (PBuiltinList PTxOut :--> PValue _ _ :--> PValue _ _ :--> PBool)
validateTreasuryWithdraw prevConfig newConfig = plam $ \ outputs prevPoolValue newPoolValue -> unTermCont $ do
  let
    poolX  = getField @"poolX"  prevConfig
    poolY  = getField @"poolY"  prevConfig
    poolLq = getField @"poolLq" prevConfig

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

    prevPoolXValue   = assetClassValueOf # prevPoolValue # poolX
    prevPoolYValue   = assetClassValueOf # prevPoolValue # poolY
    prevPoolLqValue  = assetClassValueOf # prevPoolValue # poolLq

    newPoolXValue   = assetClassValueOf # newPoolValue # poolX
    newPoolYValue   = assetClassValueOf # newPoolValue # poolY
    newPoolLqValue  = assetClassValueOf # newPoolValue # poolLq

    -- xDiffInValue, yDiffInValue, xDiffInDatum and yDiffInDatum will be negative values, because we are withdrawing treasury  
    xDiffInValue = newPoolXValue - prevPoolXValue
    yDiffInValue = newPoolYValue - prevPoolYValue

    xDiffInDatum = (pfromData newTreasuryX) - (pfromData prevTreasuryX)
    yDiffInDatum = (pfromData newTreasuryY) - (pfromData prevTreasuryY)

    correctPoolDiff = prevPoolLqValue #== newPoolLqValue #&& xDiffInValue #== xDiffInDatum #&& yDiffInValue #== yDiffInDatum

    correctTreasuryWithdraw = xValueInTreasury #== (negate xDiffInDatum) #&& yValueInTreasury #== (negate yDiffInDatum)

    treasuryAddrIsTheSame = prevTreasuryAddress #== newTreasuryAddress

  pure $ correctPoolDiff #&& correctTreasuryWithdraw #&& treasuryAddrIsTheSame

daoMultisigPolicyValidatorT :: Term s PAssetClass -> Term s (PBuiltinList PPubKeyHash) -> Term s PInteger -> Term s ((PTuple DAOAction PInteger) :--> PScriptContext :--> PBool)
daoMultisigPolicyValidatorT poolNft adminsPkhs threshold = plam $ \redeemer ctx -> unTermCont $ do
  let  
    action     = pfromData $ pfield @"_0" # redeemer
    poolInIdx  = pfromData $ pfield @"_1" # redeemer

  txinfo' <- tletField @"txInfo" ctx

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

  successor       <- tlet $ findPoolOutput # poolNft # outputs
  poolOutputDatum <- tlet $ extractPoolConfig # successor
  poolOutputValue <- tletField @"value" successor

  poolInputAddr  <- tletField @"address" poolInputResolved
  poolOutputAddr <- tletField @"address" successor

  prevConf <- pletFieldsC @'["poolNft", "poolX", "poolY", "poolLq", "feeNumX", "feeNumY", "treasuryFee", "treasuryX", "treasuryY", "DAOPolicy", "lqBound", "treasuryAddress"] poolInputDatum
  newConf  <- pletFieldsC @'["poolNft", "poolX", "poolY", "poolLq", "feeNumX", "feeNumY", "treasuryFee", "treasuryX", "treasuryY", "DAOPolicy", "lqBound", "treasuryAddress"] poolOutputDatum
  let
    validSignaturesQty =
      pfoldl # plam (\acc pkh -> pif (containsSignature # signatories # pkh) (acc + 1) acc) # 0 # adminsPkhs
  
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
    updatedPoolFeeNumIsCorrect = 
      pdelay (
        (newPoolFeeNumX #<= poolFeeNumUpperLimit #&& poolFeeNumLowerLimit #<= newPoolFeeNumX) #&&
        (newPoolFeeNumY #<= poolFeeNumUpperLimit #&& poolFeeNumLowerLimit #<= newPoolFeeNumY)
      )
    
    -- Checks that correct qty of singers present in transaction
    validThreshold = threshold #<= validSignaturesQty

    -- Checks that main pool fields: tokenX, tokenY, tokenLq, tokenNft, feeNum hadn't modified
    validCommonFields = validateCommonFields prevConf newConf

    -- Checks that pool value and address hadn't modified
    poolValueAndAddressAreTheSameDelayed = pdelay (poolInputValue #== poolOutputValue #&& poolInputAddr #== poolOutputAddr)

    -- Checks that treasury address is the same
    treasuryAddressIsTheSame = pdelay (prevTreasuryAddress #== newTreasuryAddress)

    -- Checks that treasury fee is the same
    treasuryFeeIsTheSame = pdelay (prevTreasuryFee #== newTreasuryFee)

    -- Checks that pool fee is the same
    poolFeeIsTheSame = 
      pdelay (prevPoolFeeNumX #== newPoolFeeNumX #&& prevPoolFeeNumY #== newPoolFeeNumY)

    -- Checks that dao policy is the same
    daoPolicyIsTheSame = pdelay (prevDAOPolicy #== newDAOPolicy)

    -- Checks that pool valuse is the same
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
        (validateTreasuryWithdraw prevConf newConf) # outputs # poolInputValue # poolOutputValue #&&
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
        pforce poolValueAndAddressAreTheSameDelayed #&&
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
        pforce poolValueAndAddressAreTheSameDelayed #&&
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
        pforce poolValueAndAddressAreTheSameDelayed #&&
        pforce poolFeeIsTheSame
      
      -- In case of changing DAO admin we should verify next conditions:
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
        pforce treasuryFeeIsTheSame #&&
        treasuryIsTheSame prevConf newConf #&&
        pforce treasuryAddressIsTheSame #&&
        pforce daoPolicyIsTheSame #&&
        pforce poolValueAndAddressAreTheSameDelayed #&&
        pforce updatedPoolFeeNumIsCorrect

  pure $ validCommonFields #&& validThreshold #&& validAction
