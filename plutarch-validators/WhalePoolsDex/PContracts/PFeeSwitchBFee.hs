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

daoMultisigPolicyValidatorT :: Term s PAssetClass -> Term s (PBuiltinList PPubKeyHash) -> Term s PInteger -> Term s PBool -> Term s ((PTuple DAOAction PInteger) :--> PScriptContext :--> PBool)
daoMultisigPolicyValidatorT poolNft daoPkhs threshold lpFeeIsEditable = plam $ \redeemer ctx -> unTermCont $ do
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

  pure $ validCommonFields #&& validThreshold #&& validAction
