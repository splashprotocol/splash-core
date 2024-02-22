{-# LANGUAGE UndecidableInstances #-}

module WhalePoolsDex.PContracts.PPool (
    PoolConfig (..),
    PoolAction (..),
    PoolRedeemer (..),
    PoolState(..),
    findPoolOutput,
    poolValidatorT
) where

import qualified GHC.Generics as GHC
import           Generics.SOP (Generic, I (I))

import Plutarch
import Plutarch.Api.V2              (PScriptHash(..), PMaybeData (..), PTxOut, POutputDatum(..), PAddress(..), PPubKeyHash(..), PDatum(..), PValue(..), KeyGuarantees(..), AmountGuarantees(..), PCurrencySymbol(..))
import Plutarch.Api.V2.Contexts     (PScriptContext, PScriptPurpose (PSpending), PTxInfo(..))
import Plutarch.DataRepr
import Plutarch.Lift
import Plutarch.Prelude
import Plutarch.Extra.TermCont
import Plutarch.Builtin             (pasInt, pforgetData, pfromData, pdata, PIsData(..))
import Plutarch.Unsafe              (punsafeCoerce)
import Plutarch.Internal.PlutusType (PInner, PlutusType, pcon', pmatch')
import Plutarch.TryFrom             (PTryFrom (PTryFromExcess, ptryFrom'))
import Plutarch.Api.V1.Scripts      (PValidatorHash)

import PExtra.API                   (PAssetClass, assetClassValueOf, ptryFromData, assetClass, pValueLength)
import PExtra.List                  (pelemAt)
import PExtra.Monadic               (tcon, tlet, tletField, tmatch)

import qualified WhalePoolsDex.Contracts.Pool     as P
import           WhalePoolsDex.PContracts.PApi    (burnLqInitial, feeDen, treasuryFeeDen, maxLqCap, tletUnwrap, zero, containsSignature)
import           WhalePoolsDex.PConstants

import Plutarch.Trace

newtype Flip f a b = Flip (f b a) deriving stock (Plutarch.Prelude.Generic)

instance PTryFrom PData (PAsData PScriptHash) where
  type PTryFromExcess PData (PAsData PScriptHash) = Flip Term PScriptHash
  ptryFrom' opq = runTermCont $ do
    unwrapped <- tcont . plet $ ptryFrom @(PAsData PByteString) opq snd
    tcont $ \f ->
      pif (plengthBS # unwrapped #== 28) (f ()) (ptraceError "ptryFrom(PScriptHash): must be 28 bytes long")
    pure (punsafeCoerce opq, pcon . PScriptHash $ unwrapped)

newtype PoolConfig (s :: S)
    = PoolConfig
        ( Term
            s
            ( PDataRecord
                '[ "poolNft"          ':= PAssetClass
                 , "poolX"            ':= PAssetClass
                 , "poolY"            ':= PAssetClass
                 , "poolLq"           ':= PAssetClass
                 , "feeNum"           ':= PInteger
                 , "treasuryFee"      ':= PInteger
                 , "treasuryX"        ':= PInteger
                 , "treasuryY"        ':= PInteger
                 , "DAOPolicy"        ':= PBuiltinList (PAsData PCurrencySymbol)
                 , "lqBound"          ':= PInteger
                 , "treasuryAddress"  ':= PValidatorHash
                 ]
            )
        )
    deriving stock (GHC.Generic)
    deriving
        (PIsData, PDataFields, PlutusType, PEq)

instance DerivePlutusType PoolConfig where type DPTStrat _ = PlutusTypeData

instance PUnsafeLiftDecl PoolConfig where type PLifted PoolConfig = P.PoolConfig
deriving via (DerivePConstantViaData P.PoolConfig PoolConfig) instance (PConstantDecl P.PoolConfig)

instance PTryFrom PData (PAsData PoolConfig)

data PoolAction (s :: S) = Deposit | Redeem | Swap | Destroy | DAOAction

instance PIsData PoolAction where
    pfromDataImpl tx =
        let x = pasInt # pforgetData tx
         in pmatch' x pcon
    pdataImpl x = pmatch x (punsafeCoerce . pdata . pcon')

instance PlutusType PoolAction where
    type PInner PoolAction = PInteger

    pcon' Deposit = 0
    pcon' Redeem = 1
    pcon' Swap = 2
    pcon' Destroy = 3
    pcon' DAOAction = 4

    pmatch' x f =
        pif
            (x #== 0)
            (f Deposit)
            ( pif
                (x #== 1)
                (f Redeem)
                ( pif
                    (x #== 2)
                    (f Swap)
                    (pif (x #== 3) (f Destroy) (f DAOAction))
                )
            )

newtype PoolRedeemer (s :: S)
    = PoolRedeemer
        ( Term
            s
            ( PDataRecord
                '[ "action" ':= PoolAction
                 , "selfIx" ':= PInteger
                 ]
            )
        )
    deriving stock (GHC.Generic)
    deriving
        (PIsData, PDataFields, PlutusType)

instance DerivePlutusType PoolRedeemer where type DPTStrat _ = PlutusTypeData

newtype PoolState (s :: S)
    = PoolState
        ( Term
            s
            ( PDataRecord
                '[ "reservesX"   ':= PInteger
                 , "reservesY"   ':= PInteger
                 , "liquidity"   ':= PInteger
                 ]
            )
        )
    deriving stock (GHC.Generic)
    deriving
        (PIsData, PDataFields, PlutusType)

instance DerivePlutusType PoolState where type DPTStrat _ = PlutusTypeData

newtype PoolDiff (s :: S)
    = PoolDiff
        ( Term
            s
            ( PDataRecord
                '[ "diffX" ':= PInteger
                 , "diffY" ':= PInteger
                 , "diffLq" ':= PInteger
                 ]
            )
        )
    deriving stock (GHC.Generic)
    deriving
        (PIsData, PDataFields, PlutusType)

instance DerivePlutusType PoolDiff where type DPTStrat _ = PlutusTypeData

readPoolState :: Term s (PoolConfig :--> PTxOut :--> PoolState)
readPoolState = phoistAcyclic $
    plam $ \conf' out -> unTermCont $ do
        conf  <- pletFieldsC @'["poolX", "poolY", "poolLq", "treasuryX", "treasuryY"] conf'
        let
            poolX  = getField @"poolX"  conf
            poolY  = getField @"poolY"  conf
            poolLq = getField @"poolLq" conf

            poolXTreasury = getField @"treasuryX" conf
            poolYTreasury = getField @"treasuryY" conf

        value <- tletField @"value" out
        
        let 
            x = assetClassValueOf # value # poolX
            y = assetClassValueOf # value # poolY
            negLq = assetClassValueOf # value # poolLq
            lq = pdata $ maxLqCap - negLq
        tcon $
            PoolState $
                pdcons @"reservesX" @PInteger # pdata (x - poolXTreasury)
                    #$ pdcons @"reservesY" @PInteger # pdata (y - poolYTreasury)
                    #$ pdcons @"liquidity" @PInteger # lq
                        # pdnil

correctSwapConfig :: Term s (PoolConfig :--> PoolConfig :--> PInteger :--> PInteger :--> PBool)
correctSwapConfig = plam $ \prevDatum newDatum dx dy -> unTermCont $ do
  prevConfig <- pletFieldsC @'["poolNft", "poolX", "poolY", "poolLq", "feeNum", "treasuryFee", "treasuryX", "treasuryY", "DAOPolicy", "lqBound", "treasuryAddress"] prevDatum
  newConfig  <- pletFieldsC @'["treasuryX", "treasuryY"] newDatum
  
  let
    prevPoolNft = getField @"poolNft" prevConfig
    prevPoolX   = getField @"poolX"  prevConfig
    prevPoolY   = getField @"poolY"  prevConfig
    prevPoolLq  = getField @"poolLq" prevConfig
    prevFeeNum  = getField @"feeNum" prevConfig
    prevTreasuryFee = getField @"treasuryFee" prevConfig
    prevTreasuryX = getField @"treasuryX" prevConfig
    prevTreasuryY = getField @"treasuryY" prevConfig
    prevDAOPolicy = getField @"DAOPolicy" prevConfig
    prevLqBound   = getField @"lqBound" prevConfig
    prevTreasuryAddress = getField @"treasuryAddress" prevConfig

    newTreasuryX = getField @"treasuryX" newConfig
    newTreasuryY = getField @"treasuryY" newConfig

    dt = 
      pif
        (zero #< dx)
        (newTreasuryX - prevTreasuryX)
        (newTreasuryY - prevTreasuryY)

    c1 = feeDen * treasuryFeeDen

    c2 = 
      pif
        (zero #< dx)
        (dx * prevTreasuryFee * (feeDen - prevFeeNum))
        (dy * prevTreasuryFee * (feeDen - prevFeeNum))
        
    validTreasuryChange = (c1 * dt #<= c2) #&& (c2 #< c1 * (dt + 1))

    anotherTokenTreasuryCorrect =
      pif
        (zero #< dx)
        (prevTreasuryY #== newTreasuryY)
        (prevTreasuryX #== newTreasuryX)

  expectedConfig <-
        tcon $ (PoolConfig $
            pdcons @"poolNft" @PAssetClass # pdata prevPoolNft
                #$ pdcons @"poolX" @PAssetClass # pdata prevPoolX
                #$ pdcons @"poolY" @PAssetClass # pdata prevPoolY
                #$ pdcons @"poolLq" @PAssetClass # pdata prevPoolLq
                #$ pdcons @"feeNum" @PInteger # pdata prevFeeNum
                #$ pdcons @"treasuryFee" @PInteger # pdata prevTreasuryFee
                #$ pdcons @"treasuryX" @PInteger # pdata newTreasuryX
                #$ pdcons @"treasuryY" @PInteger # pdata newTreasuryY
                #$ pdcons @"DAOPolicy" @(PBuiltinList (PAsData PCurrencySymbol)) # pdata prevDAOPolicy
                #$ pdcons @"lqBound" @PInteger # pdata prevLqBound
                #$ pdcons @"treasuryAddress" @PValidatorHash # pdata prevTreasuryAddress
                    # pdnil)

  let validConfig = expectedConfig #== newDatum

  pure $ validConfig #&& validTreasuryChange #&& anotherTokenTreasuryCorrect

-- Guarantees preservation of pool NFT
findPoolOutput :: Term s (PAssetClass :--> PBuiltinList PTxOut :--> PTxOut)
findPoolOutput =
    phoistAcyclic $
        plam $ \nft ->
            precList
                ( \self x xs ->
                    let value = pfield @"value" # x
                        amt   = assetClassValueOf # value # nft
                     in pif (amt #== 1) x (self # xs)
                )
                (const $ ptraceError "Pool output not found")

validDAOAction :: ClosedTerm (PoolConfig :--> PTxInfo :--> PBool)
validDAOAction = plam $ \cfg txInfo -> unTermCont $ do
  valueMint <- tletField @"mint" txInfo
  policies  <- tletField @"DAOPolicy" cfg
  let 
      policyCS = pfromData $ phead # policies
      mintedAc = assetClass # policyCS # poolStakeChangeMintTokenNameP
  pure $ assetClassValueOf # valueMint # mintedAc #== 1

parseDatum :: ClosedTerm (PDatum :--> PoolConfig)
parseDatum = plam $ \newDatum -> unTermCont $ do
  PDatum poolDatum <- pmatchC newDatum
  tletUnwrap $ ptryFromData @(PoolConfig) $ poolDatum

poolValidatorT :: ClosedTerm (PoolConfig :--> PoolRedeemer :--> PScriptContext :--> PBool)
poolValidatorT = plam $ \conf redeemer' ctx' -> unTermCont $ do
    redeemer <- pletFieldsC @'["action", "selfIx"] redeemer'
    let
        selfIx = getField @"selfIx" redeemer
        action = getField @"action" redeemer

    ctx <- pletFieldsC @'["txInfo", "purpose"] ctx'
    let txinfo' = getField @"txInfo" ctx

    txInfo  <- pletFieldsC @'["inputs", "outputs"] txinfo'
    inputs  <- tletUnwrap $ getField @"inputs" txInfo
    selfIn' <- tlet $ pelemAt # selfIx # inputs
    selfIn  <- pletFieldsC @'["outRef", "resolved"] selfIn'

    PSpending selfRef' <- pmatchC $ getField @"purpose" ctx

    selfRef <- tletField @"_0" selfRef'
    let 
        selfInRef    = getField @"outRef" selfIn
        selfIdentity = selfRef #== selfInRef -- self is the output currently validated by this script

        selfInput = getField @"resolved" selfIn
        
    s0  <- tlet $ readPoolState # conf # selfInput
    lq0 <- tletField @"liquidity" s0

    pure $
        selfIdentity #&& (pmatch action $ \case
            Destroy -> lq0 #<= burnLqInitial -- all tokens except for permanetly locked ones are removed
            _ -> unTermCont $ do
                outputs <- tletUnwrap $ getField @"outputs" txInfo

                nft <- tletField @"poolNft" conf

                successorOut <- tlet $ findPoolOutput # nft # outputs -- nft is preserved
                successor <- pletFieldsC @'["datum", "address", "value"] successorOut
                self      <- pletFieldsC @'["datum", "address", "value"] selfInput

                s1  <- tlet $ readPoolState # conf # successorOut
                rx0 <- tletField @"reservesX" s0
                rx1 <- tletField @"reservesX" s1
                ry0 <- tletField @"reservesY" s0
                ry1 <- tletField @"reservesY" s1
                lq1 <- tletField @"liquidity" s1
                let dx  = rx1 - rx0
                    dy  = ry1 - ry0
                    dlq = lq1 - lq0 -- pool keeps only the negative part of LQ tokens

                selfDatum <- tletUnwrap $ getField @"datum" self
                succDatum <- tletUnwrap $ getField @"datum" successor

                POutputDatum succD' <- pmatchC succDatum
                succD               <- tletField @"outputDatum" succD'

                selfValue     <- tletUnwrap $ getField @"value" self
                succesorValue <- tletUnwrap $ getField @"value" successor

                let 
                    selfValueLength = pValueLength # selfValue
                    succesorValueLength = pValueLength # succesorValue

                    noMoreTokens = selfValueLength #== succesorValueLength

                selfAddr <- tletUnwrap $ getField @"address" self
                succAddr <- tletUnwrap $ getField @"address" successor
                let 
                    scriptPreserved = succAddr #== selfAddr -- validator, staking cred preserved
                    valid = pmatch action $ \case
                        Swap -> unTermCont $ do
                            feeNum  <- tletField @"feeNum" conf
                            feeDen' <- tlet feeDen
                            lqBound <- tletField @"lqBound" conf
                            let
                                newConfig     = parseDatum # succD
                                validTreasury = correctSwapConfig # conf # newConfig # dx # dy

                                swapAllowed = lqBound #<= (rx0 * 2)

                                dxf = dx * feeNum
                                dyf = dy * feeNum

                                validSwap =
                                    pif
                                        (zero #< dx)
                                        (-dy * (rx0 * feeDen' + dxf) #<= ry0 * dxf)
                                        (-dx * (ry0 * feeDen' + dyf) #<= rx0 * dyf)

                            pure $ noMoreTokens #&& swapAllowed #&& scriptPreserved #&& dlq #== 0 #&& validSwap #&& validTreasury -- liquidity left intact and swap is performed properly
                        DAOAction -> validDAOAction # conf # txinfo'
                        _ -> unTermCont $ do
                            POutputDatum selfD' <- pmatchC selfDatum
                            selfD               <- tletField @"outputDatum" selfD'
                            let
                              confPreserved      = selfD #== succD -- whole config preserved
                              validDepositRedeem = dlq * rx0 #<= dx * lq0 #&& dlq * ry0 #<= dy * lq0
                            pure $ noMoreTokens #&& confPreserved #&& scriptPreserved #&& validDepositRedeem -- either deposit or redeem is performed properly                
                pure valid
            )
