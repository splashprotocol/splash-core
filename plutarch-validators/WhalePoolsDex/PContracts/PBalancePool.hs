{-# LANGUAGE UndecidableInstances #-}

module WhalePoolsDex.PContracts.PBalancePool where

import qualified GHC.Generics as GHC
import           Generics.SOP (Generic, I (I))

import Plutarch
import Plutarch.Api.V2              (PScriptHash(..), PMaybeData (..), PTxOut, POutputDatum(..), PAddress(..), PPubKeyHash(..), PDatum(..), PValue(..), KeyGuarantees(..), AmountGuarantees(..), PStakingCredential(..))
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
import Plutarch.Rational
import Plutarch.Num                 ((#*), pabs)
import Plutarch.Extra.Maybe         as Maybe
import Plutarch.Api.V1.AssocMap
import PExtra.Num                   (pexp)
import Plutarch.Positive

import PExtra.API                   (PAssetClass, assetClassValueOf, ptryFromData, assetClass, pValueLength, tletUnwrap)
import PExtra.List                  (pelemAt)
import PExtra.Monadic               (tcon, tlet, tletField, tmatch)
import PExtra.Pair
import PExtra.Integer
import Plutarch.Trace

import qualified WhalePoolsDex.Contracts.BalancePool as BP
import           WhalePoolsDex.PContracts.PPool      hiding (PoolConfig(..), PoolAction(..))
import           WhalePoolsDex.PContracts.PApi       (maxLqCap, feeDen, zero)


newtype BalancePoolConfig (s :: S)
    = BalancePoolConfig
        ( Term
            s
            ( PDataRecord
                '[ "poolNft"          ':= PAssetClass
                 , "poolX"            ':= PAssetClass
                 , "weightX"          ':= PInteger
                 , "poolY"            ':= PAssetClass
                 , "weightY"          ':= PInteger
                 , "poolLq"           ':= PAssetClass
                 , "feeNum"           ':= PInteger
                 , "treasuryFee"      ':= PInteger
                 , "treasuryX"        ':= PInteger
                 , "treasuryY"        ':= PInteger
                 , "DAOPolicy"        ':= PBuiltinList (PAsData PStakingCredential)
                 , "treasuryAddress"  ':= PValidatorHash
                 , "invariant"        ':= PInteger
                 ]
            )
        )
    deriving stock (GHC.Generic)
    deriving
        (PIsData, PDataFields, PlutusType, PEq)

instance DerivePlutusType BalancePoolConfig where type DPTStrat _ = PlutusTypeData

instance PUnsafeLiftDecl BalancePoolConfig where type PLifted BalancePoolConfig = BP.BalancePoolConfig
deriving via (DerivePConstantViaData BP.BalancePoolConfig BalancePoolConfig) instance (PConstantDecl BP.BalancePoolConfig)

instance PTryFrom PData (PAsData BalancePoolConfig)

newtype BalancePoolState (s :: S)
    = BalancePoolState
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

instance DerivePlutusType BalancePoolState where type DPTStrat _ = PlutusTypeData

data BalancePoolAction (s :: S) = Deposit | Redeem | Swap | DAOAction

instance PIsData BalancePoolAction where
    pfromDataImpl tx =
        let x = pasInt # pforgetData tx
         in pmatch' x pcon
    pdataImpl x = pmatch x (punsafeCoerce . pdata . pcon')

instance PlutusType BalancePoolAction where
    type PInner BalancePoolAction = PInteger

    pcon' Deposit = 0
    pcon' Redeem = 1
    pcon' Swap = 2
    pcon' DAOAction = 3

    pmatch' x f =
        pif
            (x #== 0)
            (f Deposit)
            ( pif
                (x #== 1)
                (f Redeem)
                ( pif (x #== 2) (f Swap) (f DAOAction))
            )

newtype BalancePoolRedeemer (s :: S)
    = BalancePoolRedeemer
        ( Term
            s
            ( PDataRecord
                '[ "action" ':= BalancePoolAction
                 , "selfIx" ':= PInteger
                 -- for swap, deposit / redeem (All assets) contains: gX, gY
                 -- for deposit / redeem (Single asset) contains also ideal deposit at 6 idx
                 , "g"     ':= PBuiltinList (PAsData PInteger)
                 -- for swap, deposit / redeem (All assets) contains: tX, Y
                 -- for deposit / redeem (Single asset) contains:
                 , "t"     ':= PBuiltinList (PAsData PInteger)
                 ]
            )
        )
    deriving stock (GHC.Generic)
    deriving
        (PIsData, PDataFields, PlutusType)

instance DerivePlutusType BalancePoolRedeemer where type DPTStrat _ = PlutusTypeData

-- Balance pool related constants --

pDen :: Term s PInteger
pDen = pconstant 10

------------------------------------

parseDatum :: ClosedTerm (PDatum :--> BalancePoolConfig)
parseDatum = plam $ \newDatum -> unTermCont $ do
  PDatum poolDatum <- pmatchC newDatum
  tletUnwrap $ ptryFromData @(BalancePoolConfig) $ poolDatum

pIntLength :: ClosedTerm (PInteger :--> PInteger)
pIntLength = plam $ \integerToProcess -> pIntLengthInternal # integerToProcess # 1 # 1

pIntLengthInternal :: Term s (PInteger :--> PInteger :--> PInteger :--> PInteger)
pIntLengthInternal =
    phoistAcyclic $
        pfix #$ plam $ \self integerToProcess acc10 accLength ->
            pif
                (integerToProcess #<= acc10)
                (accLength - 1)
                (self # integerToProcess # (acc10 * 10) # (accLength + 1))

roundTo :: ClosedTerm (PInteger :--> PInteger :--> PInteger)
roundTo = plam $ \origValue roundIdx ->
    let
        roundingDenum = ptryPositive # (ppow # 10 # ((pIntLength # origValue) - roundIdx))
        rational      = (pcon $ PRational origValue roundingDenum)
    in pround # rational

verifyGTValues ::
    ClosedTerm
        (    PInteger
        :--> PInteger
        :--> PInteger
        :--> PInteger
        :--> PBool
        )
verifyGTValues = plam $ \tokenBalance tokenWeight tokenG tokenT ->
    let 
        tokenPrecision  = pIntLength # tokenBalance
        finalLeftValue  = roundTo # tokenG # tokenPrecision
        finalRightValue = roundTo # (ppow # tokenT # tokenWeight) # tokenPrecision
    in finalLeftValue #== finalRightValue

verifyGEquality ::
    ClosedTerm
        (    PInteger
        :--> PInteger
        :--> PInteger
        :--> PInteger
        :--> PInteger
        :--> PBool
        )
verifyGEquality = plam $ \leftSideMultiplicator rightSideRaw prevTokenBalance tokenG tokenWeight ->
    let
        tokenBalanceIntLength = pIntLength # prevTokenBalance

        degree = pdiv # pDen # tokenWeight

        -- tokenG = rightSideNum ^ (tokenWeight / pDen)
        -- leftSideRaw = tokenG ^ (pDen / tokenWeight) => leftSide == (rightSide +-1)
        leftSideRaw = (ppow # tokenG # degree) * leftSideMultiplicator
        leftSide    = roundTo # leftSideRaw # tokenBalanceIntLength
        rightSide   = roundTo # rightSideRaw # tokenBalanceIntLength

        gEDiff = leftSide - rightSide
        validGEquality = pif
            ( gEDiff #<= 0 )
            ( (-1) #<= gEDiff )
            ( gEDiff #<= (1) )

    in validGEquality

verifyTExpEquality ::
    ClosedTerm
        (    PInteger
        :--> PInteger
        :--> PBool
        )
verifyTExpEquality = plam $ \tokenT rightSide ->
    let
        rightLength = pIntLength # rightSide
        leftRounded = roundTo # (ppow # tokenT # 10) # rightLength
    in leftRounded #== rightSide

validGTAndTokenDeltaWithFees ::
    ClosedTerm
        (    PInteger
        :--> PInteger
        :--> PInteger
        :--> PInteger
        :--> PInteger
        :--> PInteger
        :--> PBool
        )
validGTAndTokenDeltaWithFees = plam $ \prevTokenBalance tokenWeight tokenDelta tokenG tokenT fees ->
    let
        correctGandT = verifyGTValues # (prevTokenBalance + tokenDelta) # tokenWeight # tokenG # tokenT

        correctTokenValue = pif
            ( (pmod # pDen # tokenWeight) #== 0 )
            ( verifyGEquality # feeDen # (prevTokenBalance * feeDen + tokenDelta * fees) # prevTokenBalance # tokenG # tokenWeight )  --( leftSide #== rightSide )
            ( verifyTExpEquality # tokenT # (prevTokenBalance * feeDen + tokenDelta * fees) )

    in correctGandT #&& correctTokenValue

-- Common task is validate G against T and new token value 
validGTAndTokenDeltaWithoutFees ::
    ClosedTerm
        (    PInteger
        :--> PInteger
        :--> PInteger
        :--> PInteger
        :--> PInteger
        :--> PBool
        )
validGTAndTokenDeltaWithoutFees = plam $ \prevTokenBalance tokenWeight tokenDelta tokenG tokenT ->
    let
        correctGandT = verifyGTValues # prevTokenBalance # tokenWeight # tokenG # tokenT

        correctTokenValue = pif
            ( (pmod # pDen # tokenWeight) #== 0 )
            ( verifyGEquality # 1 # (prevTokenBalance + tokenDelta) # prevTokenBalance # tokenG # tokenWeight )
            ( verifyTExpEquality # tokenT # (prevTokenBalance + tokenDelta) )

    in correctGandT #&& correctTokenValue

validSwap :: 
    ClosedTerm 
        (    BalancePoolState 
        :--> BalancePoolState 
        :--> BalancePoolConfig 
        :--> BalancePoolConfig 
        :--> PInteger
        :--> PInteger
        :--> PInteger
        :--> PInteger
        :--> PBool
        )
validSwap = plam $ \prevState' newState' prevPoolConfig newPoolConfig newGX newTx newGY newTy -> unTermCont $ do
    prevState  <- pletFieldsC @'["reservesX", "reservesY", "liquidity"] prevState'
    newState   <- pletFieldsC @'["reservesX", "reservesY", "liquidity"] newState'
    prevConfig <- pletFieldsC @'["poolNft", "poolX", "weightX", "poolY", "weightY", "poolLq", "feeNum", "treasuryFee", "treasuryX", "treasuryY", "DAOPolicy", "treasuryAddress", "invariant"] prevPoolConfig
    newConfig  <- pletFieldsC @'["treasuryX", "treasuryY"] newPoolConfig
    let
        prevPoolNft = getField @"poolNft" prevConfig
        prevPoolX   = getField @"poolX"  prevConfig
        weightX     = getField @"weightX" prevConfig
        prevPoolY   = getField @"poolY"  prevConfig
        weightY     = getField @"weightY" prevConfig
        prevPoolLq  = getField @"poolLq" prevConfig
        feeNum      = getField @"feeNum" prevConfig
        treasuryFee = getField @"treasuryFee" prevConfig
        prevTreasuryX = getField @"treasuryX" prevConfig
        prevTreasuryY = getField @"treasuryY" prevConfig
        prevDAOPolicy = getField @"DAOPolicy" prevConfig
        prevTreasuryAddress = getField @"treasuryAddress" prevConfig
        prevInvariant = getField @"invariant" prevConfig

        newTreasuryX = getField @"treasuryX" newConfig
        newTreasuryY = getField @"treasuryY" newConfig

        -- Treasury X/Y already substracted from reserveX/Y
        prevX  = pfromData $ getField @"reservesX" prevState
        prevY  = pfromData $ getField @"reservesY" prevState
        prevLq = pfromData $ getField @"liquidity" prevState

        newX  = pfromData $ getField @"reservesX" newState
        newY  = pfromData $ getField @"reservesY" newState
        newLq = pfromData $ getField @"liquidity" newState

        dx  = newX - prevX
        dy  = newY - prevY
        dlq = newLq - prevLq

        prevInvariantLength = pIntLength # prevInvariant
        newInvarianRounded  = roundTo # (newGX #* newGY) # prevInvariantLength

        -- Verify that new value of invariant equals to previous
        newInvariantIsCorrect = prevInvariant #== newInvarianRounded

        correctTokensUpdate =
            pif
                ( zero #< dx )
                ( (validGTAndTokenDeltaWithFees # prevX # weightX # dx # newGX # newTx # (feeNum - treasuryFee)) #&& (validGTAndTokenDeltaWithoutFees # prevY # weightY # dy # newGY # newTy) )
                ( (validGTAndTokenDeltaWithoutFees # prevX # weightX # dx # newGX # newTx) #&& (validGTAndTokenDeltaWithFees # prevY # weightY # dy # newGY # newTy # (feeNum - treasuryFee)) )

        correctTreasuryUpdate =
            pif
                ( zero #< dx )
                ( ((newTreasuryX * feeDen) #== (prevTreasuryX + (dx * treasuryFee))) #&& (prevTreasuryY #== newTreasuryY) )
                ( ((newTreasuryY * feeDen) #== (prevTreasuryY + (dy * treasuryFee))) #&& (prevTreasuryX #== newTreasuryX) )

    newExpectedConfig <-
        tcon $ (BalancePoolConfig $
            pdcons @"poolNft" @PAssetClass # pdata prevPoolNft
                #$ pdcons @"poolX" @PAssetClass # pdata prevPoolX
                #$ pdcons @"weightX" @PInteger # pdata weightX
                #$ pdcons @"poolY" @PAssetClass # pdata prevPoolY
                #$ pdcons @"weightY" @PInteger # pdata weightY
                #$ pdcons @"poolLq" @PAssetClass # pdata prevPoolLq
                #$ pdcons @"feeNum" @PInteger # pdata feeNum
                #$ pdcons @"treasuryFee" @PInteger # pdata treasuryFee
                #$ pdcons @"treasuryX" @PInteger # pdata newTreasuryX
                #$ pdcons @"treasuryY" @PInteger # pdata newTreasuryY
                #$ pdcons @"DAOPolicy" @(PBuiltinList (PAsData PStakingCredential)) # pdata prevDAOPolicy
                #$ pdcons @"treasuryAddress" @PValidatorHash # pdata prevTreasuryAddress
                #$ pdcons @"invariant" @PInteger # pdata prevInvariant
                    # pdnil)

    pure $
        (   newInvariantIsCorrect 
        #&& correctTokensUpdate 
        #&& correctTreasuryUpdate 
        #&& (newPoolConfig #== newExpectedConfig)
        #&& (dlq #== zero)
        )

validDAOAction :: ClosedTerm (BalancePoolConfig :--> PTxInfo :--> PBool)
validDAOAction = plam $ \cfg txInfo -> unTermCont $ do
  wdrl     <- tletField @"wdrl" txInfo
  policies <- tletField @"DAOPolicy" cfg
  let 
      policySC = pfromData $ phead # policies
      headWithdrawl = plookup # policySC # wdrl
  pure $ Maybe.pisJust # headWithdrawl

correctLpTokenDelta ::
    ClosedTerm 
        (    PInteger
        :--> PInteger
        :--> PInteger
        :--> PInteger
        :--> PInteger
        :--> PInteger
        :--> PInteger
        :--> PBool
        )
correctLpTokenDelta = plam $ \lpIssued lpDelta tokenDelta tokenBalance tokenWeight tokenG tokenT ->
    let
        tokenBalanceIntLength = pIntLength # tokenBalance
                
        leftPart  = roundTo # ((pabs #tokenDelta) * lpIssued)   # tokenBalanceIntLength
        rightPart = roundTo # (tokenBalance * (pabs # lpDelta)) # tokenBalanceIntLength

        leftRightPartDiff = leftPart - rightPart

        -- rounding. double check
        correctTokenIn = pif
            ( leftRightPartDiff #<= 0 )
            ( (-1) #<= leftRightPartDiff )
            ( leftRightPartDiff #<= (1) )
        
        correctTokenValue = pif
            ( (pmod # pDen # tokenWeight) #== 0 )
            -- todo: on deposit `+ tokenIn`
            ( verifyGEquality # 1 # (tokenBalance + tokenDelta) # tokenBalance # tokenG # tokenWeight )
            ( verifyTExpEquality # tokenT # (tokenBalance + tokenDelta) )

    in correctTokenIn #&& correctTokenValue

validDepositRedeemAllTokens :: 
    ClosedTerm 
        (    BalancePoolState 
        :--> BalancePoolState 
        :--> BalancePoolConfig 
        :--> BalancePoolConfig 
        :--> PInteger
        :--> PInteger
        :--> PInteger
        :--> PInteger
        :--> PBool
        )
validDepositRedeemAllTokens = plam $ \prevState' newState' prevPoolConfig newPoolConfig newGX newTx newGY newTy -> unTermCont $ do
    prevState  <- pletFieldsC @'["reservesX", "reservesY", "liquidity"] prevState'
    newState   <- pletFieldsC @'["reservesX", "reservesY", "liquidity"] newState'
    prevConfig <- pletFieldsC @'["poolNft", "poolX", "weightX", "poolY", "weightY", "poolLq", "feeNum", "treasuryFee", "treasuryX", "treasuryY", "DAOPolicy", "treasuryAddress", "invariant"] prevPoolConfig
    let
        prevPoolNft = getField @"poolNft" prevConfig
        prevPoolX   = getField @"poolX"  prevConfig
        weightX     = getField @"weightX" prevConfig
        prevPoolY   = getField @"poolY"  prevConfig
        weightY     = getField @"weightY" prevConfig
        prevPoolLq  = getField @"poolLq" prevConfig
        feeNum      = getField @"feeNum" prevConfig
        treasuryFee = getField @"treasuryFee" prevConfig
        prevTreasuryX = getField @"treasuryX" prevConfig
        prevTreasuryY = getField @"treasuryY" prevConfig
        prevDAOPolicy = getField @"DAOPolicy" prevConfig
        prevTreasuryAddress = getField @"treasuryAddress" prevConfig

        prevX  = pfromData $ getField @"reservesX" prevState
        prevY  = pfromData $ getField @"reservesY" prevState
        prevLq = pfromData $ getField @"liquidity" prevState

        newX  = pfromData $ getField @"reservesX" newState
        newY  = pfromData $ getField @"reservesY" newState
        newLq = pfromData $ getField @"liquidity" newState

        dx  = newX - prevX
        dy  = newY - prevY
        dlq = newLq - prevLq

        xDepositRedeemIsValid = correctLpTokenDelta # prevLq # dlq # dx # prevX # weightX # newGX # newTx
        yDepositRedeemIsValid = correctLpTokenDelta # prevLq # dlq # dy # prevY # weightY # newGY # newTy

        newInvariant = newGX * newGY
    
    newExpectedConfig <-
        tcon $ (BalancePoolConfig $
            pdcons @"poolNft" @PAssetClass # pdata prevPoolNft
                #$ pdcons @"poolX" @PAssetClass # pdata prevPoolX
                #$ pdcons @"weightX" @PInteger # pdata weightX
                #$ pdcons @"poolY" @PAssetClass # pdata prevPoolY
                #$ pdcons @"weightY" @PInteger # pdata weightY
                #$ pdcons @"poolLq" @PAssetClass # pdata prevPoolLq
                #$ pdcons @"feeNum" @PInteger # pdata feeNum
                #$ pdcons @"treasuryFee" @PInteger # pdata treasuryFee
                #$ pdcons @"treasuryX" @PInteger # pdata prevTreasuryX
                #$ pdcons @"treasuryY" @PInteger # pdata prevTreasuryY
                #$ pdcons @"DAOPolicy" @(PBuiltinList (PAsData PStakingCredential)) # pdata prevDAOPolicy
                #$ pdcons @"treasuryAddress" @PValidatorHash # pdata prevTreasuryAddress
                #$ pdcons @"invariant" @PInteger # pdata newInvariant
                    # pdnil)

    pure $ 
        (   xDepositRedeemIsValid
        #&& yDepositRedeemIsValid
        #&& newPoolConfig #== newExpectedConfig
        )

correctLpTokenRedeem ::
    ClosedTerm 
        (    PInteger
        :--> PInteger
        :--> PInteger
        :--> PInteger
        :--> PInteger
        :--> PInteger
        :--> PInteger
        :--> PBool
        )
correctLpTokenRedeem = plam $ \lpIssued lpRedeemed tokenOut tokenBalance tokenWeight tokenG tokenT ->
    let
        correctTokenOut  = (1 - (pdiv # (lpIssued - lpRedeemed) # lpIssued)) #* tokenBalance
        correctTokenDelta = 
            pif
                ( (pmod # pDen # tokenWeight) #== 0 )
                ( tokenOut #== tokenBalance - (ppow # tokenG # (pdiv # pDen # tokenWeight)) )
                ( tokenOut #== tokenBalance - (ppow # tokenT # tokenWeight) )
    in tokenOut #== correctTokenOut #&& correctTokenDelta

readPoolState :: Term s (BalancePoolConfig :--> PTxOut :--> BalancePoolState)
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
            BalancePoolState $
                pdcons @"reservesX" @PInteger # pdata (x - poolXTreasury)
                    #$ pdcons @"reservesY" @PInteger # pdata (y - poolYTreasury)
                    #$ pdcons @"liquidity" @PInteger # lq
                        # pdnil

balancePoolValidatorT :: ClosedTerm (BalancePoolConfig :--> BalancePoolRedeemer :--> PScriptContext :--> PBool)
balancePoolValidatorT = plam $ \conf redeemer' ctx' -> unTermCont $ do
    redeemer <- pletFieldsC @'["action", "selfIx", "g", "t", "maxDen"] redeemer'
    let
        selfIx = getField @"selfIx" redeemer
        action = getField @"action" redeemer

        gList = getField @"g" redeemer
        tList = getField @"t" redeemer

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

    outputs <- tletUnwrap $ getField @"outputs" txInfo

    nft <- tletField @"poolNft" conf

    successorOut <- tlet $ findPoolOutput # nft # outputs -- nft is preserved
    successor <- pletFieldsC @'["datum", "address", "value"] successorOut
    self      <- pletFieldsC @'["datum", "address", "value"] selfInput

    s1  <- tlet $ readPoolState # conf # successorOut

    succDatum <- tletUnwrap $ getField @"datum" successor
    POutputDatum succD' <- pmatchC succDatum
    succD               <- tletField @"outputDatum" succD'

    selfValue     <- tletUnwrap $ getField @"value" self
    succesorValue <- tletUnwrap $ getField @"value" successor

    selfAddr <- tletUnwrap $ getField @"address" self
    succAddr <- tletUnwrap $ getField @"address" successor
    let 
        scriptPreserved = succAddr #== selfAddr 
        selfValueLength = pValueLength # selfValue
        succesorValueLength = pValueLength # succesorValue

        noMoreTokens = selfValueLength #== succesorValueLength

        newConfig     = parseDatum # succD

    pure $
        selfIdentity #&& (pmatch action $ \case
            Swap    -> unTermCont $ do
                gx <- tletUnwrap $ phead # gList
                gy <- tletUnwrap $ pelemAt # (pconstant 1) # gList
                tx <- tletUnwrap $ phead # tList
                ty <- tletUnwrap $ pelemAt # (pconstant 1) # tList
                pure $ noMoreTokens #&& scriptPreserved #&& (validSwap # s0 # s1 # conf # newConfig # gx # tx # gy # ty)
            DAOAction -> validDAOAction # conf # txinfo'
            _ -> unTermCont $ do
                gx <- tletUnwrap $ phead # gList
                gy <- tletUnwrap $ pelemAt # (pconstant 1) # gList
                tx <- tletUnwrap $ phead # tList
                ty <- tletUnwrap $ pelemAt # (pconstant 1) # tList
                pure $ noMoreTokens #&& scriptPreserved #&& (validDepositRedeemAllTokens # s0 # s1 # conf # newConfig # gx # tx # gy # ty)
        )