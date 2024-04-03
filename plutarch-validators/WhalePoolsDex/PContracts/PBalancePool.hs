{-# LANGUAGE UndecidableInstances #-}

module WhalePoolsDex.PContracts.PBalancePool where

import qualified GHC.Generics as GHC hiding (log)
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
import Plutarch.Num                 ((#*), (#+), (#-), pabs, pnegate)
import Plutarch.Extra.Maybe         as Maybe
import Plutarch.Api.V1.AssocMap
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
                 , "invariantLength"  ':= PInteger
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
                 , "g"     ':= PBuiltinList (PAsData PInteger)
                 -- for swap, deposit / redeem (All assets) contains: tX, tY
                 , "t"     ':= PBuiltinList (PAsData PInteger)
                 -- info about internals lengths
                 , "lengths" ':= PBuiltinList (PAsData PInteger)
                 ]
            )
        )
    deriving stock (GHC.Generic)
    deriving
        (PIsData, PDataFields, PlutusType)

instance DerivePlutusType BalancePoolRedeemer where type DPTStrat _ = PlutusTypeData

-- Balance pool related constants --

pDen :: Term s PInteger
pDen = pconstant 5

------------------------------------

parseDatum :: ClosedTerm (PDatum :--> BalancePoolConfig)
parseDatum = plam $ \newDatum -> unTermCont $ do
  PDatum poolDatum <- pmatchC newDatum
  tletUnwrap $ ptryFromData @(BalancePoolConfig) $ poolDatum

pIntLength :: ClosedTerm (PInteger :--> PInteger)
pIntLength = plam $ \integerToProcess -> pIntLengthInternal # integerToProcess # 1

pIntLengthInternal :: Term s (PInteger :--> PInteger :--> PInteger)
pIntLengthInternal =
    phoistAcyclic $
        pfix #$ plam $ \self integerToProcess accLength ->
            plet (pdiv # integerToProcess # (pconstant 10)) $ \divided ->
            pif
                (divided #== (pconstant 0))
                (accLength #- (pconstant 1))
                (self # divided # (accLength #+ (pconstant 1)))

checkLength :: Term s (PInteger :--> PInteger :--> PInteger)
checkLength = phoistAcyclic $ plam $ \origValue apLength ->
    plet (ppow10 # (pconstant 10) # apLength) $ \upperBound ->
        plet (pdiv # upperBound # (pconstant 10)) $ \lowerBound -> unTermCont $ do
            pure (pif (lowerBound #<= origValue #&& origValue #< upperBound)
                (apLength)
                (perror))

roundToTest :: Term s (PInteger :--> PInteger :--> PInteger :--> PInteger)
roundToTest = phoistAcyclic $ plam $ \origValue roundIdx lengthTest -> unTermCont $ do
    checkedLength <- tlet $ checkLength # origValue # lengthTest
    denum         <- tlet $ (ppow10 # (pconstant 10) # (checkedLength - roundIdx))
    roundingDenum <- tlet $ ptryPositive # denum
    rational      <- tlet $ (pcon $ PRational origValue roundingDenum)
    pure $ pround # rational

verifyGTValues ::
    Term s
        (    PInteger
        :--> PInteger
        :--> PInteger
        :--> PInteger
        :--> PInteger
        :--> PInteger
        :--> PInteger
        :--> PBool
        )
verifyGTValues = plam $ \tokenBalance tokenBalanceLength tokenWeight tokenG tokenT tokenGLength tokenTPowWeightLength ->
    plet (checkLength # tokenBalance # tokenBalanceLength) $ \_ ->
      (roundToTest # tokenG # tokenBalanceLength # tokenGLength) #== (roundToTest # (ppow # tokenT # tokenWeight) # tokenBalanceLength # tokenTPowWeightLength)

verifyGEquality ::
    Term s
        (    PInteger
        :--> PInteger
        :--> PInteger
        :--> PInteger
        :--> PInteger
        :--> PInteger
        :--> PBool
        )
verifyGEquality = phoistAcyclic $ plam $ \leftSideMultiplicator rightSideRaw tokenG tokenWeight leftSideLength rightSideLength -> unTermCont $ do
    degree      <- tlet $ (pdiv # pDen # tokenWeight)
    leftSideRaw <- tlet $ (ppow # tokenG # degree) #* leftSideMultiplicator
    leftSide    <- tlet $ roundToTest # leftSideRaw # rightSideLength # leftSideLength
    gEDiff      <- tlet $ leftSide - rightSideRaw
    let
        validGEquality = pif
            ( gEDiff #<= (pconstant 0) )
            ( (pconstant (-1)) #<= gEDiff )
            ( gEDiff #<= (pconstant 1) )

    pure validGEquality

verifyTExpEquality ::
    Term s
        (    PInteger
        :--> PInteger
        :--> PInteger
        :--> PInteger
        :--> PBool
        )
verifyTExpEquality = phoistAcyclic $
    plam $ \tokenT rightSide rightSideLength tokenTPowLength -> unTermCont $ do
       leftSideRounded <- tlet $ (roundToTest # (ppow # tokenT # pDen) # rightSideLength # tokenTPowLength)
       delta <- tlet (leftSideRounded - rightSide)
       tlet (checkLength # rightSide # rightSideLength)
       let
            validDelta = pif
                ( delta #<= (pconstant 0) )
                ( (pconstant (-1)) #<= delta )
                ( delta #<= (pconstant 1) )

       pure $ validDelta

validGTAndTokenDeltaWithFees ::
    ClosedTerm
        (    PInteger
        :--> PInteger
        :--> PInteger
        :--> PInteger
        :--> PInteger
        :--> PInteger
        :--> PInteger
        :--> PInteger
        :--> PInteger
        :--> PInteger
        :--> PInteger
        :--> PInteger
        :--> PBool
        )
validGTAndTokenDeltaWithFees = phoistAcyclic $
    -- leftSideMultiplicator = feeDen
    -- leftSide  = (ppow # tokenG # degree) #* leftSideMultiplicator
    -- rightSide = (prevTokenBalance #* feeDen + tokenDelta #* fees)
    plam $ \prevTokenBalance newTokenBalanceLength tokenWeight tokenDelta tokenG tokenGLength tokenT tokenTPowLength tokenTPowWeightLength leftSideLength rightSideLength fees ->
        let
            correctGandT = verifyGTValues # (prevTokenBalance #+ tokenDelta) # newTokenBalanceLength # tokenWeight # tokenG # tokenT # tokenGLength # tokenTPowWeightLength

            correctTokenValue = pif
                ( (pmod # pDen # tokenWeight) #== (pconstant 0) )
                ( verifyGEquality    # feeDen # (prevTokenBalance #* feeDen + tokenDelta #* fees) # tokenG # tokenWeight # leftSideLength # rightSideLength )  --( leftSide #== rightSide )
                ( verifyTExpEquality # tokenT # (prevTokenBalance #* feeDen + tokenDelta #* fees) # rightSideLength # tokenTPowLength )

        in correctGandT #&& correctTokenValue

-- Common task is validate G against T and new token value 
validGTAndTokenDeltaWithoutFees ::
    Term s
        (    PInteger
        :--> PInteger
        :--> PInteger
        :--> PInteger
        :--> PInteger
        :--> PInteger
        :--> PInteger
        :--> PInteger
        :--> PInteger
        :--> PInteger
        :--> PInteger
        :--> PBool
        )
validGTAndTokenDeltaWithoutFees = phoistAcyclic $
    -- leftSideMultiplicator = (pconstant 1)
    -- leftSide  = (ppow # tokenG # degree) #* leftSideMultiplicator
    -- rightSide = (prevTokenBalance + tokenDelta)
    plam $ \prevTokenBalance prevTokenBalanceLength tokenWeight tokenDelta tokenG tokenGLength tokenT tokenTPowLength tokenTPowWeightLength leftSideLength rightSideLength ->
        let
            correctGandT = verifyGTValues # (prevTokenBalance + tokenDelta) # prevTokenBalanceLength # tokenWeight # tokenG # tokenT # tokenGLength # tokenTPowWeightLength

            correctTokenValue = pif
                ( (pmod # pDen # tokenWeight) #== (pconstant 0) )
                ( verifyGEquality    # (pconstant 1) # (prevTokenBalance + tokenDelta) # tokenG # tokenWeight # leftSideLength # rightSideLength )
                ( verifyTExpEquality # tokenT # (prevTokenBalance + tokenDelta) # rightSideLength # tokenTPowLength )

        in (correctGandT #&& correctTokenValue)

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
        :--> PInteger
        :--> PInteger
        :--> PInteger
        :--> PInteger
        :--> PInteger
        :--> PInteger
        :--> PInteger
        :--> PInteger
        :--> PInteger
        :--> PInteger
        :--> PInteger
        :--> PInteger
        :--> PInteger
        :--> PBool
        )
validSwap = plam $ \prevState' newState' prevPoolConfig newPoolConfig newGX newTx newGY newTy gXgYLength newXBalanceLength newGXLength newTxPowLength newTxPowWeightLength leftSideLengthX rightSideLengthX newYBalanceLength newGYLength newTyPowLength newTyPowWeightLength leftSideLengthY rightSideLengthY -> unTermCont $ do
    prevState  <- pletFieldsC @'["reservesX", "reservesY", "liquidity"] prevState'
    newState   <- pletFieldsC @'["reservesX", "reservesY", "liquidity"] newState'
    prevConfig <- pletFieldsC @'["poolNft", "poolX", "weightX", "poolY", "weightY", "poolLq", "feeNum", "treasuryFee", "treasuryX", "treasuryY", "DAOPolicy", "treasuryAddress", "invariant", "invariantLength"] prevPoolConfig
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
        invariantLength = getField @"invariantLength" prevConfig

        newTreasuryX = getField @"treasuryX" newConfig
        newTreasuryY = getField @"treasuryY" newConfig

        -- Treasury X/Y already substracted from reserveX/Y
        prevX  = pfromData $ getField @"reservesX" prevState
        prevY  = pfromData $ getField @"reservesY" prevState
        prevLq = pfromData $ getField @"liquidity" prevState

        newX  = pfromData $ getField @"reservesX" newState
        newY  = pfromData $ getField @"reservesY" newState
        newLq = pfromData $ getField @"liquidity" newState

    dx  <- tlet $ newX - prevX
    dy  <- tlet $ newY - prevY
    dlq <- tlet $ newLq - prevLq

    newInvarianRounded    <- tlet $ roundToTest # (newGX #* newGY) # invariantLength # gXgYLength
    invariantRoundingDiff <- tlet $ newInvarianRounded - prevInvariant

    let
        -- Verify that new value of invariant equals to previous
        newInvariantIsCorrect = pif
            ( invariantRoundingDiff #<= (pconstant 0) )
            ( (pconstant (-1)) #<= invariantRoundingDiff )
            ( invariantRoundingDiff #<= (pconstant 1) )

        -- g,t related to tokens with fees

        -- leftSideMultiplicator = feeDen
        -- leftSide  = (ppow # tokenG # degree) #* leftSideMultiplicator
        -- rightSide = (prevTokenBalance #* feeDen + tokenDelta #* fees)

        -- without fees
        -- leftSideMultiplicator = (pconstant 1)
        -- leftSide  = (ppow # tokenG # degree) #* leftSideMultiplicator
        -- rightSide = (prevTokenBalance + tokenDelta)

        correctTokensUpdate =
            pif
                ( zero #< dx )
                ( 
                    (validGTAndTokenDeltaWithFees # prevX # newXBalanceLength # weightX # dx # newGX # newGXLength # newTx # newTxPowLength # newTxPowWeightLength # leftSideLengthX # rightSideLengthX # (feeNum - treasuryFee)) 
                #&& (validGTAndTokenDeltaWithoutFees # prevY # newYBalanceLength # weightY # dy # newGY # newGYLength # newTy # newTyPowLength # newTyPowWeightLength # leftSideLengthY # rightSideLengthY) 
                )
                (   (validGTAndTokenDeltaWithoutFees # prevX # newXBalanceLength # weightX # dx # newGX # newGXLength # newTx # newTxPowLength # newTxPowWeightLength # leftSideLengthX # rightSideLengthX) 
                #&& (validGTAndTokenDeltaWithFees # prevY # newYBalanceLength # weightY # dy # newGY # newGYLength # newTy # newTyPowLength # newTyPowWeightLength # leftSideLengthY # rightSideLengthY # (feeNum - treasuryFee))
                )

        correctTreasuryUpdate =
            pif
                ( zero #< dx )
                ( ((feeDen * prevTreasuryX + (dx * treasuryFee)) #<= ((newTreasuryX + 1) * feeDen)) #&& (prevTreasuryY #== newTreasuryY) )
                ( ((feeDen * prevTreasuryY + (dy * treasuryFee)) #<= ((newTreasuryY + 1) * feeDen)) #&& (prevTreasuryX #== newTreasuryX) )

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
                #$ pdcons @"invariantLength" @PInteger # pdata invariantLength
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
        :--> PBool
        )
correctLpTokenDelta = plam $ \lpIssued lpDelta tokenDelta tokenBalance -> unTermCont $ do
    calcTokenDelta <- tlet (pdiv # (lpDelta #* tokenBalance) # lpIssued)
    tokensDiff     <- tlet (calcTokenDelta - tokenDelta)
    let
        correctTokenError = pif
            ( tokensDiff #<= (pconstant 0) )
            ( (pconstant (-1)) #<= tokensDiff )
            ( tokensDiff #<= (pconstant 1) )

        correctTokenIn = correctTokenError #&& (calcTokenDelta #<= tokenDelta)

    pure $ correctTokenIn

validDepositRedeemAllTokens :: 
    ClosedTerm 
        (    BalancePoolState 
        :--> BalancePoolState 
        :--> BalancePoolConfig 
        :--> BalancePoolConfig
        :--> PBool
        )
validDepositRedeemAllTokens = plam $ \prevState' newState' prevPoolConfig newPoolConfig -> unTermCont $ do
    prevState  <- pletFieldsC @'["reservesX", "reservesY", "liquidity"] prevState'
    newState   <- pletFieldsC @'["reservesX", "reservesY", "liquidity"] newState'
    prevConfig <- pletFieldsC @'["poolNft", "poolX", "weightX", "poolY", "weightY", "poolLq", "feeNum", "treasuryFee", "treasuryX", "treasuryY", "DAOPolicy", "treasuryAddress", "invariant", "invariantLength"] prevPoolConfig
    newConfig  <- pletFieldsC @'["invariant", "invariantLength"] newPoolConfig
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

        newInvariant       = getField @"invariant" newConfig
        newInvariantLength = getField @"invariantLength" newConfig
        
        prevX  = pfromData $ getField @"reservesX" prevState
        prevY  = pfromData $ getField @"reservesY" prevState
        prevLq = pfromData $ getField @"liquidity" prevState

        newX  = pfromData $ getField @"reservesX" newState
        newY  = pfromData $ getField @"reservesY" newState
        newLq = pfromData $ getField @"liquidity" newState

    dx  <- tlet $ newX - prevX
    dy  <- tlet $ newY - prevY
    dlq <- tlet $ newLq - prevLq

    let
        newCalculatedInvariant = pdiv # ((prevX + dx) #* prevInvariant) # prevX

        invariantroundUpIsNecessary = 0 #< (pmod # ((prevX + dx) #* prevInvariant) # prevX)

        normalizedInvariant =
            pif (invariantroundUpIsNecessary)
                (newCalculatedInvariant #+ 1)
                (newCalculatedInvariant)

        xDepositRedeemIsValid = correctLpTokenDelta # prevLq # dlq # dx # prevX
        yDepositRedeemIsValid = correctLpTokenDelta # prevLq # dlq # dy # prevY

    tlet (checkLength # newInvariant # newInvariantLength)

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
                #$ pdcons @"invariantLength" @PInteger # pdata newInvariantLength
                    # pdnil)

    pure $ 
        (   normalizedInvariant #== newInvariant
        #&& xDepositRedeemIsValid
        #&& yDepositRedeemIsValid
        #&& newPoolConfig #== newExpectedConfig
        )

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
    redeemer <- pletFieldsC @'["action", "selfIx", "g", "t", "lengths"] redeemer'
    let
        selfIx = getField @"selfIx" redeemer
        action = getField @"action" redeemer

        gList = getField @"g" redeemer
        tList = getField @"t" redeemer

        lList = getField @"lengths" redeemer

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

        newConfig    = parseDatum # succD

    pure $
        selfIdentity #&& (pmatch action $ \case
            Swap    -> unTermCont $ do

                gx <- tletUnwrap $ phead # gList
                gy <- tletUnwrap $ pelemAt # (pconstant 1) # gList
                tx <- tletUnwrap $ phead # tList
                ty <- tletUnwrap $ pelemAt # (pconstant 1) # tList

                -- Parsing length list
                gXgYLength           <- tletUnwrap $ phead # lList
                newXBalanceLength    <- tletUnwrap $ pelemAt # (pconstant 1)  # lList
                newGXLength          <- tletUnwrap $ pelemAt # (pconstant 2)  # lList
                newTxPowLength       <- tletUnwrap $ pelemAt # (pconstant 3)  # lList
                newTxPowWeightLength <- tletUnwrap $ pelemAt # (pconstant 4)  # lList
                leftSideLengthX      <- tletUnwrap $ pelemAt # (pconstant 5)  # lList
                rightSideLengthX     <- tletUnwrap $ pelemAt # (pconstant 6)  # lList
                newYBalanceLength    <- tletUnwrap $ pelemAt # (pconstant 7)  # lList
                newGYLength          <- tletUnwrap $ pelemAt # (pconstant 8)  # lList
                newTyPowLength       <- tletUnwrap $ pelemAt # (pconstant 9)  # lList
                newTyPowWeightLength <- tletUnwrap $ pelemAt # (pconstant 10) # lList
                leftSideLengthY      <- tletUnwrap $ pelemAt # (pconstant 11) # lList
                rightSideLengthY     <- tletUnwrap $ pelemAt # (pconstant 12) # lList  
                
                pure $ noMoreTokens #&& scriptPreserved #&& (validSwap # s0 # s1 # conf # newConfig # gx # tx # gy # ty # gXgYLength # newXBalanceLength # newGXLength # newTxPowLength # newTxPowWeightLength # leftSideLengthX # rightSideLengthX # newYBalanceLength # newGYLength # newTyPowLength # newTyPowWeightLength # leftSideLengthY # rightSideLengthY)
            DAOAction -> validDAOAction # conf # txinfo'
            _ ->         noMoreTokens #&& scriptPreserved #&& (validDepositRedeemAllTokens # s0 # s1 # conf # newConfig)
        )