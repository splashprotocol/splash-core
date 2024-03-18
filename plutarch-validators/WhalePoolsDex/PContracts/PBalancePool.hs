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
import Plutarch.Num                 ((#*))
import Plutarch.Extra.Maybe         as Maybe
import Plutarch.Api.V1.AssocMap
import PExtra.Num (pexp)
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

data BalancePoolAction (s :: S) = Deposit | DepositSingle | Redeem | RedeemSingle | Swap | DAOAction

instance PIsData BalancePoolAction where
    pfromDataImpl tx =
        let x = pasInt # pforgetData tx
         in pmatch' x pcon
    pdataImpl x = pmatch x (punsafeCoerce . pdata . pcon')

instance PlutusType BalancePoolAction where
    type PInner BalancePoolAction = PInteger

    pcon' Deposit = 0
    pcon' DepositSingle = 1
    pcon' Redeem = 2
    pcon' RedeemSingle = 3
    pcon' Swap = 4
    pcon' DAOAction = 5

    pmatch' x f =
        pif
            (x #== 0)
            (f Deposit)
            ( pif
                (x #== 1)
                (f DepositSingle)
                ( pif
                    (x #== 2)
                    (f Redeem)
                    ( pif
                        (x #== 3)
                        (f RedeemSingle)
                        ((pif (x #== 4) (f Swap) (f DAOAction)))
                    )
                )
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
                 , "maxDen" ':= PInteger
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

gtPrecision :: Term s PInteger
gtPrecision = pconstant 15

-- ** For Swap, Deposit/Redeem all tokens (Have Clc Postfix in variables name) ** --

gxIndexClc :: Term s PInteger
gxIndexClc = pconstant 0

gyIndexClc :: Term s PInteger
gyIndexClc = pconstant 0

------------------------------------

parseDatum :: ClosedTerm (PDatum :--> BalancePoolConfig)
parseDatum = plam $ \newDatum -> unTermCont $ do
  PDatum poolDatum <- pmatchC newDatum
  tletUnwrap $ ptryFromData @(BalancePoolConfig) $ poolDatum

{-  # Swap operation validation #

    1 - Verifing that invariant value is the same
    2 - Verifing correctness of t'_x and t'_y
    3 - Verifing correctness of final pool balance
 
-}

-- Common task is validate G against T and new token value 
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
validGTAndTokenDeltaWithFees = plam $ \prevTokenBalance tokenWeight tokenDelta tokenG tokenT fees -> -- unTermCont $ do
    let
        correctGandT = verifyGTValues # prevTokenBalance # tokenWeight # tokenG # tokenT
        correctTokenValue = pif
            ( (pmod # pDen # tokenWeight) #== 0 )
            ( ((ppow # tokenG # (pdiv # pDen # tokenWeight)) * feeDen) #== prevTokenBalance * feeDen + tokenDelta * fees)
            ( ((ppow # tokenT # tokenWeight) * feeDen) #== prevTokenBalance * feeDen + tokenDelta * fees)
    in correctGandT #&& correctTokenValue

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

verifyGTValues ::
    ClosedTerm
        (    PInteger
        :--> PInteger
        :--> PInteger
        :--> PInteger
        :--> PBool
        )
verifyGTValues = plam $ \tokenBalance tokenWeight tokenG tokenT -> unTermCont $ do
    let 
        -- Precision will be equals to qty of digits in prevTokenBalance value
        -- todo: should be calculated with greather token value
        -- for test: 7
        tokenPrecision = pIntLength # tokenBalance

        --- 6172836716237612638761237861263671267386712863

        -- (gtPrecision - tokenPrecision) - incorrect for cases when tokenPrecision > 15
        --tokenGNum = pround # (pcon $ PRational tokenG (ptryPositive # (ppow # 10 # (gtPrecision - tokenPrecision))) )

        tokenTPowNum =
            pround # (pcon $ PRational (ppow # tokenT # tokenWeight) (ptryPositive # (ppow # 10 # ((gtPrecision * tokenWeight) - tokenPrecision))))

        -- We should "truncate" tokenGRational and tokenTPowRational to first tokenPrecision digits

        tokenGRationalLength = pIntLength # tokenG

        --todo: check
        finalLeftValue = pround # (pcon $ PRational tokenG (ptryPositive # (ppow # 10 # (tokenGRationalLength - tokenPrecision))))

        tokenTPowNumLength = pIntLength # tokenTPowNum

        finalRightValue = pround # (pcon $ PRational tokenTPowNum (ptryPositive # (ppow # 10 # (tokenTPowNumLength - tokenPrecision)))) --(ptryPositive # (ppow # 10 # (tokenTPowNumLength - tokenPrecision))))

    ptraceC $ "tokenPrecision"
    ptraceC $ pshow tokenPrecision
    ptraceC $ "finalLeftValue"
    ptraceC $ pshow finalLeftValue
    ptraceC $ "tokenTPowNum"
    ptraceC $ pshow tokenTPowNum
    ptraceC $ "(tokenTPowNumLength - tokenPrecision)"
    ptraceC $ pshow (tokenTPowNumLength - tokenPrecision)
    ptraceC $ "ptryPositive # (16)"
    ptraceC $ pshow (ptryPositive # (16))
    -- ptraceC $ "finalRightValue"
    -- ptraceC $ pshow finalRightValue

    pure $ pcon PTrue -- finalLeftValue #== finalRightValue

verifyGEquality ::
    ClosedTerm
        (    PInteger
        :--> PInteger
        :--> PInteger
        :--> PInteger
        :--> PInteger
        :--> PBool
        )
verifyGEquality = plam $ \leftSideMultiplicator rightSideNum prevTokenBalance tokenG tokenWeight -> unTermCont $ do
    let
        tokenBalanceIntLength = pIntLength # prevTokenBalance

        degree = pdiv # pDen # tokenWeight

        leftSideNum   = (ppow # tokenG # degree) * leftSideMultiplicator
        leftSideDenum = ptryPositive # (ppow # 10 # ((pIntLength # leftSideNum) - tokenBalanceIntLength))

        rightSideDen = ptryPositive # (ppow # 10 # ((pIntLength # rightSideNum) - tokenBalanceIntLength))

        leftSide  = pround # (pcon $ PRational leftSideNum leftSideDenum)
        rightSide = pround # (pcon $ PRational rightSideNum rightSideDen)

        gEDiff = leftSide - rightSide
        validGEquality = pif
            ( gEDiff #<= 0 )
            ( (-1) #<= gEDiff )
            ( gEDiff #<= (1) )

    ptraceC $ "tokenBalanceIntLength"
    ptraceC $ pshow $ tokenBalanceIntLength
    ptraceC $ "(ppow # tokenG # degree) * leftSideMultiplicator"
    ptraceC $ pshow $ ((ppow # tokenG # degree) * leftSideMultiplicator)
    ptraceC $ "leftSideNum"
    ptraceC $ pshow $ leftSideNum
    ptraceC $ "rightSideNum"
    ptraceC $ pshow $ rightSideNum
    ptraceC $ "leftSide verifyGEquality"
    ptraceC $ pshow $ leftSide
    ptraceC $ "rightSide verifyGEquality"
    ptraceC $ pshow $ rightSide
    ptraceC $ "validGEquality"
    ptraceC $ pshow $ validGEquality
    pure $ validGEquality

verifyTExpEquality ::
    ClosedTerm
        (    PInteger
        :--> PInteger
        :--> PBool
        )
verifyTExpEquality = plam $ \tokenT rightSide -> unTermCont $ do
    let
        tokenTWeight = ppow # tokenT # 10

        rightLength = pIntLength # rightSide

        leftLength   = pIntLength # tokenTWeight
        letfNewDenum = ppow # 10 # ((pIntLength # tokenTWeight) - rightLength)
        leftRational = pcon $ PRational tokenTWeight (ptryPositive # letfNewDenum)
        leftRounded  = pround # leftRational

    -- ptraceC $ "rightSideNum"
    -- ptraceC $ pshow $ rightSideNum
    -- ptraceC $ "leftRounded verifyTExpEquality"
    -- ptraceC $ pshow $ leftRounded
    -- ptraceC $ "rightSide verifyTExpEquality"
    -- ptraceC $ pshow $ rightSide

    pure $ leftRounded #== rightSide

validGTAndTokenDeltaWithFeesTest ::
    ClosedTerm
        (    PInteger
        :--> PInteger
        :--> PInteger
        :--> PInteger
        :--> PInteger
        :--> PInteger
        :--> PInteger
        :--> PPositive
        :--> PBool
        )
validGTAndTokenDeltaWithFeesTest = plam $ \prevTokenBalance tokenWeight tokenDelta tokenG tokenT fees denOrig den -> unTermCont $ do
    let
        correctGandT = verifyGTValues # (prevTokenBalance + tokenDelta) # tokenWeight # tokenG # tokenT

        correctTokenValue = pif
            ( (pmod # pDen # tokenWeight) #== 0 )
            ( verifyGEquality # feeDen # (prevTokenBalance * feeDen + tokenDelta * fees) # prevTokenBalance # tokenG # tokenWeight )  --( leftSide #== rightSide )
            ( verifyTExpEquality # tokenT # (prevTokenBalance * feeDen + tokenDelta * fees) )

    -- ptraceC $ "(pmod # pDen # tokenWeight) #== 0"
    -- ptraceC $ pshow $ (pmod # pDen # tokenWeight) #== 0
    -- ptraceC $ "((ppow # tokenG # (pdiv # pDen # tokenWeight)) * feeDen)"
    -- ptraceC $ pshow $ ((ppow # tokenG # (pdiv # pDen # tokenWeight)) * feeDen)
    -- ptraceC $ "((ppow # tokenG # (pdiv # pDen # tokenWeight)) * feeDen) with denom"
    -- ptraceC $ pshow $ pround # (pcon $ PRational ((ppow # tokenG # (pdiv # pDen # tokenWeight)) * feeDen) (denomInFirstCaseReal))
    -- ptraceC $ "((ppow # tokenG # (pdiv # pDen # tokenWeight)) * feeDen) with denom length"
    -- ptraceC $ pshow $ pIntLength # (pround # (pcon $ PRational ((ppow # tokenG # (pdiv # pDen # tokenWeight)) * feeDen) (denomInFirstCaseReal)))
    -- ptraceC $ "firstCast"
    -- ptraceC $ pshow $ firstCast
    -- ptraceC $ "prevTokenBalance * feeDen + tokenDelta * fees"
    -- ptraceC $ pshow $ prevTokenBalance * feeDen + tokenDelta * fees
    -- ptraceC $ "prevTokenBalance * feeDen + tokenDelta * fees with denom"
    -- ptraceC $ pshow $ pround # (pcon $ PRational (prevTokenBalance * feeDen + tokenDelta * fees) (denomInSecondCaseReal))
    
    
    -- ptraceC $ "tokenGRational"
    -- ptraceC $ pshow tokenGRational
    -- ptraceC $ "tokenGRational round"
    -- ptraceC $ pshow $ pround # tokenGRational
    -- ptraceC $ "tokenTRational"
    -- ptraceC $ pshow tokenTRational
    -- ptraceC $ "tokenTPowRational"
    -- ptraceC $ pshow tokenTPowRational
    -- ptraceC $ "tokenTPowRational round"
    -- ptraceC $ pshow $ pround # tokenTPowRational
    -- ptraceC $ "correctGandT"
    -- ptraceC $ pshow $ correctGandT
    -- ptraceC $ "firstCaseLeftPartTest #== firstCaseRightPartTest"
    -- ptraceC $ pshow $ (firstCaseLeftPartTest #== firstCaseRightPartTest)
    -- ptraceC $ "correctTokenValue"
    -- ptraceC $ pshow $ correctTokenValue
    pure $ correctGandT #&& correctTokenValue

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
validGTAndTokenDeltaWithoutFees = plam $ \prevTokenBalance tokenWeight tokenDelta tokenG tokenT -> unTermCont $ do
    let
        correctGandT = verifyGTValues # prevTokenBalance # tokenWeight # tokenG # tokenT

        correctTokenValue = pif
            ( (pmod # pDen # tokenWeight) #== 0 )
            ( verifyGEquality # 1 # (prevTokenBalance + tokenDelta) # prevTokenBalance # tokenG # tokenWeight )
            ( verifyTExpEquality # tokenT # (prevTokenBalance + tokenDelta) )

    -- ptraceC $ "tokenGRational"
    -- ptraceC $ pshow $ tokenGRational
    -- ptraceC $ "tokenTRational"
    -- ptraceC $ pshow $ tokenTRational
    -- ptraceC $ "tokenTPowRational"
    -- ptraceC $ pshow $ tokenTPowRational
    -- ptraceC $ "correctGandT"
    -- ptraceC $ pshow $ correctGandT
    -- ptraceC $ "correctGandT without fees"
    -- ptraceC $ pshow $ correctGandT
    -- ptraceC $ "tokenTWeight"
    -- ptraceC $ pshow $ tokenTWeight
    -- ptraceC $ "letfNewDenum"
    -- ptraceC $ pshow $ letfNewDenum
    -- ptraceC $ "leftRounded"
    -- ptraceC $ pshow $ leftRounded
    -- ptraceC $ "(prevTokenBalance + tokenDelta))"
    -- ptraceC $ pshow $ ((prevTokenBalance + tokenDelta))
    -- ptraceC $ "correctTokenValue without fees"
    -- ptraceC $ pshow $ correctTokenValue

    pure $ correctGandT #&& correctTokenValue

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
        :--> PBool
        )
validSwap = plam $ \prevState' newState' prevPoolConfig newPoolConfig newGX newTx newGY newTy maxDen -> unTermCont $ do
    ptraceC $ "in swap"
    prevState  <- pletFieldsC @'["reservesX", "reservesY", "liquidity"] prevState'
    ptraceC $ "prev state"
    newState   <- pletFieldsC @'["reservesX", "reservesY", "liquidity"] newState'
    ptraceC $ "newState"
    prevConfig <- pletFieldsC @'["poolNft", "poolX", "weightX", "poolY", "weightY", "poolLq", "feeNum", "treasuryFee", "treasuryX", "treasuryY", "DAOPolicy", "treasuryAddress", "invariant"] prevPoolConfig
    ptraceC $ "prevConfig"
    newConfig  <- pletFieldsC @'["treasuryX", "treasuryY"] newPoolConfig
    ptraceC $ "newConfig"
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

        -- In case of x -> y swap fee will be withdrawn from y token

        den = ptryPositive # (ppow # 10 # maxDen)

        newGxRational = pcon $ PRational newGX den
        newGyRational = pcon $ PRational newGY den

        -- todo: check

        newInvariantWithoutRound = (newGxRational #* newGyRational)
        newInvariantRational = pround # (newGxRational #* newGyRational)

        -- There is no matter which swap direction is, so we are calculating new invartiant directly
        newInvariant = newGX #* newGY

        -- Verify that new value of invariant equals to previous
        newInvariantIsCorrect = prevInvariant #<= newInvariant -- todo: check for rounding

        correctTokensUpdate =
            pif
                ( zero #< dx )
                ( (validGTAndTokenDeltaWithFeesTest # prevX # weightX # dx # newGX # newTx # (feeNum + treasuryFee) #(ppow # 10 # maxDen) # den) #&& (validGTAndTokenDeltaWithoutFees # prevY # weightY # dy # newGY # newTy) )
                ( (validGTAndTokenDeltaWithoutFees # prevX # weightX # dx # newGX # newTx) #&& (validGTAndTokenDeltaWithFeesTest # prevY # weightY # dy # newGY # newTy # (feeNum + treasuryFee) # (ppow # 10 # maxDen) # den) )

        -- Due to rounding newInvariant should be greather or equals to prevValue
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


-- start x: 4000356
-- nex x:   5557657

-- offchain: 22.334574
-- onchain:  22 334575046809/100000000000000


-- 1000 ^ 0.2 * 1000 ^ 0.8 = x
-- 999.9999999989182

-- 1000 ^ 0.2 * 1000 ^ 0.8 = 999.9999999989182
-- 2000 ^ 0.2 * 2000 ^ 0.8 = 2000.000000000001


-- 999 ^ 0.2 * 1000 ^ 0.8 = x'
-- 999.7999199519668

    -- ptraceC $ "xLength"
    -- ptraceC $ plength $ pshow dx
    -- ptraceC $ "dx"
    -- ptraceC $ pshow dx
    -- ptraceC $ "xTest"
    -- ptraceC $ pshow xTest
    -- ptraceC $ "xTestWithFee"
    -- ptraceC $ pshow xTestWithFee

    -- ptraceC $ "prevInvariant"
    -- ptraceC $ pshow prevInvariant
    -- ptraceC $ "newInvariantWithoutRound"
    -- ptraceC $ pshow newInvariantWithoutRound
    -- ptraceC $ "newInvariantR"
    -- ptraceC $ pshow newInvariantRational
    -- ptraceC $ "newInvariant"
    -- ptraceC $ pshow newInvariant
    
    -- ptraceC $ "newInvariantIsCorrect"
    -- ptraceC $ pshow newInvariantIsCorrect
    -- ptraceC $ "correctTokensUpdate"
    -- ptraceC $ pshow correctTokensUpdate
    -- ptraceC $ "correctTreasuryUpdate"
    -- ptraceC $ pshow correctTreasuryUpdate
    -- ptraceC $ "(newPoolConfig #== newExpectedConfig)"
    -- ptraceC $ pshow (newPoolConfig #== newExpectedConfig)
    -- ptraceC $ "(dlq #== zero)"
    -- ptraceC $ pshow (dlq #== zero)

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

correctLpTokenOut ::
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
correctLpTokenOut = plam $ \lpIssued lpOut tokenIn tokenBalance tokenWeight tokenG tokenT -> unTermCont $ do
    let
        --correctTokenIn = ((pdiv # (lpIssued + lpOut) # lpIssued) - 1) #* tokenBalance

        tokenBalanceIntLength = pIntLength # tokenBalance
        
        correctGandT = verifyGTValues # (tokenBalance + tokenIn) # tokenWeight # tokenG # tokenT
        
        leftLpPartNum = (tokenIn * lpIssued)
        leftLpPartDenum = ptryPositive # (ppow # 10 # ((pIntLength # leftLpPartNum) - tokenBalanceIntLength))

        leftPart = pround # (pcon $ PRational leftLpPartNum leftLpPartDenum)
        
        rightLpPartNum = (tokenBalance * lpOut)
        rightLpDenum = ptryPositive # (ppow # 10 # ((pIntLength # rightLpPartNum) - tokenBalanceIntLength))

        rightPart = pround # (pcon $ PRational rightLpPartNum rightLpDenum)

        leftRightPartDiff = leftPart - rightPart

        -- rounding. double check
        correctTokenIn = pif
            ( leftRightPartDiff #<= 0 )
            ( (-1) #<= leftRightPartDiff )
            ( leftRightPartDiff #<= (1) )
        correctTokenValue = pif
            ( (pmod # pDen # tokenWeight) #== 0 )
            ( verifyGEquality # 1 # (tokenBalance + tokenIn) # tokenBalance # tokenG # tokenWeight )  --( leftSide #== rightSide )
            ( verifyTExpEquality # tokenT # (tokenBalance + tokenIn) )

    ptraceC $ "leftRightPartDiff"
    ptraceC $ pshow $ leftRightPartDiff
    ptraceC $ "correctGandT"
    ptraceC $ pshow $ correctGandT
    ptraceC $ "tokenBalance"
    ptraceC $ pshow $ tokenBalance
    ptraceC $ "tokenIn"
    ptraceC $ pshow $ tokenIn
    ptraceC $ "lpIssued"
    ptraceC $ pshow $ lpIssued
    ptraceC $ "lpOut"
    ptraceC $ pshow $ lpOut
    ptraceC $ "tokenBalanceIntLength"
    ptraceC $ pshow $ tokenBalanceIntLength
    ptraceC $ "leftPart"
    ptraceC $ pshow $ leftPart
    ptraceC $ "leftLpPartNum"
    ptraceC $ pshow $ leftLpPartNum
    ptraceC $ "(pIntLength # leftLpPartNum)"
    ptraceC $ pshow $ (pIntLength # leftLpPartNum)
    ptraceC $ "rightPart"
    ptraceC $ pshow $ rightPart
    ptraceC $ "correctTokenIn"
    ptraceC $ pshow $ correctTokenIn
    -- ptraceC $ "tokenIn"
    -- ptraceC $ pshow $ tokenIn
    ptraceC $ "correctTokenValue"
    ptraceC $ pshow $ correctTokenValue
    pure $ correctTokenIn #&& correctTokenValue

validDepositAllTokens :: 
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
validDepositAllTokens = plam $ \prevState' newState' prevPoolConfig newPoolConfig newGX newTx newGY newTy -> unTermCont $ do
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

        xDepositIsValid = correctLpTokenOut # prevLq # dlq # dx # prevX # weightX # newGX # newTx
        yDepositIsValid = correctLpTokenOut # prevLq # dlq # dy # prevY # weightY # newGY # newTy

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

    ptraceC $ "xDepositIsValid"
    ptraceC $ pshow xDepositIsValid
    ptraceC $ "yDepositIsValid"
    ptraceC $ pshow yDepositIsValid
    ptraceC $ "(newPoolConfig #== newExpectedConfig)"
    ptraceC $ pshow (newPoolConfig #== newExpectedConfig)

    pure $ 
        (   xDepositIsValid
        #&& yDepositIsValid
        #&& newPoolConfig #== newExpectedConfig
        )

-- Single deposit check requires next bunch of g, t variables:
--  1) g and t for full fee (treasury + lp). Will be used for verifying lp issued qty
--  2) g and t for treausry fee. Will be user for verifying final invariant value
singleDepositIsValid ::
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
        :--> PBool
        )
singleDepositIsValid = 
    plam $ \prevState' newState' prevPoolConfig newPoolConfig tokenXGWF tokenYGWF tokenXTWF tokenYTWF tokenXGLP tokenYGLP tokenXTLP tokenYTLP tokenXGT tokenYGT tokenXTT tokenYTT idealDeltaToken -> unTermCont $ do
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

            newTreasuryX = getField @"treasuryX" newConfig
            newTreasuryY = getField @"treasuryY" newConfig

            prevX  = pfromData $ getField @"reservesX" prevState
            prevY  = pfromData $ getField @"reservesY" prevState
            prevLq = pfromData $ getField @"liquidity" prevState

            newX  = pfromData $ getField @"reservesX" newState
            newY  = pfromData $ getField @"reservesY" newState
            newLq = pfromData $ getField @"liquidity" newState

            dx  = newX - prevX
            dy  = newY - prevY
            dlq = newLq - prevLq

        -- first of all - verifing correctness of invariant without any fees
        -- 1) verify that delta in deposit token is correct
            validDeltaInToken =
                pif
                    (dx #== zero)
                    -- Deposit token is Y
                    ((validGTAndTokenDeltaWithoutFees # prevX # weightX # zero # tokenXGWF # tokenXTWF) #&& (validGTAndTokenDeltaWithoutFees # prevY # weightY # dy # tokenYGWF # tokenYTWF))
                    -- Deposit token is X
                    ((validGTAndTokenDeltaWithoutFees # prevX # weightX # dx # tokenXGWF # tokenXTWF) #&& (validGTAndTokenDeltaWithoutFees # prevY # weightY # zero # tokenYGWF # tokenYTWF))
        -- 2) We should calculate invariant for this single deposit to verify it with ideal invariant
            invariantWithoutFees = tokenXGWF #* tokenYGWF
        
        -- Second step is verifying correctness of values associated with ideal deposit calculations.
        -- Used for verifying correcntess of lp out.
        -- 1) verify correctness of ideal invariant and withoutFeesInvariant
            idealInvariant = tokenXGLP #* tokenYGLP

            idealAndWFInvariantAreEquals = invariantWithoutFees #== idealInvariant

        -- 2) verify correctness of ideal delta in deposit token
            validIdealTokenDelta =
                pif (dx #== zero)
                    -- Deposit token is Y
                    (validGTAndTokenDeltaWithoutFees # prevY # weightY # idealDeltaToken # tokenYGLP # tokenYTLP)
                    -- Deposit token is X
                    (validGTAndTokenDeltaWithoutFees # prevX # weightX # idealDeltaToken # tokenXGLP # tokenXTLP)
        -- 3) verify correctness of ideal delta with protocol fees (lp + treasury)
            validIdealTokenDeltaWithFees =
                pif (dx #== zero)
                    -- Deposit token is Y
                    (validGTAndTokenDeltaWithFees # prevY # weightY # idealDeltaToken # tokenYGLP # tokenYTLP # (feeNum + treasuryFee))
                    -- Deposit token is X
                    (validGTAndTokenDeltaWithFees # prevX # weightX # idealDeltaToken # tokenXGLP # tokenXTLP # (feeNum + treasuryFee))

        -- Third step is verifying correctness of values associated with final invariant.
        -- Only treasury fee.
            validIdealTokenDeltaWithLPTreasuryFees =
                pif (dx #== zero)
                    -- Deposit token is Y
                    (validGTAndTokenDeltaWithFees # prevY # weightY # idealDeltaToken # tokenYGT # tokenYTT # treasuryFee)
                    -- Deposit token is X
                    (validGTAndTokenDeltaWithFees # prevX # weightX # idealDeltaToken # tokenXGT # tokenXTT # treasuryFee)

            finalInvariant = tokenXGT * tokenYGT

            correctTreasuryUpdate =
                pif
                    ( zero #< dx )
                    ( ((newTreasuryX * feeDen) #== (prevTreasuryX + (idealDeltaToken * treasuryFee))) #&& (prevTreasuryY #== newTreasuryY) )
                    ( ((newTreasuryY * feeDen) #== (prevTreasuryY + (idealDeltaToken * treasuryFee))) #&& (prevTreasuryX #== newTreasuryX) )

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
                    #$ pdcons @"invariant" @PInteger # pdata finalInvariant
                    # pdnil
                )

        pure $
            (   validDeltaInToken
            #&& idealAndWFInvariantAreEquals
            #&& validIdealTokenDelta
            #&& validIdealTokenDeltaWithFees
            #&& validIdealTokenDeltaWithLPTreasuryFees
            #&& correctTreasuryUpdate
            #&& (newExpectedConfig #== newPoolConfig)
            )

singleRedeemIsValid ::
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
        :--> PBool
        )
singleRedeemIsValid = 
    plam $ \prevState' newState' prevPoolConfig newPoolConfig tokenXGWF tokenYGWF tokenXTWF tokenYTWF tokenXGLP tokenYGLP tokenXTLP tokenYTLP tokenXGT tokenYGT tokenXTT tokenYTT -> unTermCont $ do
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

            newTreasuryX = getField @"treasuryX" newConfig
            newTreasuryY = getField @"treasuryY" newConfig

            prevX  = pfromData $ getField @"reservesX" prevState
            prevY  = pfromData $ getField @"reservesY" prevState
            prevLq = pfromData $ getField @"liquidity" prevState

            newX  = pfromData $ getField @"reservesX" newState
            newY  = pfromData $ getField @"reservesY" newState
            newLq = pfromData $ getField @"liquidity" newState

            dx  = newX - prevX
            dy  = newY - prevY
            dlq = newLq - prevLq

        --todo: fix

        pure $ pconstant False
             
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

validRedeemAllTokens ::
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
validRedeemAllTokens = plam $ \prevState' newState' prevPoolConfig newPoolConfig newGX newTx newGY newTy -> unTermCont $ do
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

        xRedeemIsValid = correctLpTokenOut # prevLq # dlq # (-dx) # prevX # weightX # newGX # newTx
        yRedeemIsValid = correctLpTokenOut # prevLq # dlq # (-dy) # prevY # weightY # newGY # newTy

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

    ptraceC $ "xRedeemIsValid"
    ptraceC $ pshow xRedeemIsValid
    -- ptraceC $ "yRedeemIsValid"
    -- ptraceC $ pshow yRedeemIsValid
    ptraceC $ "(newPoolConfig #== newExpectedConfig)"
    ptraceC $ pshow (newPoolConfig #== newExpectedConfig)

    pure $ 
        (   xRedeemIsValid
        -- #&& yRedeemIsValid
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
    ptraceC $ "contract init"
    redeemer <- pletFieldsC @'["action", "selfIx", "g", "t", "maxDen"] redeemer'
    ptraceC $ "redeemer parsed"
    let
        selfIx = getField @"selfIx" redeemer
        action = getField @"action" redeemer

        gList = getField @"g" redeemer
        tList = getField @"t" redeemer

        maxDex = getField @"maxDen" redeemer

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
    ptraceC $ "succDatum parsed"
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
    ptraceC $ "selfIdentity"
    ptraceC $ pshow selfIdentity
    pure $
        selfIdentity #&& (pmatch action $ \case
            Swap    -> unTermCont $ do
                ptraceC $ "Swap"
                gx <- tletUnwrap $ phead # gList
                gy <- tletUnwrap $ pelemAt # (pconstant 1) # gList
                tx <- tletUnwrap $ phead # tList
                ty <- tletUnwrap $ pelemAt # (pconstant 1) # tList
                pure $ noMoreTokens #&& scriptPreserved #&& (validSwap # s0 # s1 # conf # newConfig # gx # tx # gy # ty # maxDex)
            Deposit -> unTermCont $ do
                ptraceC $ "Deposit"
                gx <- tletUnwrap $ phead # gList
                gy <- tletUnwrap $ pelemAt # (pconstant 1) # gList
                tx <- tletUnwrap $ phead # tList
                ty <- tletUnwrap $ pelemAt # (pconstant 1) # tList
                pure $ noMoreTokens #&& scriptPreserved #&& (validDepositAllTokens # s0 # s1 # conf # newConfig # gx # tx # gy # ty)
            DepositSingle -> unTermCont $ do
                ptraceC $ "DepositSingle"
                gXWF <- tletUnwrap $ phead # gList
                tXWF <- tletUnwrap $ phead # tList
                gYWF <- tletUnwrap $ pelemAt # (pconstant 1) # gList
                tYWF <- tletUnwrap $ pelemAt # (pconstant 1) # tList

                gXLP <- tletUnwrap $ pelemAt # (pconstant 2) # gList
                gYLP <- tletUnwrap $ pelemAt # (pconstant 3) # gList
                tXLP <- tletUnwrap $ pelemAt # (pconstant 2) # tList
                tYLP <- tletUnwrap $ pelemAt # (pconstant 3) # tList

                gXT <- tletUnwrap $ pelemAt # (pconstant 4) # gList
                gYT <- tletUnwrap $ pelemAt # (pconstant 5) # gList
                tXT <- tletUnwrap $ pelemAt # (pconstant 4) # tList
                tYT <- tletUnwrap $ pelemAt # (pconstant 5) # tList
                
                -- temporal solution
                idealDeposit <- tletUnwrap $ pelemAt # (pconstant 6) # gList

                pure $ noMoreTokens #&& scriptPreserved #&& (singleDepositIsValid # s0 # s1 # conf # newConfig # gXWF # tXWF # tYWF # gYWF # gXLP # tXLP # tYLP # gYLP # gXT # tXT # tYT # gYT # idealDeposit)
            Redeem  -> unTermCont $ do
                ptraceC $ "Redeem"
                gx <- tletUnwrap $ phead # gList
                gy <- tletUnwrap $ pelemAt # (pconstant 1) # gList
                tx <- tletUnwrap $ phead # tList
                ty <- tletUnwrap $ pelemAt # (pconstant 1) # tList
                pure $ noMoreTokens #&& scriptPreserved #&& (validRedeemAllTokens # s0 # s1 # conf # newConfig # gx # tx # gy # ty)
            RedeemSingle -> unTermCont $ do
                ptraceC $ "RedeemSingle"
                gXWF <- tletUnwrap $ phead # gList
                tXWF <- tletUnwrap $ phead # tList
                gYWF <- tletUnwrap $ pelemAt # (pconstant 1) # gList
                tYWF <- tletUnwrap $ pelemAt # (pconstant 1) # tList

                gXLP <- tletUnwrap $ pelemAt # (pconstant 2) # gList
                gYLP <- tletUnwrap $ pelemAt # (pconstant 3) # gList
                tXLP <- tletUnwrap $ pelemAt # (pconstant 2) # tList
                tYLP <- tletUnwrap $ pelemAt # (pconstant 3) # tList

                gXT <- tletUnwrap $ pelemAt # (pconstant 4) # gList
                gYT <- tletUnwrap $ pelemAt # (pconstant 5) # gList
                tXT <- tletUnwrap $ pelemAt # (pconstant 4) # tList
                tYT <- tletUnwrap $ pelemAt # (pconstant 5) # tList
                pure $ noMoreTokens #&& scriptPreserved #&& (singleRedeemIsValid # s0 # s1 # conf # newConfig # gXWF # tXWF # tYWF # gYWF # gXLP # tXLP # tYLP # gYLP # gXT # tXT # tYT # gYT)
            DAOAction -> validDAOAction # conf # txinfo'
        )