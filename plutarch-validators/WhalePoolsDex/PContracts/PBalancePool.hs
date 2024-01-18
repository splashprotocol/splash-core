{-# LANGUAGE UndecidableInstances #-}

module WhalePoolsDex.PContracts.PBalancePool where

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
import Plutarch.Rational
import Plutarch.Num                 ((#*))

import PExtra.API                   (PAssetClass, assetClassValueOf, ptryFromData, assetClass, pValueLength, tletUnwrap)
import PExtra.List                  (pelemAt)
import PExtra.Monadic               (tcon, tlet, tletField, tmatch)
import PExtra.Pair
import PExtra.Integer

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
                 , "DAOPolicy"        ':= PBuiltinList (PAsData PCurrencySymbol)
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
        correctGandT = tokenG #== (ppow # tokenT # tokenWeight)
        correctTokenValue = pif
            ( (pmod # pDen # tokenWeight) #== 0 )
            ( ((ppow # tokenG # (pdiv # pDen # tokenWeight)) * feeDen) #== prevTokenBalance * feeDen + tokenDelta * fees)
            ( ((ppow # tokenT # tokenWeight) * feeDen) #== prevTokenBalance * feeDen + tokenDelta * fees)
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
validGTAndTokenDeltaWithoutFees = plam $ \prevTokenBalance tokenWeight tokenDelta tokenG tokenT -> -- unTermCont $ do
    let
        correctGandT = tokenG #== (ppow # tokenT # tokenWeight)
        correctTokenValue = pif
            ( (pmod # pDen # tokenWeight) #== 0 )
            ( ((ppow # tokenG # (pdiv # pDen # tokenWeight))) #== (prevTokenBalance + tokenDelta) )
            ( ((ppow # tokenT # tokenWeight)) #== (prevTokenBalance + tokenDelta) )
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

        -- In case of x -> y swap fee will be withdrawn from y token

        -- There is no matter which swap direction is, so we are calculating new invartiant directly
        newInvariant = newGX #* newGY

        -- Verify that new value of invariant equals to previous
        newInvariantIsCorrect = prevInvariant #== newInvariant -- todo: check for rounding

        correctTokensUpdate =
            pif
                ( zero #< dx )
                ( (validGTAndTokenDeltaWithFees  # prevX # weightX # dx # newGX # newTx # (feeNum + treasuryFee)) #&& (validGTAndTokenDeltaWithoutFees # prevY # weightY # dy # newGY # newTy) )
                ( (validGTAndTokenDeltaWithoutFees # prevX # weightX # dx # newGX # newTx) #&& (validGTAndTokenDeltaWithFees # prevY # weightY # dy # newGY # newTy # (feeNum + treasuryFee)) )

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
                #$ pdcons @"DAOPolicy" @(PBuiltinList (PAsData PCurrencySymbol)) # pdata prevDAOPolicy
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

-- validDAOAction :: ClosedTerm (BalancePoolConfig :--> PTxInfo :--> PBool)
-- validDAOAction = plam $ \cfg txInfo -> unTermCont $ do
--   valueMint <- tletField @"mint" txInfo
--   policies  <- tletField @"DAOPolicy" cfg
--   let 
--       policyCS = pfromData $ phead # policies
--       mintedAc = assetClass # policyCS # poolStakeChangeMintTokenNameP
--   pure $ assetClassValueOf # valueMint # mintedAc #== 1

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
correctLpTokenOut = plam $ \lpIssued lpOut tokenIn tokenBalance tokenWeight tokenG tokenT ->
    let
        correctTokenIn  = ((pdiv # (lpIssued + lpOut) # lpIssued) - 1) #* tokenBalance
        correctTokenDelta = 
            pif
                ( (pmod # pDen # tokenWeight) #== 0 )
                ( tokenIn #== (ppow # tokenG # (pdiv # pDen # tokenWeight)) - tokenBalance )
                ( tokenIn #== (ppow # tokenT # tokenWeight) - tokenBalance )
    in tokenIn #== correctTokenIn #&& correctTokenDelta

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

        -- Verify that provided G0 / G1 corresponds to provided newTX/Y
        newGXandNewTxIsCorrect = newGX #== (ppow # newTx # weightX)
        newGYandNewTyIsCorrect = newGY #== (ppow # newTy # weightY)

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
                #$ pdcons @"DAOPolicy" @(PBuiltinList (PAsData PCurrencySymbol)) # pdata prevDAOPolicy
                #$ pdcons @"treasuryAddress" @PValidatorHash # pdata prevTreasuryAddress
                #$ pdcons @"invariant" @PInteger # pdata newInvariant
                    # pdnil)

    pure $ 
        (   xDepositIsValid
        #&& yDepositIsValid
        #&& newGXandNewTxIsCorrect
        #&& newGYandNewTyIsCorrect
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
                    #$ pdcons @"DAOPolicy" @(PBuiltinList (PAsData PCurrencySymbol)) # pdata prevDAOPolicy
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

        xDepositIsValid = correctLpTokenOut # prevLq # dlq # (-dx) # prevX # weightX # newGX # newTx
        yDepositIsValid = correctLpTokenOut # prevLq # dlq # (-dy) # prevY # weightY # newGY # newTy

        -- Verify that provided G0 / G1 corresponds to provided newTX/Y
        newGXandNewTxIsCorrect = newGX #== (ppow # newTx # weightX)
        newGYandNewTyIsCorrect = newGY #== (ppow # newTy # weightY)

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
                #$ pdcons @"DAOPolicy" @(PBuiltinList (PAsData PCurrencySymbol)) # pdata prevDAOPolicy
                #$ pdcons @"treasuryAddress" @PValidatorHash # pdata prevTreasuryAddress
                #$ pdcons @"invariant" @PInteger # pdata newInvariant
                    # pdnil)
    
    pure $ 
        (   xDepositIsValid
        #&& yDepositIsValid
        #&& newGXandNewTxIsCorrect
        #&& newGYandNewTyIsCorrect
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
    redeemer <- pletFieldsC @'["action", "selfIx", "g", "t"] redeemer'
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
            Deposit -> unTermCont $ do
                gx <- tletUnwrap $ phead # gList
                gy <- tletUnwrap $ pelemAt # (pconstant 1) # gList
                tx <- tletUnwrap $ phead # tList
                ty <- tletUnwrap $ pelemAt # (pconstant 1) # tList
                pure $ noMoreTokens #&& scriptPreserved #&& (validDepositAllTokens # s0 # s1 # conf # newConfig # gx # tx # gy # ty)
            DepositSingle -> unTermCont $ do
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
                gx <- tletUnwrap $ phead # gList
                gy <- tletUnwrap $ pelemAt # (pconstant 1) # gList
                tx <- tletUnwrap $ phead # tList
                ty <- tletUnwrap $ pelemAt # (pconstant 1) # tList
                pure $ noMoreTokens #&& scriptPreserved #&& (validRedeemAllTokens # s0 # s1 # conf # newConfig # gx # tx # gy # ty)
            RedeemSingle -> undefined
            DAOAction -> undefined -- validDAOAction # conf # txinfo'
        )