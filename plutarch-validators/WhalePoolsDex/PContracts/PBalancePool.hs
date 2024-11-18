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
import PExtra.Ada

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
                 , "poolY"            ':= PAssetClass
                 , "poolLq"           ':= PAssetClass
                 , "feeNum"           ':= PInteger
                 , "treasuryFee"      ':= PInteger
                 , "treasuryX"        ':= PInteger
                 , "treasuryY"        ':= PInteger
                 , "DAOPolicy"        ':= PBuiltinList (PAsData PStakingCredential)
                 , "treasuryAddress"  ':= PValidatorHash
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
                  -- in T2T pool contains Lovelace qty, in N2T case contains `0`
                 , "lovelaceToken2Token" ':= PInteger
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
    
validSwap :: 
    ClosedTerm 
        (    BalancePoolState 
        :--> BalancePoolState 
        :--> BalancePoolConfig 
        :--> BalancePoolConfig
        :--> PBool
        )
validSwap = plam $ \prevState' newState' prevPoolConfig newPoolConfig -> unTermCont $ do
    prevState  <- pletFieldsC @'["reservesX", "reservesY", "liquidity"] prevState'
    newState   <- pletFieldsC @'["reservesX", "reservesY", "liquidity"] newState'
    prevConfig <- pletFieldsC @'["poolNft", "poolX", "poolY", "poolLq", "feeNum", "treasuryFee", "treasuryX", "treasuryY", "DAOPolicy", "treasuryAddress"] prevPoolConfig
    newConfig  <- pletFieldsC @'["treasuryX", "treasuryY"] newPoolConfig
    let
        prevPoolNft = getField @"poolNft" prevConfig
        prevPoolX   = getField @"poolX"  prevConfig
        prevPoolY   = getField @"poolY"  prevConfig
        prevPoolLq  = getField @"poolLq" prevConfig
        feeNum      = getField @"feeNum" prevConfig
        treasuryFee = getField @"treasuryFee" prevConfig
        prevTreasuryX = getField @"treasuryX" prevConfig
        prevTreasuryY = getField @"treasuryY" prevConfig
        prevDAOPolicy = getField @"DAOPolicy" prevConfig
        prevTreasuryAddress = getField @"treasuryAddress" prevConfig

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

    let
        fullFeeNum = feeNum - treasuryFee

        currentInvariant = prevX * (prevY * prevY * prevY * prevY)

        newXPart =
            pif
                (zero #< dx)
                (prevX + (pdiv # (dx * fullFeeNum) # feeDen))
                (prevX + dx)

        newYPart =
            pif
                (zero #< dx)
                (prevY + dy)
                (prevY + pdiv # (dy * fullFeeNum) # feeDen)

        newInvariant = newXPart * (newYPart * newYPart * newYPart * newYPart)

        correctTreasuryUpdate =
            pif
                (zero #< dx)
                (newTreasuryX #== (prevTreasuryX + (pdiv # (dx * treasuryFee) # feeDen)))
                (newTreasuryY #== (prevTreasuryY + (pdiv # (dy * treasuryFee) # feeDen)))

        anotherTokenTreasuryCorrect =
            pif
                (zero #< dx)
                (prevTreasuryY #== newTreasuryY)
                (prevTreasuryX #== newTreasuryX)

    correctInv <- tlet $ currentInvariant #<= newInvariant

    newExpectedConfig <-
        tcon $ (BalancePoolConfig $
            pdcons @"poolNft" @PAssetClass # pdata prevPoolNft
                #$ pdcons @"poolX" @PAssetClass # pdata prevPoolX
                #$ pdcons @"poolY" @PAssetClass # pdata prevPoolY
                #$ pdcons @"poolLq" @PAssetClass # pdata prevPoolLq
                #$ pdcons @"feeNum" @PInteger # pdata feeNum
                #$ pdcons @"treasuryFee" @PInteger # pdata treasuryFee
                #$ pdcons @"treasuryX" @PInteger # pdata newTreasuryX
                #$ pdcons @"treasuryY" @PInteger # pdata newTreasuryY
                #$ pdcons @"DAOPolicy" @(PBuiltinList (PAsData PStakingCredential)) # pdata prevDAOPolicy
                #$ pdcons @"treasuryAddress" @PValidatorHash # pdata prevTreasuryAddress
                    # pdnil)

    pure $
        (   correctInv 
        #&& correctTreasuryUpdate 
        #&& anotherTokenTreasuryCorrect 
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
    prevConfig <- pletFieldsC @'["poolNft", "poolX", "poolY", "poolLq", "feeNum", "treasuryFee", "treasuryX", "treasuryY", "DAOPolicy", "treasuryAddress"] prevPoolConfig
    let
        prevPoolNft = getField @"poolNft" prevConfig
        prevPoolX   = getField @"poolX"  prevConfig
        prevPoolY   = getField @"poolY"  prevConfig
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

    dx  <- tlet $ newX - prevX
    dy  <- tlet $ newY - prevY
    dlq <- tlet $ newLq - prevLq

    let
        xDepositRedeemIsValid = correctLpTokenDelta # prevLq # dlq # dx # prevX
        yDepositRedeemIsValid = correctLpTokenDelta # prevLq # dlq # dy # prevY

    newExpectedConfig <-
        tcon $ (BalancePoolConfig $
            pdcons @"poolNft" @PAssetClass # pdata prevPoolNft
                #$ pdcons @"poolX" @PAssetClass # pdata prevPoolX
                #$ pdcons @"poolY" @PAssetClass # pdata prevPoolY
                #$ pdcons @"poolLq" @PAssetClass # pdata prevPoolLq
                #$ pdcons @"feeNum" @PInteger # pdata feeNum
                #$ pdcons @"treasuryFee" @PInteger # pdata treasuryFee
                #$ pdcons @"treasuryX" @PInteger # pdata prevTreasuryX
                #$ pdcons @"treasuryY" @PInteger # pdata prevTreasuryY
                #$ pdcons @"DAOPolicy" @(PBuiltinList (PAsData PStakingCredential)) # pdata prevDAOPolicy
                #$ pdcons @"treasuryAddress" @PValidatorHash # pdata prevTreasuryAddress
                    # pdnil)

    pure $ 
        (   xDepositRedeemIsValid
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
            lovelaceToken2Token =
                pif
                    ((poolX #== pAdaAssetClass) #|| (poolY #== pAdaAssetClass))
                    (pconstant 0)
                    (assetClassValueOf # value # pAdaAssetClass)
        tcon $
            BalancePoolState $
                pdcons @"reservesX" @PInteger # pdata (x - poolXTreasury)
                    #$ pdcons @"reservesY" @PInteger # pdata (y - poolYTreasury)
                    #$ pdcons @"liquidity" @PInteger # lq
                    #$ pdcons @"lovelaceToken2Token" @PInteger # pdata lovelaceToken2Token
                        # pdnil

balancePoolValidatorT :: ClosedTerm (BalancePoolConfig :--> BalancePoolRedeemer :--> PScriptContext :--> PBool)
balancePoolValidatorT = plam $ \conf redeemer' ctx' -> unTermCont $ do
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

    lovelaceToken2Token0 <- tletField @"lovelaceToken2Token" s0
    lovelaceToken2Token1 <- tletField @"lovelaceToken2Token" s1
    let 
        scriptPreserved = succAddr #== selfAddr 
        selfValueLength = pValueLength # selfValue
        succesorValueLength = pValueLength # succesorValue

        noMoreTokens = selfValueLength #== succesorValueLength

        newConfig    = parseDatum # succD

        correctLovelaceToken2TokenValue = lovelaceToken2Token1 #== lovelaceToken2Token0

    pure $
        correctLovelaceToken2TokenValue #&& selfIdentity #&& (pmatch action $ \case
            Swap    -> unTermCont $ do
                pure $ noMoreTokens #&& scriptPreserved #&& (validSwap # s0 # s1 # conf # newConfig)
            DAOAction -> validDAOAction # conf # txinfo'
            _ ->         noMoreTokens #&& scriptPreserved #&& (validDepositRedeemAllTokens # s0 # s1 # conf # newConfig)
        )
