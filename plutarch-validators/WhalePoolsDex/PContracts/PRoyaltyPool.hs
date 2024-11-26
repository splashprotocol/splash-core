{-# LANGUAGE UndecidableInstances #-}

module WhalePoolsDex.PContracts.PRoyaltyPool (
    RoyaltyPoolConfig (..),
    PoolAction (..),
    PoolRedeemer (..),
    PoolState(..),
    findPoolOutput,
    poolValidatorT,
    parseDatum,
    readPoolState,
    extractPoolConfig
) where

import qualified GHC.Generics as GHC
import           Generics.SOP (Generic, I (I))

import Plutarch
import Plutarch.Api.V2              (PScriptHash(..), PMaybeData (..), PTxOut, POutputDatum(..), PAddress(..), PPubKeyHash(..), PDatum(..), PValue(..), KeyGuarantees(..), AmountGuarantees(..), PCurrencySymbol(..), PStakingCredential(..), PTxInInfo(..), scriptHash)
import Plutarch.Api.V1              (PCredential (PScriptCredential))
import Plutarch.Api.V2.Contexts     (PScriptContext, PScriptPurpose (PSpending), PTxInfo(..))
import Plutarch.DataRepr
import Plutarch.Lift
import Plutarch.Prelude
import Plutarch.Extra.TermCont
import Plutarch.Builtin             (pasInt, pforgetData, pfromData, pdata, PIsData(..))
import Plutarch.Unsafe              (punsafeCoerce)
import Plutarch.Internal.PlutusType (PInner, PlutusType, pcon', pmatch')
import Plutarch.TryFrom             (PTryFrom (PTryFromExcess, ptryFrom'))
import Plutarch.Api.V1.Scripts
import Plutarch.Api.V1.AssocMap     as Map
import Plutarch.Extra.Maybe         as Maybe
import PlutusTx.Builtins.Internal
import PlutusLedgerApi.V2 hiding    (getValue)

import PExtra.API                   (PAssetClass, assetClassValueOf, ptryFromData, assetClass, pValueLength)
import PExtra.List                  (pelemAt)
import PExtra.Monadic               (tcon, tlet, tletField, tmatch)
import PExtra.Ada
import PlutusLedgerApi.V1.Address
import PlutusLedgerApi.V1.Credential

import qualified WhalePoolsDex.Contracts.RoyaltyPool  as P
import           WhalePoolsDex.PContracts.PApi        (burnLqInitial, feeDen, maxLqCap, tletUnwrap, zero, containsSignature)
import           WhalePoolsDex.PConstants

import Plutarch.Trace
import Data.Text                    as T
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16  as Hex
import qualified Data.Text.Encoding      as E

royaltyWithdrawPoolScriptHash :: BuiltinByteString
royaltyWithdrawPoolScriptHash = BuiltinByteString $ mkByteString . T.pack $ "b7f10508f0bca230b30172c17ae98fc11846ff573b4b91b4ea41ffc6"

royaltyStakeCred :: Term s PStakingCredential
royaltyStakeCred = pconstant (StakingHash . ScriptCredential . ValidatorHash $ royaltyWithdrawPoolScriptHash)

newtype RoyaltyPoolConfig (s :: S)
    = RoyaltyPoolConfig
        ( Term
            s
            ( PDataRecord
                '[ "poolNft"           ':= PAssetClass
                 , "poolX"             ':= PAssetClass
                 , "poolY"             ':= PAssetClass
                 , "poolLq"            ':= PAssetClass
                 , "feeNum"            ':= PInteger
                 , "treasuryFee"       ':= PInteger
                 , "royaltyFee"        ':= PInteger
                 , "treasuryX"         ':= PInteger
                 , "treasuryY"         ':= PInteger
                 , "royaltyX"          ':= PInteger
                 , "royaltyY"          ':= PInteger
                 , "DAOPolicy"         ':= PBuiltinList (PAsData PStakingCredential)
                 , "treasuryAddress"   ':= PValidatorHash
                 , "royaltyPubKey"     ':= PByteString
                 , "nonce"             ':= PInteger
                 ]
            )
        )
    deriving stock (GHC.Generic)
    deriving
        (PIsData, PDataFields, PlutusType, PEq)

instance DerivePlutusType RoyaltyPoolConfig where type DPTStrat _ = PlutusTypeData

instance PUnsafeLiftDecl RoyaltyPoolConfig where type PLifted RoyaltyPoolConfig = P.RoyaltyPoolConfig
deriving via (DerivePConstantViaData P.RoyaltyPoolConfig RoyaltyPoolConfig) instance (PConstantDecl P.RoyaltyPoolConfig)

instance PTryFrom PData (PAsData RoyaltyPoolConfig)

data PoolAction (s :: S) = Deposit | Redeem | Swap | DAOAction | WithdrawRoyalty

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
    pcon' DAOAction = 3
    pcon' WithdrawRoyalty = 4

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
                    ( pif (x #== 3) (f DAOAction) (f WithdrawRoyalty)) 
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
                 -- in T2T pool contains Lovelace qty, in N2T case contains `0`
                 , "lovelaceToken2Token" ':= PInteger
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

extractPoolConfig :: Term s (PTxOut :--> RoyaltyPoolConfig)
extractPoolConfig = plam $ \txOut -> unTermCont $ do
  txOutDatum <- tletField @"datum" txOut

  POutputDatum txOutOutputDatum <- pmatchC txOutDatum

  rawDatum <- tletField @"outputDatum" txOutOutputDatum

  PDatum poolDatum <- pmatchC rawDatum

  tletUnwrap $ ptryFromData @(RoyaltyPoolConfig) $ poolDatum

readPoolState :: Term s (RoyaltyPoolConfig :--> PTxOut :--> PoolState)
readPoolState = phoistAcyclic $
    plam $ \conf' out -> unTermCont $ do
        conf  <- pletFieldsC @'["poolX", "poolY", "poolLq", "treasuryX", "treasuryY", "royaltyX", "royaltyY"] conf'
        let
            poolX  = getField @"poolX"  conf
            poolY  = getField @"poolY"  conf
            poolLq = getField @"poolLq" conf

            poolXTreasury = getField @"treasuryX" conf
            poolYTreasury = getField @"treasuryY" conf

            poolXRoyalty = getField @"royaltyX" conf
            poolYRoyalty = getField @"royaltyY" conf

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
            PoolState $
                pdcons @"reservesX" @PInteger # pdata (x - poolXTreasury - poolXRoyalty)
                    #$ pdcons @"reservesY" @PInteger # pdata (y - poolYTreasury - poolYRoyalty)
                    #$ pdcons @"liquidity" @PInteger # lq
                    #$ pdcons @"lovelaceToken2Token" @PInteger # pdata lovelaceToken2Token
                        # pdnil

correctSwapConfig :: Term s (RoyaltyPoolConfig :--> RoyaltyPoolConfig :--> PInteger :--> PInteger :--> PBool)
correctSwapConfig = plam $ \prevDatum newDatum dx dy -> unTermCont $ do
  prevConfig <- pletFieldsC @'["poolNft", "poolX", "poolY", "poolLq", "feeNum", "treasuryFee", "royaltyFee", "treasuryX", "treasuryY", "royaltyX", "royaltyY", "DAOPolicy", "treasuryAddress", "royaltyPubKey", "nonce"] prevDatum
  newConfig  <- pletFieldsC @'["treasuryX", "treasuryY", "royaltyX", "royaltyY"] newDatum
  
  let
    prevPoolNft = getField @"poolNft" prevConfig
    prevPoolX   = getField @"poolX"  prevConfig
    prevPoolY   = getField @"poolY"  prevConfig
    prevPoolLq  = getField @"poolLq" prevConfig
    prevFeeNum  = getField @"feeNum" prevConfig
    prevTreasuryFeeNum = getField @"treasuryFee" prevConfig
    prevRoyaltyFeeNum  = getField @"royaltyFee"  prevConfig
    prevTreasuryX = getField @"treasuryX" prevConfig
    prevTreasuryY = getField @"treasuryY" prevConfig
    prevRoyaltyX = getField @"royaltyX" prevConfig
    prevRoyaltyY = getField @"royaltyY" prevConfig
    prevDAOPolicy = getField @"DAOPolicy" prevConfig
    prevTreasuryAddress = getField @"treasuryAddress" prevConfig
    prevroyaltyPubKey = getField @"royaltyPubKey" prevConfig
    prevnonce = getField @"nonce" prevConfig

    newTreasuryX = getField @"treasuryX" newConfig
    newTreasuryY = getField @"treasuryY" newConfig
    newRoyaltyX  = getField @"royaltyX" newConfig
    newRoyaltyY  = getField @"royaltyY" newConfig

    dt = 
      pif
        (zero #< dx)
        (newTreasuryX - prevTreasuryX)
        (newTreasuryY - prevTreasuryY)

    c2t = 
      pif
        (zero #< dx)
        (dx * prevTreasuryFeeNum)
        (dy * prevTreasuryFeeNum)
        
    dr = 
      pif
        (zero #< dx)
        (newRoyaltyX - prevRoyaltyX)
        (newRoyaltyY - prevRoyaltyY)

    c2r = 
      pif
        (zero #< dx)
        (dx * prevRoyaltyFeeNum)
        (dy * prevRoyaltyFeeNum)
        
    validTreasuryAndRoyaltyChange = (feeDen * dr #<= c2r) #&& (c2r #< feeDen * (dr + 1)) #&& (feeDen * dt #<= c2t) #&& (c2t #< feeDen * (dt + 1))

    anotherTokenTreasuryAndRoyaltyCorrect =
      pif
        (zero #< dx)
        ((prevTreasuryY #== newTreasuryY) #&& (prevRoyaltyY #== newRoyaltyY))
        ((prevTreasuryX #== newTreasuryX) #&& (prevRoyaltyX #== newRoyaltyX))

  expectedConfig <-
        tcon $ (RoyaltyPoolConfig $
            pdcons @"poolNft" @PAssetClass # pdata prevPoolNft
                #$ pdcons @"poolX" @PAssetClass # pdata prevPoolX
                #$ pdcons @"poolY" @PAssetClass # pdata prevPoolY
                #$ pdcons @"poolLq" @PAssetClass # pdata prevPoolLq
                #$ pdcons @"feeNum" @PInteger # pdata prevFeeNum
                #$ pdcons @"treasuryFee" @PInteger # pdata prevTreasuryFeeNum
                #$ pdcons @"royaltyFee" @PInteger # pdata prevRoyaltyFeeNum
                #$ pdcons @"treasuryX" @PInteger # pdata newTreasuryX
                #$ pdcons @"treasuryY" @PInteger # pdata newTreasuryY
                #$ pdcons @"royaltyX" @PInteger # pdata newRoyaltyX
                #$ pdcons @"royaltyY" @PInteger # pdata newRoyaltyY
                #$ pdcons @"DAOPolicy" @(PBuiltinList (PAsData PStakingCredential)) # pdata prevDAOPolicy
                #$ pdcons @"treasuryAddress" @PValidatorHash # pdata prevTreasuryAddress
                #$ pdcons @"royaltyPubKey" @PByteString # pdata prevroyaltyPubKey
                #$ pdcons @"nonce" @PInteger # pdata prevnonce
                    # pdnil)

  let validConfig = expectedConfig #== newDatum

  pure $ validConfig #&& validTreasuryAndRoyaltyChange #&& anotherTokenTreasuryAndRoyaltyCorrect

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

validDAOAction :: ClosedTerm (RoyaltyPoolConfig :--> PTxInfo :--> PBool)
validDAOAction = plam $ \cfg txInfo -> unTermCont $ do
  wdrl     <- tletField @"wdrl" txInfo
  policies <- tletField @"DAOPolicy" cfg
  let 
      policySC = pfromData $ phead # policies
      headWithdrawl = plookup # policySC # wdrl
  pure $ Maybe.pisJust # headWithdrawl

validRoyaltyWithdrawAction :: ClosedTerm (PTxInfo :--> PBool)
validRoyaltyWithdrawAction = plam $ \txInfo -> unTermCont $ do
  wdrl     <- tletField @"wdrl" txInfo
  let
      headWithdrawl = plookup # royaltyStakeCred # wdrl
  pure $ Maybe.pisJust # headWithdrawl

parseDatum :: ClosedTerm (PDatum :--> RoyaltyPoolConfig)
parseDatum = plam $ \newDatum -> unTermCont $ do
  PDatum poolDatum <- pmatchC newDatum
  tletUnwrap $ ptryFromData @(RoyaltyPoolConfig) $ poolDatum

poolValidatorT :: ClosedTerm (RoyaltyPoolConfig :--> PoolRedeemer :--> PScriptContext :--> PBool)
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
        selfIdentity #&& (unTermCont $ do
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

            lovelaceToken2Token0 <- tletField @"lovelaceToken2Token" s0
            lovelaceToken2Token1 <- tletField @"lovelaceToken2Token" s1
            let dx  = rx1 - rx0
                dy  = ry1 - ry0
                dlq = lq1 - lq0 -- pool keeps only the negative part of LQ tokens

                correctLovelaceToken2TokenValue = lovelaceToken2Token1 #== lovelaceToken2Token0

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
                        feeNum   <- tletField @"feeNum" conf
                        tFeeNum  <- tletField @"treasuryFee" conf
                        rFeeNum  <- tletField @"royaltyFee"  conf
                        feeDen'  <- tlet feeDen
                        let
                            newConfig     = parseDatum # succD
                            validTreasury = correctSwapConfig # conf # newConfig # dx # dy

                            dxf = dx * (feeNum - tFeeNum - rFeeNum)
                            dyf = dy * (feeNum - tFeeNum - rFeeNum)

                            validSwap =
                                pif
                                    (zero #< dx)
                                    (-dy * (rx0 * feeDen' + dxf) #<= ry0 * dxf)
                                    (-dx * (ry0 * feeDen' + dyf) #<= rx0 * dyf)
                        pure $ noMoreTokens #&& scriptPreserved #&& dlq #== 0 #&& validSwap #&& validTreasury -- liquidity left intact and swap is performed properly
                    DAOAction -> validDAOAction # conf # txinfo'
                    WithdrawRoyalty -> validRoyaltyWithdrawAction # txinfo'
                    _ -> unTermCont $ do
                        POutputDatum selfD' <- pmatchC selfDatum
                        selfD               <- tletField @"outputDatum" selfD'
                        let
                            confPreserved      = selfD #== succD -- whole config preserved
                            validDepositRedeem = dlq * rx0 #<= dx * lq0 #&& dlq * ry0 #<= dy * lq0
                        pure $ noMoreTokens #&& confPreserved #&& scriptPreserved #&& validDepositRedeem -- either deposit or redeem is performed properly                
            pure $ correctLovelaceToken2TokenValue #&& valid
        )
