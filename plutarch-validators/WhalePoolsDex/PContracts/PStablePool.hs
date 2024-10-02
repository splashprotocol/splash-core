{-# LANGUAGE UndecidableInstances #-}

module WhalePoolsDex.PContracts.PStablePool (
    StablePoolConfig (..),
    extractStablePoolConfig
) where

import qualified GHC.Generics as GHC
import           Generics.SOP (Generic, I (I))

import Plutarch
import Plutarch.Api.V2              (PScriptHash(..), PMaybeData (..), PTxOut, POutputDatum(..), PAddress(..), PPubKeyHash(..), PDatum(..), PValue(..), KeyGuarantees(..), AmountGuarantees(..), PCurrencySymbol(..), PStakingCredential(..))
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
import Plutarch.Api.V1.AssocMap     as Map
import Plutarch.Extra.Maybe         as Maybe

import PExtra.API                   (PAssetClass, assetClassValueOf, ptryFromData, assetClass, pValueLength)
import PExtra.List                  (pelemAt)
import PExtra.Monadic               (tcon, tlet, tletField, tmatch)

import qualified WhalePoolsDex.Contracts.StablePool as P
import           WhalePoolsDex.PContracts.PApi      (burnLqInitial, feeDen, maxLqCap, tletUnwrap, zero, containsSignature)
import           WhalePoolsDex.PConstants

import Plutarch.Trace

data AikenBool (s :: S)
  =  False ( Term
            s
            ( PDataRecord '[ ]
            )
        ) 
    | True ( Term
            s
            ( PDataRecord '[ ]
            )
        )
    deriving stock (GHC.Generic)
    deriving
        (PIsData, PlutusType, PEq)

instance DerivePlutusType AikenBool where type DPTStrat _ = PlutusTypeData
instance PTryFrom PData AikenBool

newtype StablePoolConfig (s :: S)
    = StablePoolConfig
        ( Term
            s
            ( PDataRecord
                '[ "poolNft"              ':= PAssetClass
                 , "an2n"                 ':= PInteger
                 , "assetX"               ':= PAssetClass
                 , "assetY"               ':= PAssetClass
                 , "multiplierX"          ':= PInteger
                 , "multiplierY"          ':= PInteger
                 , "lpToken"              ':= PAssetClass
                 , "amplCoeffIsEditable"  ':= AikenBool
                 , "lpFeeIsEditable"      ':= AikenBool
                 , "lpFeeNum"             ':= PInteger
                 , "protocolFeeNum"       ':= PInteger
                 , "daoStabeProxyWitness" ':= PByteString
                 , "treasuryAddress"      ':= PByteString
                 , "treasuryX"            ':= PInteger
                 , "treasuryY"            ':= PInteger
                 ]
            )
        )
    deriving stock (GHC.Generic)
    deriving
        (PIsData, PDataFields, PlutusType, PEq)

instance DerivePlutusType StablePoolConfig where type DPTStrat _ = PlutusTypeData

instance PUnsafeLiftDecl StablePoolConfig where type PLifted StablePoolConfig = P.StablePoolConfig
deriving via (DerivePConstantViaData P.StablePoolConfig StablePoolConfig) instance (PConstantDecl P.StablePoolConfig)

instance PTryFrom PData (PAsData StablePoolConfig)

extractStablePoolConfig :: Term s (PTxOut :--> StablePoolConfig)
extractStablePoolConfig = plam $ \txOut -> unTermCont $ do
  txOutDatum <- tletField @"datum" txOut

  POutputDatum txOutOutputDatum <- pmatchC txOutDatum

  rawDatum <- tletField @"outputDatum" txOutOutputDatum

  PDatum poolDatum <- pmatchC rawDatum

  tletUnwrap $ ptryFromData @(StablePoolConfig) $ poolDatum