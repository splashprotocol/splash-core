{-# LANGUAGE UndecidableInstances #-}

module WhalePoolsDex.PContracts.PStablePoolV1 (
    StablePoolConfigV1 (..),
    extractStablePoolConfigV1
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

newtype StablePoolConfigV1 (s :: S)
    = StablePoolConfigV1
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
                 , "lpFeeNum"             ':= PInteger
                 , "protocolFeeNum"       ':= PInteger
                 , "daoStabeProxyWitness" ':= PBuiltinList (PByteString)
                 , "treasuryAddress"      ':= PByteString
                 , "treasuryX"            ':= PInteger
                 , "treasuryY"            ':= PInteger
                 ]
            )
        )
    deriving stock (GHC.Generic)
    deriving
        (PIsData, PDataFields, PlutusType, PEq)

instance DerivePlutusType StablePoolConfigV1 where type DPTStrat _ = PlutusTypeData

instance PUnsafeLiftDecl StablePoolConfigV1 where type PLifted StablePoolConfigV1 = P.StablePoolConfigV1
deriving via (DerivePConstantViaData P.StablePoolConfigV1 StablePoolConfigV1) instance (PConstantDecl P.StablePoolConfigV1)

instance PTryFrom PData (PAsData StablePoolConfigV1)

extractStablePoolConfigV1 :: Term s (PTxOut :--> StablePoolConfigV1)
extractStablePoolConfigV1 = plam $ \txOut -> unTermCont $ do
  txOutDatum <- tletField @"datum" txOut

  POutputDatum txOutOutputDatum <- pmatchC txOutDatum

  rawDatum <- tletField @"outputDatum" txOutOutputDatum

  PDatum poolDatum <- pmatchC rawDatum

  tletUnwrap $ ptryFromData @(StablePoolConfigV1) $ poolDatum