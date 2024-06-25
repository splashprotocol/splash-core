{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module WhalePoolsDex.Contracts.StablePool (
    StablePoolConfig (..)
) where

import PlutusTx.Builtins
import qualified PlutusTx
import PlutusLedgerApi.V1.Value
import PlutusLedgerApi.V1.Crypto (PubKeyHash)
import PlutusLedgerApi.V1.Scripts (ValidatorHash)
import PlutusLedgerApi.V1.Credential

data StablePoolConfig = StablePoolConfig
    { poolNft      :: AssetClass
    , an2n         :: Integer
    , assetX       :: AssetClass
    , assetY       :: AssetClass
    , multiplierX  :: Integer
    , multiplierY  :: Integer
    , poolLq       :: AssetClass
    , amplCoeffIsEditable :: Bool
    , lpFeeIsEditable     :: Bool
    , lpFeeNum       :: Integer
    , protocolFeeNum :: Integer
    , daoStabeProxyWitness :: ValidatorHash
    , treasuryAddress      :: ValidatorHash
    , treasuryX   :: Integer
    , treasuryY   :: Integer
    }
    deriving stock (Show)

PlutusTx.makeIsDataIndexed ''StablePoolConfig [('StablePoolConfig, 0)]