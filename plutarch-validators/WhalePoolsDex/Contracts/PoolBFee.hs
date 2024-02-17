{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module WhalePoolsDex.Contracts.PoolBFee (
    PoolConfig (..)
) where

import PlutusTx.Builtins
import qualified PlutusTx
import PlutusLedgerApi.V1.Value
import PlutusLedgerApi.V1.Crypto (PubKeyHash)
import PlutusLedgerApi.V1.Scripts (ValidatorHash)
import PlutusLedgerApi.V1.Credential

data PoolConfig = PoolConfig
    { poolNft      :: AssetClass
    , poolX        :: AssetClass
    , poolY        :: AssetClass
    , poolLq       :: AssetClass
    , poolFeeNumX  :: Integer
    , poolFeeNumY  :: Integer
    , treasuryFee  :: Integer
    , treasuryX    :: Integer
    , treasuryY    :: Integer
    , daoPolicy    :: [StakingCredential]
    , lqBound      :: Integer
    , treasuryAddress :: ValidatorHash
    }
    deriving stock (Show)

PlutusTx.makeIsDataIndexed ''PoolConfig [('PoolConfig, 0)]
