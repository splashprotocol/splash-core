{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module WhalePoolsDex.Contracts.BalancePool (
    BalancePoolConfig (..)
) where

import PlutusTx.Builtins
import qualified PlutusTx
import PlutusLedgerApi.V1.Value
import PlutusLedgerApi.V1.Crypto (PubKeyHash)
import PlutusLedgerApi.V1.Scripts (ValidatorHash)

data BalancePoolConfig = BalancePoolConfig
    { poolNft     :: AssetClass
    , poolX       :: AssetClass
    , poolY       :: AssetClass
    , poolLq      :: AssetClass
    , poolFeeNum  :: Integer
    , treasuryFee :: Integer
    , treasuryX   :: Integer
    , treasuryY   :: Integer
    , daoPolicy   :: [CurrencySymbol]
    , treasuryAddress :: ValidatorHash
    , weightX     :: Integer
    , weightY     :: Integer
    }
    deriving stock (Show)

PlutusTx.makeIsDataIndexed ''BalancePoolConfig [('BalancePoolConfig, 0)]