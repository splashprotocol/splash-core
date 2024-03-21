{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module WhalePoolsDex.Contracts.Proxy.DepositBalance where

import PlutusLedgerApi.V1.Crypto (PubKeyHash)
import PlutusLedgerApi.V1.Value
import qualified PlutusTx
import PlutusTx.Builtins

data DepositBalanceConfig = DepositBalanceConfig
    { poolNft :: AssetClass
    , tokenA  :: AssetClass
    , tokenB  :: AssetClass
    , tokenLp :: AssetClass
    , exFee   :: Integer
    , rewardPkh :: PubKeyHash
    , stakePkh  :: Maybe PubKeyHash
    , collateralAda :: Integer
    }
    deriving stock (Show)

PlutusTx.makeIsDataIndexed ''DepositBalanceConfig [('DepositBalanceConfig, 0)]