{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module WhalePoolsDex.Contracts.Proxy.RedeemBalance where

import PlutusLedgerApi.V1.Crypto (PubKeyHash)
import PlutusLedgerApi.V1.Value
import qualified PlutusTx
import PlutusTx.Builtins

data RedeemBalanceConfig = RedeemBalanceConfig
    { poolNft :: AssetClass
    , poolX   :: AssetClass
    , poolY   :: AssetClass
    , poolLp  :: AssetClass
    , exFee   :: Integer
    , rewardPkh :: PubKeyHash
    , stakePkh  :: Maybe PubKeyHash
    }
     deriving stock (Show)

PlutusTx.makeIsDataIndexed ''RedeemBalanceConfig [('RedeemBalanceConfig, 0)]
