{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module WhalePoolsDex.Contracts.Proxy.RoyaltyWithdraw (
    RoyaltyWithdrawConfig (..),
    RoyaltyData(..),
    RoyaltyPoolWithdrawRedeemer(..)
) where

import PlutusLedgerApi.V1.Crypto (PubKeyHash)
import PlutusLedgerApi.V1.Value
import qualified PlutusTx
import PlutusTx.Builtins

data RoyaltyData = RoyaltyData
    { poolNft    :: AssetClass
    , withdrawRoyaltyX :: Integer
    , withdrawRoyaltyY :: Integer
    , royaltyAddress   :: PubKeyHash
    , royaltyPubKey :: BuiltinByteString
    , exFee         :: Integer
    }
    deriving stock (Show)

PlutusTx.makeIsDataIndexed ''RoyaltyData [('RoyaltyData, 0)]

data RoyaltyWithdrawConfig = RoyaltyWithdrawConfig
    { royaltyData :: RoyaltyData
    , signature   :: BuiltinByteString
    , additionalBytes   :: BuiltinByteString
    }
    deriving stock (Show)

PlutusTx.makeIsDataIndexed ''RoyaltyWithdrawConfig [('RoyaltyWithdrawConfig, 0)]

data RoyaltyPoolWithdrawRedeemer = RoyaltyPoolWithdrawRedeemer
    { poolInIdx  :: Integer
    , orderInIdx :: Integer
    }
    deriving stock (Show)

PlutusTx.makeIsDataIndexed ''RoyaltyPoolWithdrawRedeemer [('RoyaltyPoolWithdrawRedeemer, 0)]