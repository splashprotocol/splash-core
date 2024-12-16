{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module WhalePoolsDex.Contracts.RoyaltyPool (
    RoyaltyPoolConfig (..),
    RoyaltyPoolRedeemer (..),
    RoyaltyPoolAction (..),
    burnLqInitial,
    maxLqCap
) where

import PlutusTx.Builtins
import qualified PlutusTx
import PlutusLedgerApi.V1.Value
import PlutusLedgerApi.V1.Crypto (PubKeyHash)
import PlutusLedgerApi.V1.Scripts (ValidatorHash)
import PlutusLedgerApi.V1.Credential
import qualified Data.ByteString as BS

data RoyaltyPoolConfig = RoyaltyPoolConfig
    { poolNft     :: AssetClass
    , poolX       :: AssetClass
    , poolY       :: AssetClass
    , poolLq      :: AssetClass
    , poolFeeNum  :: Integer
    , treasuryFee :: Integer
    , royaltyFee  :: Integer
    , treasuryX   :: Integer
    , treasuryY   :: Integer
    , royaltyX    :: Integer
    , royaltyY    :: Integer
    , daoPolicy   :: [StakingCredential]
    , treasuryAddress :: ValidatorHash
    , royaltyPubKey :: BuiltinByteString
    , nonce :: Integer
    }
    deriving stock (Show)

PlutusTx.makeIsDataIndexed ''RoyaltyPoolConfig [('RoyaltyPoolConfig, 0)]

data RoyaltyPoolAction = Deposit | Redeem | Swap | DAOAction | WithdrawRoyalty
    deriving (Show)

instance PlutusTx.FromData RoyaltyPoolAction where
    {-# INLINE fromBuiltinData #-}
    fromBuiltinData d = matchData' d (\_ _ -> Nothing) (const Nothing) (const Nothing) chooseAction (const Nothing)
      where
        chooseAction i
            | i == 0 = Just Deposit
            | i == 1 = Just Redeem
            | i == 2 = Just Swap
            | i == 3 = Just DAOAction
            | i == 4 = Just WithdrawRoyalty
            | otherwise = Nothing

instance PlutusTx.UnsafeFromData RoyaltyPoolAction where
    {-# INLINE unsafeFromBuiltinData #-}
    unsafeFromBuiltinData = maybe (Prelude.error "Couln't convert PoolAction from builtin data") id . PlutusTx.fromBuiltinData

instance PlutusTx.ToData RoyaltyPoolAction where
    {-# INLINE toBuiltinData #-}
    toBuiltinData a = mkI $ case a of
        Deposit -> 0
        Redeem -> 1
        Swap -> 2
        DAOAction -> 3
        WithdrawRoyalty -> 4

data RoyaltyPoolRedeemer = RoyaltyPoolRedeemer
    { action :: RoyaltyPoolAction
    , selfIx :: Integer
    }
    deriving (Show)

PlutusTx.makeIsDataIndexed ''RoyaltyPoolRedeemer [('RoyaltyPoolRedeemer, 0)]

{-# INLINEABLE maxLqCap #-}
maxLqCap :: Integer
maxLqCap = 0x7fffffffffffffff

{-# INLINEABLE burnLqInitial #-}
burnLqInitial :: Integer
burnLqInitial = 1000