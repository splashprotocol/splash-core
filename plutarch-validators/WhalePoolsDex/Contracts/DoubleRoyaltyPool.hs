{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module WhalePoolsDex.Contracts.DoubleRoyaltyPool (
    DoubleRoyaltyPoolConfig (..),
    DoubleRoyaltyPoolRedeemer (..),
    DoubleRoyaltyPoolAction (..),
    burnLqInitial,
    maxLqCap
) where

import qualified PlutusTx
import PlutusTx.Builtins
import PlutusLedgerApi.V1.Credential
import PlutusLedgerApi.V1.Scripts (ValidatorHash)
import PlutusLedgerApi.V1.Value

-- | Off-chain representation of the datum used by
-- 'WhalePoolsDex.PContracts.PDoubleRoyaltyPool'.  Field order is part of the
-- Plutus data encoding and must remain aligned with the Plutarch record.
data DoubleRoyaltyPoolConfig = DoubleRoyaltyPoolConfig
    { poolNft             :: AssetClass
    , poolX               :: AssetClass
    , poolY               :: AssetClass
    , poolLq              :: AssetClass
    , poolFeeNum          :: Integer
    , treasuryFee         :: Integer
    , firstRoyaltyFee     :: Integer
    , secondRoyaltyFee    :: Integer
    , treasuryX           :: Integer
    , treasuryY           :: Integer
    , firstRoyaltyX       :: Integer
    , firstRoyaltyY       :: Integer
    , secondRoyaltyX      :: Integer
    , secondRoyaltyY      :: Integer
    , daoPolicy           :: [StakingCredential]
    , treasuryAddress     :: ValidatorHash
    , firstRoyaltyPubKey  :: BuiltinByteString
    , secondRoyaltyPubKey :: BuiltinByteString
    , nonce               :: Integer
    }
    deriving stock (Show)

PlutusTx.makeIsDataIndexed ''DoubleRoyaltyPoolConfig [('DoubleRoyaltyPoolConfig, 0)]

data DoubleRoyaltyPoolAction
    = Deposit
    | Redeem
    | Swap
    | DAOAction
    | WithdrawRoyalty
    deriving (Show)

instance PlutusTx.FromData DoubleRoyaltyPoolAction where
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

instance PlutusTx.UnsafeFromData DoubleRoyaltyPoolAction where
    {-# INLINE unsafeFromBuiltinData #-}
    unsafeFromBuiltinData = maybe (Prelude.error "Couldn't convert DoubleRoyaltyPoolAction from builtin data") id . PlutusTx.fromBuiltinData

instance PlutusTx.ToData DoubleRoyaltyPoolAction where
    {-# INLINE toBuiltinData #-}
    toBuiltinData action = mkI $ case action of
        Deposit -> 0
        Redeem -> 1
        Swap -> 2
        DAOAction -> 3
        WithdrawRoyalty -> 4

data DoubleRoyaltyPoolRedeemer = DoubleRoyaltyPoolRedeemer
    { action :: DoubleRoyaltyPoolAction
    , selfIx :: Integer
    }
    deriving (Show)

PlutusTx.makeIsDataIndexed ''DoubleRoyaltyPoolRedeemer [('DoubleRoyaltyPoolRedeemer, 0)]

{-# INLINEABLE maxLqCap #-}
maxLqCap :: Integer
maxLqCap = 0x7fffffffffffffff

{-# INLINEABLE burnLqInitial #-}
burnLqInitial :: Integer
burnLqInitial = 1000
