{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module WhalePoolsDex.Contracts.BalancePool (
    BalancePoolConfig (..),
    BalancePoolAction (..),
    BalancePoolRedeemer (..)
) where

import PlutusTx.Builtins
import qualified PlutusTx
import PlutusLedgerApi.V1.Value
import PlutusLedgerApi.V1.Crypto  (PubKeyHash)
import PlutusLedgerApi.V1.Scripts (ValidatorHash)
import PlutusLedgerApi.V1.Credential

data BalancePoolConfig = BalancePoolConfig
    { poolNft     :: AssetClass
    , poolX       :: AssetClass
    , weightX     :: Integer
    , poolY       :: AssetClass
    , weightY     :: Integer
    , poolLq      :: AssetClass
    , poolFeeNum  :: Integer
    , treasuryFee :: Integer
    , treasuryX   :: Integer
    , treasuryY   :: Integer
    , daoPolicy   :: [StakingCredential]
    , treasuryAddress :: ValidatorHash
    , invariant   :: Integer
    }
    deriving stock (Show)

PlutusTx.makeIsDataIndexed ''BalancePoolConfig [('BalancePoolConfig, 0)]

data BalancePoolAction = Deposit | DepositSingle | Redeem | RedeemSingle | Swap | DAOAction
    deriving (Show)

instance PlutusTx.FromData BalancePoolAction where
    {-# INLINE fromBuiltinData #-}
    fromBuiltinData d = matchData' d (\_ _ -> Nothing) (const Nothing) (const Nothing) chooseAction (const Nothing)
      where
        chooseAction i
            | i == 0 = Just Deposit
            | i == 1 = Just DepositSingle
            | i == 2 = Just Redeem
            | i == 3 = Just RedeemSingle
            | i == 4 = Just Swap
            | i == 5 = Just DAOAction
            | otherwise = Nothing

instance PlutusTx.UnsafeFromData BalancePoolAction where
    {-# INLINE unsafeFromBuiltinData #-}
    unsafeFromBuiltinData = maybe (Prelude.error "Couln't convert BalancePoolAction from builtin data") id . PlutusTx.fromBuiltinData

instance PlutusTx.ToData BalancePoolAction where
    {-# INLINE toBuiltinData #-}
    toBuiltinData a = mkI $ case a of
        Deposit -> 0
        DepositSingle -> 1
        Redeem -> 2
        RedeemSingle -> 3
        Swap -> 4
        DAOAction -> 5

data BalancePoolRedeemer = BalancePoolRedeemer
    { action :: BalancePoolAction
    , selfIx :: Integer
    , g :: [Integer]
    , t :: [Integer]
    , maxDen :: Integer
    }
    deriving (Show)

PlutusTx.makeIsDataIndexed ''BalancePoolRedeemer [('BalancePoolRedeemer, 0)]