{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module WhalePoolsDex.Contracts.Proxy.FeeSwitch (
    DAORedeemer (..),
    DAOAction (..),
) where

import qualified PlutusTx
import PlutusTx.Builtins
import PlutusTx.IsData.Class

data DAOAction = WithdrawTreasury | ChangeStakePart | ChangeTreasuryFee | ChangeTreasuryAddress | ChangeAdminAddress | ChangePoolFee
    deriving (Show)

instance FromData DAOAction where
    {-# INLINE fromBuiltinData #-}
    fromBuiltinData d = matchData' d (\_ _ -> Nothing) (const Nothing) (const Nothing) chooseAction (const Nothing)
      where
        chooseAction i
            | i == 0 = Just WithdrawTreasury
            | i == 1 = Just ChangeStakePart
            | i == 2 = Just ChangeTreasuryFee
            | i == 3 = Just ChangeTreasuryAddress
            | i == 4 = Just ChangeAdminAddress
            | i == 5 = Just ChangePoolFee
            | otherwise = Nothing

instance UnsafeFromData DAOAction where
    {-# INLINE unsafeFromBuiltinData #-}
    unsafeFromBuiltinData = maybe (Prelude.error "") id . fromBuiltinData

instance ToData DAOAction where
    {-# INLINE toBuiltinData #-}
    toBuiltinData a = mkI $ case a of
        WithdrawTreasury -> 0
        ChangeStakePart -> 1
        ChangeTreasuryFee -> 2
        ChangeTreasuryAddress -> 3
        ChangeAdminAddress -> 4
        ChangePoolFee -> 5

data DAORedeemer = DAORedeemer
    { action    :: DAOAction
    , poolInIdx :: Integer
    }
    deriving (Show)

PlutusTx.makeIsDataIndexed ''DAORedeemer [('DAORedeemer, 0)]