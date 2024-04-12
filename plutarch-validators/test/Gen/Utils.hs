{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Gen.Utils where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog as HH

import Hedgehog
import Hedgehog.Gen   as Gen      hiding (map)
import Hedgehog.Range as Range
import Hedgehog.Internal.Property

import Data.Text as T

import PlutusLedgerApi.V2
import qualified PlutusTx.AssocMap as AssocMap
import PlutusLedgerApi.V1.Address
import PlutusLedgerApi.V1.Value
import Plutarch.Api.V2 (scriptHash)
import PlutusTx.Builtins.Internal
import qualified PlutusLedgerApi.V1 as Plutus

import Gen.DepositGen (mkByteString)
import Gen.Models     (mkDatum, genTxOutRef)

import WhalePoolsDex.Contracts.Pool
import WhalePoolsDex.Contracts.BalancePool
import WhalePoolsDex.PConstants hiding (mkByteString)

--todo: change to real
fakePoolHashValue :: String
fakePoolHashValue = "f002facfd69d51b63e7046c6d40349b0b17c8dd775ee415c66af3ccc"

fakePoolValidatorHash :: Plutus.ValidatorHash
fakePoolValidatorHash = Plutus.ValidatorHash $ BuiltinByteString . mkByteString $ T.pack fakePoolHashValue

class ToTxOut a where
  toTxOut :: a -> TxOut

class ToTxInfo a where
  toTxInInfo :: (MonadGen m) => a -> m TxInInfo

data ActionResult = ActionResult
  { newPool :: Pool
  , additionalOutputs :: [TxOut]
  } deriving Show

data TestAction m = TestAction
  { name   :: String
  , action :: (Pool -> m ActionResult)
  }

data Pool = Pool 
  { config       :: PoolConfig
  , stakeAddress :: PubKeyHash
  , value        :: Value
  } deriving Show

instance ToTxOut Pool where
  toTxOut Pool{..} = TxOut
    { txOutAddress  = Address (ScriptCredential poolValidatorHash) (Just $ StakingHash (PubKeyCredential stakeAddress)) -- todo: add staking part
    , txOutValue    = value
    , txOutDatum    = OutputDatum $ mkDatum config
    , txOutReferenceScript = Nothing
    }

data BalancePool = BalancePool 
  { config       :: BalancePoolConfig
  , stakeAddress :: PubKeyHash
  , value        :: Value
  } deriving Show

instance ToTxOut BalancePool where
  toTxOut BalancePool{..} = TxOut
    { txOutAddress  = Address (ScriptCredential fakePoolValidatorHash) (Just $ StakingHash (PubKeyCredential stakeAddress)) -- todo: add staking part
    , txOutValue    = value
    , txOutDatum    = OutputDatum $ mkDatum config
    , txOutReferenceScript = Nothing
    }

instance ToTxInfo TxOut where
  toTxInInfo txOut = do
    ref <- genTxOutRef
    pure $ TxInInfo
      { txInInfoOutRef   = ref
      , txInInfoResolved = txOut
      }

data TestResult = Success | Failed

data TestGroup action = TestGroup 
  { name :: String
  , contractAction  :: action
  , validAction     :: TestAction Gen
  , invalidActions  :: [TestAction Gen]
  }

filterAssetClass :: AssetClass -> Value -> Value
filterAssetClass ac2filter initValue = let
  (cs2filter, tn2filter) = unAssetClass ac2filter
  filterTNList tnList = AssocMap.fromList $ (\(tn, _) -> tn /= tn2filter) `Prelude.filter` (AssocMap.toList tnList)
  in Value {
    getValue = AssocMap.fromList $ (\(cs, tnList) -> if (cs /= cs2filter) then (cs, tnList) else (cs, filterTNList tnList)) `Prelude.map` (AssocMap.toList $ getValue initValue)
  }

constructCase testResult TestAction{..} = 
  let
    testName :: TestName = 
      case testResult of
        Success -> "Correct " ++ name ++ " change"
        Failed ->  "Attempt to " ++ name ++ " failed"
    propertyName = PropertyName name
  in (testName, propertyName, action, testResult)

constructIncorrectSignatureThresholdCase TestAction{..} = 
  ("Attempt to " ++ name ++ " failed, with incorrect signature qty", PropertyName name, action, Failed)

eraseRight :: Either a b -> Either a ()
eraseRight (Right _) = Right ()
eraseRight (Left l)  = Left l

eraseLeft :: Either a b -> Either () b
eraseLeft (Right l) = Right l
eraseLeft (Left _)  = Left ()

eraseBoth :: Either a b -> Either () ()
eraseBoth (Right _) = Right ()
eraseBoth (Left _)  = Left ()