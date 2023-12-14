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
import PlutusLedgerApi.V1.Address
import Plutarch.Api.V2 (scriptHash)
import PlutusTx.Builtins.Internal
import qualified PlutusLedgerApi.V1 as Plutus

import Gen.DepositGen (mkByteString)
import Gen.Models     (mkDatum)

import WhalePoolsDex.Contracts.Pool

--todo: change to real
fakePoolHashValue :: String
fakePoolHashValue = "0237cc313756ebb5bcfc2728f7bdc6a8047b471220a305aa373b278a"

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
    { txOutAddress  = Address (ScriptCredential fakePoolValidatorHash) (Just $ StakingHash (PubKeyCredential stakeAddress)) -- todo: add staking part
    , txOutValue    = value
    , txOutDatum    = OutputDatum $ mkDatum config
    , txOutReferenceScript = Nothing
    }

data TestResult = Success | Failed

data TestGroup action = TestGroup 
  { name :: String
  , contractAction  :: action
  , validAction     :: TestAction Gen
  , invalidActions  :: [TestAction Gen]
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