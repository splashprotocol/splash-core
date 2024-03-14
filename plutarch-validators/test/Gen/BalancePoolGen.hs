{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Gen.BalancePoolGen where

import Test.Tasty

import Plutarch.Prelude
import Plutarch
import Plutarch.Api.V2.Contexts (PScriptContext)
import Plutarch.Prelude
import Numeric

import RIO hiding (Data(..))
import Hedgehog
import Hedgehog.Internal.Property
import Hedgehog.Gen   as Gen
import Hedgehog.Range as Range
import HaskellWorks.Hedgehog.Gen hiding (MonadGen)

import Data.Text as T

import WhalePoolsDex.PContracts.PFeeSwitch

import qualified PlutusLedgerApi.V1.Interval as Interval
import PlutusLedgerApi.V1.Value
import qualified PlutusLedgerApi.V1.Value as Value
import qualified Data.ByteString.Base16  as Hex
import qualified Data.Text.Encoding      as E
import qualified Data.List      as List
import qualified Data.ByteString as BS
import PlutusLedgerApi.V2
import PlutusLedgerApi.V1.Address
import Plutarch.Api.V2 (scriptHash)
import PlutusLedgerApi.V1.Credential
import PlutusTx.Builtins.Internal
import WhalePoolsDex.PMintingValidators
import qualified PlutusLedgerApi.V1 as Plutus
import Gen.Models (mkAdaValue, mkValues, mkValue, genAssetClass, genPkh, genCSRandom, genSCRandom, genTxId, genTxOutRef, genValidatorHash, mkContext)
import Gen.DepositGen (unsafeFromEither, mkByteString)
import Gen.Utils hiding (Pool(..), TestAction(..), TestGroup(..))

import WhalePoolsDex.Contracts.BalancePool

data BalancePoolActionResult = BalancePoolActionResult
  { newPool :: BalancePool
  , additionalOutputs :: [TxOut]
  , g :: [Integer]
  , t :: [Integer]
  , maxDen :: Integer
  } deriving Show

data BalancePoolTestAction m = BalancePoolTestAction
  { name   :: String
  , action :: (BalancePool -> m BalancePoolActionResult)
  }

data BalancePoolTestGroup action = BalancePoolTestGroup 
  { name :: String
  , contractAction  :: action
  , validAction     :: BalancePoolTestAction Gen
  , invalidActions  :: [BalancePoolTestAction Gen]
  }

constructCase testResult BalancePoolTestAction{..} = 
  let
    testName :: TestName = 
      case testResult of
        Success -> "Correct " ++ name ++ " change"
        Failed ->  "Attempt to " ++ name ++ " failed"
    propertyName = PropertyName name
  in (testName, propertyName, action, testResult)

instance ToTxInfo BalancePool where
  toTxInInfo pool = do
    ref <- genTxOutRef
    let txOut = toTxOut pool
    pure $ TxInInfo
      { txInInfoOutRef   = ref
      , txInInfoResolved = txOut
      }

feeDen = 100000

daoMintingPurpose :: BalancePool -> ScriptPurpose
daoMintingPurpose BalancePool{..} = Rewarding $ List.head (daoPolicy config)

daoValidator :: BalancePool -> [PubKeyHash] -> Integer -> Bool -> ClosedTerm (PData :--> PScriptContext :--> POpaque)
daoValidator BalancePool{..} admins threshold lpFeeIsEditable = 
  wrapMintingValidator (daoMultisigPolicyValidatorT (pconstant (poolNft config)) (pconstant admins) (pconstant threshold) (pconstant lpFeeIsEditable))

createTxInfo :: MonadGen m => BalancePool -> BalancePoolActionResult -> [PubKeyHash] -> m TxInfo
createTxInfo prevPool@BalancePool{..} BalancePoolActionResult{..} adminPkhs = do
  poolTxIn <- toTxInInfo prevPool
  let
    daoCS = List.head (daoPolicy config)

  pure $ TxInfo
    { txInfoInputs = [poolTxIn]
    , txInfoReferenceInputs = []
    , txInfoOutputs = [toTxOut newPool] ++ additionalOutputs
    , txInfoFee = mempty
    , txInfoMint = mempty
    , txInfoDCert = mempty
    , txInfoWdrl = fromList [(daoCS, 0)]
    , txInfoValidRange = Interval.always
    , txInfoSignatories = adminPkhs
    , txInfoRedeemers = fromList []
    , txInfoData = fromList []
    , txInfoId = "b0"
    }

genBalancePool :: MonadGen f => [PubKeyHash] -> Integer -> Bool -> f BalancePool
genBalancePool adminsPkhs threshold lpFeeIsEditable = do
  (x, y, lq, nft) <- tuple4 genAssetClass

  stakeHash <- genPkh
  
  (xQty :: Int) <- integral (Range.constant 10000000000 10000000000000)
  (yQty :: Int) <- integral (Range.constant 10000000000 10000000000000)

 -- xWeight <- integral (Range.constant 1 9)

  poolFee <- integral (Range.constant 0 1000)

  treasuryAddress <- genValidatorHash
  let
    -- xQty = 4000356
    -- yQty = 1000000
    xWeight = 2
    yWeight = 10 - xWeight
    nftQty = 1
    xQtyFloat = (fromIntegral xQty) :: Double
    xWeightFloat = (fromIntegral xWeight) :: Double
    yQtyFloat = (fromIntegral yQty) :: Double
    yWeightFloat = (fromIntegral yWeight) :: Double
    invariant = (xQtyFloat**(xWeightFloat / 10)) * (yQtyFloat**(yWeightFloat / 10))
    lqQty  = 0x7fffffffffffffff - (round invariant)

    daoContract =
        StakingHash . ScriptCredential . ValidatorHash . getScriptHash . scriptHash $ (unMintingPolicyScript (daoMintPolicyValidator nft adminsPkhs threshold lpFeeIsEditable))

    poolConfig = BalancePoolConfig
      { poolNft = nft
      , poolX   = x
      , weightX = xWeight
      , poolY   = y
      , weightY = yWeight
      , poolLq  = lq
      , poolFeeNum  = 99997
      , treasuryFee = 0
      , treasuryX  = 0
      , treasuryY  = 0
      , daoPolicy  = [daoContract]
      , treasuryAddress = treasuryAddress
      , invariant  = round invariant
      }

    poolValue = mkValues ((\(ac, qty) -> mkValue ac (fromIntegral qty)) `RIO.map` [(x, xQty), (y, yQty), (nft, nftQty), (lq, lqQty)]) mempty

  pure $ BalancePool poolConfig stakeHash poolValue

--- Test utils ---

--- Test cases ---

cutFloat :: Double -> Int -> Integer
cutFloat toCut maxInt = let
    strValue = T.pack $ showFFloat (Just maxInt) toCut ""
    splitted = splitOn "." strValue
  in read $ T.unpack . Prelude.head $ splitted

correctSwap :: MonadGen m => BalancePoolTestAction m
correctSwap = 
  let
    testAction prevPool@BalancePool{..} = do

      let
        (xCS, xTN) = unAssetClass (poolX config)
        (yCS, yTN) = unAssetClass (poolY config)
        xValue = valueOf value xCS xTN
        xValueFloat = (fromIntegral xValue) :: Double
        invariantFloat = fromIntegral (invariant config) :: Double
        xWeightFloat = (fromIntegral (weightX config)) :: Double
        yValue = valueOf value yCS yTN
        yValueFloat = (fromIntegral yValue) :: Double
        yWeightFloat = (fromIntegral (weightY config)) :: Double
        treasuryFeeNum = (fromIntegral $ treasuryFee config) :: Double
        lpFeeNum = (fromIntegral $ poolFeeNum config) :: Double

        xLength = RIO.length (show xValue)
        yLength = RIO.length (show yValue)
        -- maxDen = if (xLength > yLength) then xLength else yLength
     -- xToSwap <- integral (Range.constant 1 ((xValue `div` 2) - 1))


      --

      -- fullLpFeeInContract = reverseFullFee / feeDen

      --


      let
        xToSwap = 1557301
        -- 32562 * (99997 / 100000)
        xInInvariant = fromIntegral $ xValue + round ((fromIntegral xToSwap) * ((lpFeeNum - treasuryFeeNum) / feeDen))
        xInInvariantWithDegree = (xInInvariant :: Double) ** (xWeightFloat / (10 :: Double)) -- g
        xInInvariantWith1Degree = (xInInvariant :: Double) ** (1 / (10 :: Double)) -- t
        xInInvariantWith1WeightDegree = (xInInvariantWith1Degree) ** xWeightFloat
        maxDen = 15

        gX = ((read $ List.delete '.' $ showFFloat (Just maxDen) xInInvariantWithDegree "") :: Integer)
        tX = ((read $ List.delete '.' $ showFFloat (Just maxDen) xInInvariantWith1Degree "") :: Integer)

        xInInvariaintDownDegree = xInInvariantWithDegree ** (10 / xWeightFloat)
        invDivision = invariantFloat / xInInvariantWithDegree
        invDivisionInReverseDegree = invDivision ** ((fromIntegral 10 :: Double) / yWeightFloat)
        yToSwap = yValue - (ceiling invDivisionInReverseDegree)

        gYDouble = (fromIntegral $ yValue - yToSwap) ** (yWeightFloat / (10 :: Double)) -- g
        tGDouble = (fromIntegral $ yValue - yToSwap) ** (1 / (10 :: Double)) -- g

        gY = ((read $ List.delete '.' $ showFFloat (Just maxDen) gYDouble "") :: Integer)
        tY = ((read $ List.delete '.' $ showFFloat (Just maxDen) tGDouble "") :: Integer)

        -- gY = (yValue - yToSwap) ** (yWeightFloat / (10 :: Double))
        -- tY = (yValue - yToSwap) ** (yWeightFloat / (10 :: Double))
    
      -- traceM $ ("--------------")


      -- traceM $ T.pack . show $ ((2000 ** 0.2) * (2000 ** 0.8))
      -- traceM $ T.pack . show $ xToSwap

      -- traceM $ "xTOSwap"
      -- traceM $ T.pack . show $ xToSwap

      -- traceM $ "maxDen"
      -- traceM $ T.pack . show $ maxDen

      -- traceM $ "(fromIntegral xInInvariant)"
      -- traceM $ T.pack . show $ xInInvariant
      -- traceM $ "xInInvariantWithDegree (g)" -- знаменатель дроби должен быть 10^18
      -- traceM $ T.pack . show $ xInInvariantWithDegree
      -- traceM $ "xInInvariantWithDegree (g) decode float" -- знаменатель дроби должен быть 10^18
      -- traceM $ T.pack . show $ ((read $ List.delete '.' $ showFFloat (Just maxDen) xInInvariantWithDegree "") :: Integer)
      -- traceM $ "xInInvariantWith1Degree (t)" -- знаменатель дроби должен быть 10^18
      -- traceM $ T.pack . show $ xInInvariantWith1Degree
      -- traceM $ "xInInvariantWith1Degree (t) decode float" -- знаменатель дроби должен быть 10^18
      -- traceM $ T.pack . show $ ((read $ List.delete '.' $ showFFloat (Just maxDen) xInInvariantWith1Degree "") :: Integer)
      -- traceM $ "xInInvariantWith1WeightDegree (t ^ w)"
      -- traceM $ T.pack . show $ xInInvariantWith1WeightDegree
      -- traceM $ "xInInvariantWith1WeightDegree (t ^ w) decode float"
      -- traceM $ T.pack . show $ ((read $ List.delete '.' $ showFFloat (Just maxDen) xInInvariantWith1WeightDegree "") :: Integer)
      -- traceM $ "xInInvariaintDownDegree"
      -- traceM $ T.pack . show $ xInInvariaintDownDegree


      -- traceM $ "(fromIntegral xToSwap)"
      -- traceM $ T.pack . show $ (fromIntegral xToSwap)
      -- traceM $ "(lpFeeNum - treasuryFeeNum) / feeDen)"
      -- traceM $ T.pack . show $ ((lpFeeNum - treasuryFeeNum) / feeDen)
      -- traceM $ "((fromIntegral xToSwap) * ((lpFeeNum - treasuryFeeNum) / feeDen))"
      -- traceM $ T.pack . show $ (((fromIntegral xToSwap) * ((lpFeeNum - treasuryFeeNum) / feeDen)))

      -- traceM $ "Prev invariant: "
      -- traceM $ T.pack . show $ invariantFloat
      -- traceM $ "balanceX"
      -- traceM $ T.pack . show $ xValue
      -- traceM $ "balanceX + with fee"
      -- traceM $ T.pack . show $ xValueFloat*((lpFeeNum - treasuryFeeNum) / feeDen)
      -- traceM $ "balanceX^weight"
      -- traceM $ T.pack . show $ (((xValueFloat*((lpFeeNum - treasuryFeeNum) / feeDen))**(xWeightFloat / 10)))
      -- traceM $ "xweight"
      -- traceM $ T.pack . show $ (xWeightFloat / 10)

      traceM $ ("--------------")

      -- traceM $ ("y weight: ")
      -- traceM $ T.pack . show $ (yWeightFloat)
      -- traceM $ ("right side: ")
      -- traceM $ T.pack . show $ ((round $ ((invariantFloat / (((xValueFloat*((lpFeeNum - treasuryFeeNum) / feeDen))**(xWeightFloat / 10)))) ** ((fromIntegral 10 :: Double) / yWeightFloat))))
      -- traceM $ ("((fromIntegral 10 :: Double) / yWeightFloat): ")
      -- traceM $ T.pack . show $ (((fromIntegral 10 :: Double) / yWeightFloat))
      -- traceM $ ("right side in root: ")
      -- traceM $ T.pack . show $ ((invariantFloat / (xValueFloat**(xWeightFloat / 10))) ** ((fromIntegral 10 :: Double) / yWeightFloat))
      -- traceM $ ("prev y value: ")
      -- traceM $ T.pack . show $ (yValue)

      let
        newGX = round $ xInInvariantWithDegree
        newGY = round $ ((yValueFloat - (fromIntegral yToSwap)) ** (yWeightFloat / (fromIntegral 10 :: Double)))

        newTX = round $ (xInInvariant) ** ((fromIntegral 1 :: Double) / (fromIntegral 10 :: Double))
        newTY = round $ (yValueFloat - (fromIntegral yToSwap)) ** ((fromIntegral 1 :: Double) / (fromIntegral 10 :: Double))

        newTXWeight = (fromIntegral newTX :: Double) ** xWeightFloat
        newTYWeight = (fromIntegral newTY :: Double) ** yWeightFloat

        newInvariant = newGX * newGY

      -- traceM $ ("yValueFloat: ")
      -- traceM $ T.pack . show $ (yValueFloat)
      -- traceM $ ("(fromIntegral yToSwap): ")
      -- traceM $ T.pack . show $ (yToSwap)
      -- traceM $ ("yValueFloat - (fromIntegral yToSwap): ")
      -- traceM $ T.pack . show $ (yValue - (yToSwap))
      -- traceM $ ("newGX: ")
      -- traceM $ T.pack . show $ (newGX)
      -- traceM $ ("newTX: ")
      -- traceM $ T.pack . show $ (newTX)
      -- traceM $ ("newTXWeight: ")
      -- traceM $ T.pack . show $ (newTXWeight)
      -- traceM $ ("newGY: ")
      -- traceM $ T.pack . show $ (newGY)
      -- traceM $ ("newTY: ")
      -- traceM $ T.pack . show $ (newTY)
      -- traceM $ ("newTYWeight: ")
      -- traceM $ T.pack . show $ (newTYWeight)

      -- traceM $ ("prevInvariant: ")
      -- traceM $ T.pack . show $ ((invariant config))

      -- traceM $ ("newInvariant: ")
      -- traceM $ T.pack . show $ (newInvariant)

      let
        -- going to withdraw all pool x and y value
        newPoolConfig = config 
          { treasuryX = 0
          , treasuryY = 0
          }

        newPool = prevPool 
          { config = newPoolConfig
          , value  = value <> (assetClassValue (poolX config) (xToSwap)) <> (assetClassValue (poolY config) (negate yToSwap))
          }

      pure $ BalancePoolActionResult newPool [] [gX, gY] [tX, tY] (toInteger 15)
  in BalancePoolTestAction "Correct swap" testAction