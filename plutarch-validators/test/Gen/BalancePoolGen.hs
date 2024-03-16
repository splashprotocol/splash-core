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
import Data.BigDecimal
import Data.BigFloating
import Data.Ratio

import RIO hiding (Data(..))
import Hedgehog
import Hedgehog.Internal.Property
import Hedgehog.Gen   as Gen
import Hedgehog.Range as Range
import HaskellWorks.Hedgehog.Gen hiding (MonadGen)

import Data.Text as T

import WhalePoolsDex.PContracts.PFeeSwitch

import qualified PlutusLedgerApi.V1.Interval as Interval
import PlutusLedgerApi.V1.Value hiding (getValue)
import qualified PlutusLedgerApi.V1.Value as Value hiding (getValue)
import qualified Data.ByteString.Base16  as Hex
import qualified Data.Text.Encoding      as E
import qualified Data.List      as List
import qualified Data.ByteString as BS
import PlutusLedgerApi.V2 hiding (getValue)
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


takeNBigDecimal :: BigDecimal -> Integer -> Integer
takeNBigDecimal toCut n =
  let
    inString = T.unpack (T.take (fromInteger n) (T.pack . show $ (Data.BigDecimal.getValue toCut)))
  in (read inString) :: Integer

genBalancePool :: MonadGen f => [PubKeyHash] -> Integer -> Bool -> f BalancePool
genBalancePool adminsPkhs threshold lpFeeIsEditable = do
  (x, y, lq, nft) <- tuple4 genAssetClass

  stakeHash <- genPkh
  
  -- todo: error on big values such as 10000000000000000
  (xQty :: Integer) <- integral (Range.constant 10000000000 10000000000000000)
  -- (yQty :: Int) <- integral (Range.constant 10000000000 10000000000000000)

  -- todo: doesn't work for non 2/8 pools
  -- (xWeight :: Integer) <- integral (Range.constant 1 9)

  poolFee <- integral (Range.constant 0 1000)

  treasuryAddress <- genValidatorHash
  let
    -- xQty = 56000010004005123213
    xWeight = 3
    yWeight = 10 - xWeight
    yQty = xQty * yWeight
    nftQty = 1
    xQtyFloat = (fromIntegral xQty) :: Double
    -- xWeightFloat = (fromIntegral xWeight) :: Double
    yQtyFloat = (fromIntegral yQty) :: Double
    -- yWeightFloat = (fromIntegral yWeight) :: Double

    yPartLength  = toInteger $ RIO.length . show $ xQty
    xValueLength = toInteger $ RIO.length . show $ yQty

    maxPrecision = (if (yPartLength > xValueLength) then yPartLength else xValueLength)

    -- invariant = (xQtyFloat**(xWeightFloat / 10)) * (yQtyFloat**(yWeightFloat / 10))
    invariantT = ((BigDecimal xQty 0) ** (fromRational $ (fromIntegral xWeight) / 10)) * ( (BigDecimal yQty 0) ** (fromRational $ (fromIntegral yWeight) / 10))
    invariant = takeNBigDecimal invariantT maxPrecision
    lqQty  = 0x7fffffffffffffff - invariant

    daoContract =
        StakingHash . ScriptCredential . ValidatorHash . getScriptHash . scriptHash $ (unMintingPolicyScript (daoMintPolicyValidator nft adminsPkhs threshold lpFeeIsEditable))

  traceM $ T.pack $ "invariantT:" ++ show invariantT
  traceM $ T.pack $ "invariant:" ++ show invariant

  let
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
      , invariant  = invariant
      }

    poolValue = mkValues ((\(ac, qty) -> mkValue ac (fromIntegral qty)) `RIO.map` [(x, xQty), (y, yQty), (nft, nftQty), (lq, lqQty)]) mempty

  pure $ BalancePool poolConfig stakeHash poolValue

--- Test utils ---

-- BaseAssetBalance -> BaseAssetWeight -> QuoteAssetBalance -> QuoteAssetWeghit -> BaseIn -> lpFee -> treasuryFee -> (gBase, tBase, gQuote, tQuote, quoteOut)
calculateGandTSwap :: MonadGen m => Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> m (Integer, Integer, Integer, Integer, Integer)
calculateGandTSwap baseAssetBalance baseAssetWeight quoteAssetBalance quoteAssetWeghit baseIn lpFee treasuryFee prevInvariant = do
  let
      yPartLength = toInteger $ RIO.length . show $ quoteAssetBalance
      xValueLength = toInteger $ RIO.length . show $ (baseAssetBalance + baseIn)

      maxPrecision = (if (yPartLength > xValueLength) then yPartLength else xValueLength) + 5

      xValuePrecise = 10 ^ xValueLength

      invariantLength = toInteger $ RIO.length . show $ prevInvariant

      xValueFloat = (fromIntegral baseAssetBalance) :: Double
      invariantFloat = fromIntegral prevInvariant :: BigDecimal
      xWeightFloat = (fromIntegral baseAssetWeight) :: BigDecimal
      yValueFloat = (fromIntegral quoteAssetBalance) :: BigDecimal
      yWeightFloat = (fromIntegral quoteAssetWeghit) :: BigDecimal
      treasuryFeeNum = (fromIntegral treasuryFee) :: Double
      lpFeeNum = (fromIntegral lpFee) :: Double

      xInInvariant = fromIntegral $ baseAssetBalance + round ((fromIntegral baseIn) * ((lpFeeNum - treasuryFeeNum) / feeDen))
      -- no decimals after point
      xInInvariantBigDecimal = BigDecimal xInInvariant 0
      -- xInInvariantBigDecimal in degree `(xWieght / 10)`
      xInInvariantWithDegree = (xInInvariantBigDecimal ** (fromRational $ ((fromIntegral baseAssetWeight) / 10))) -- g
      xInInvariantWith1Degree = (xInInvariantBigDecimal) ** (fromRational $ (1 / 10)) -- t

      gX = ((takeNBigDecimal xInInvariantWithDegree (maxPrecision)) :: Integer)
      tX = ((takeNBigDecimal xInInvariantWith1Degree (maxPrecision)) :: Integer)

      invariantDegreeProcessed = BigDecimal (takeNBigDecimal xInInvariantWithDegree (maxPrecision)) 0

      -- test
      xInInvariaintDownDegreeT = (fromIntegral xInInvariant :: Double) ** (((10 :: Double)) / (fromIntegral baseAssetWeight)) --BigDecimal (takeNBigDecimal (invariantDegreeProcessed ** 10) (maxPrecision)) 0

      xInInvariaintDownDegree = xInInvariaintDownDegreeT -- xInInvariaintDownDegreeT ** (fromRational $ (1 / fromIntegral baseAssetWeight))
      xInInvariantWithDegreeProcessed = takeNBigDecimal xInInvariantWithDegree xValueLength
      invDivision = invariantFloat / xInInvariantWithDegree
      invDivisionProcessed = (takeNBigDecimal invDivision yPartLength)
      --invDivisionInReverseRoot = (nthRoot invDivisionProcessed 4 (DOWN, (Just . toInteger $ xValuePrecise)))
      invDivisionInReverseDegree = round $ (fromInteger invDivisionProcessed :: Double) ** ((10 / (fromInteger quoteAssetWeghit)))
      -- denum = 10 ^ (yPartLength - xValueLength)
        
      invDivisionInReverseDegreeBigDecimalRounded = takeNBigDecimal (BigDecimal invDivisionInReverseDegree 0) (yPartLength)

      yToSwap = quoteAssetBalance - invDivisionInReverseDegreeBigDecimalRounded

      gYDouble = ((BigDecimal (quoteAssetBalance - yToSwap) 0) ** (fromRational $ (fromIntegral quoteAssetWeghit) / 10)) :: BigDecimal -- g
      tGDouble = (((BigDecimal (quoteAssetBalance - yToSwap) 0) ** (fromRational $ (1 / 10)))) :: BigDecimal -- g

      gY = takeNBigDecimal gYDouble (maxPrecision) -- ((getValue gYDouble))
      tY = takeNBigDecimal tGDouble (maxPrecision) -- ((getValue tGDouble))



  -------- Max test ---------


  let
    testRatio = 


  -------- Max test End ---------

  traceM $ T.pack $ "xValueLength:" ++ show xValueLength
  traceM $ T.pack $ "yPartLength:" ++ show yPartLength
  traceM $ T.pack $ "maxPrecision:" ++ show maxPrecision
  traceM $ T.pack $ "x token qty:" ++ show baseAssetBalance
  traceM $ T.pack $ "y token qty:" ++ show quoteAssetBalance
  traceM $ T.pack $ "gX:" ++ show gX
  traceM $ T.pack $ "tX:" ++ show tX
  traceM $ T.pack $ "xInInvariantBigDecimal:" ++ show (xInInvariantBigDecimal)
  traceM $ T.pack $ "invariantDegreeProcessed:" ++ show (invariantDegreeProcessed)
  traceM $ T.pack $ "(fromRational $ ((fromIntegral 10) / (toRational baseAssetWeight))):" ++ show ((BigDecimal 333 10))
  traceM $ T.pack $ "xInInvariaintDownDegreeT:" ++ show (xInInvariaintDownDegreeT)
  traceM $ T.pack $ "xInInvariaintDownDegree:" ++ show (xInInvariaintDownDegree)
  traceM $ T.pack $ "invDivision:" ++ show (invDivision)
  traceM $ T.pack $ "invDivisionProcessed:" ++ show (invDivisionProcessed)
  -- traceM $ T.pack $ "invDivisionInReverseRoot:" ++ show (invDivisionInReverseRoot)
  traceM $ T.pack $ "invDivisionInReverseDegree:" ++ show (invDivisionInReverseDegree)
  traceM $ T.pack $ "invDivisionInReverseDegree:" ++ show (invDivisionInReverseDegree)
  traceM $ T.pack $ "tX:" ++ show (invDivisionInReverseDegreeBigDecimalRounded)
  traceM $ T.pack $ "quoteAssetBalance:" ++ (show quoteAssetBalance)
  traceM $ T.pack $ "xToSwap:" ++ (show baseIn)
  traceM $ T.pack $ "yToSwap:" ++ (show yToSwap)
  -- traceM $ T.pack $ "xInInvariantWithDegree rounded:" ++ (show ((roundBD xInInvariantWithDegree (PRECISE, (Just . toInteger $ xValuePrecise)))))
  -- traceM $ T.pack $ "xInInvariantWith1Degree:" ++ (show xInInvariantWith1Degree)
  -- traceM $ T.pack $ "xInInvariantWith1WeightDegree:" ++ (show xInInvariantWith1WeightDegree)
  traceM $ T.pack $ "tY:" ++ (show tY)

  pure (gX, tX, gY, tY, yToSwap)

-- BaseAssetBalance -> BaseAssetWeight -> QuoteAssetBalance -> QuoteAssetWeghit -> BaseIn -> lqSupply -> lpFee -> treasuryFee -> (gBase, tBase, gQuote, tQuote, quoteToDeposit, lqOut)
-- calculateGandTDeposit :: Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> (Integer, Integer, Integer, Integer, Integer, Integer)
-- calculateGandTDeposit baseAssetBalance baseAssetWeight quoteAssetBalance quoteAssetWeghit baseIn lqSupply lpFee treasuryFee prevInvariant =
--   let
--       xValueFloat = (fromIntegral baseAssetBalance) :: Double
--       invariantFloat = fromIntegral prevInvariant :: Double
--       xWeightFloat = (fromIntegral baseAssetWeight) :: Double
--       yValueFloat = (fromIntegral quoteAssetBalance) :: Double
--       yWeightFloat = (fromIntegral quoteAssetWeghit) :: Double
--       treasuryFeeNum = (fromIntegral treasuryFee) :: Double
--       lpFeeNum = (fromIntegral lpFee) :: Double
--       lqSupplyDouble = (fromIntegral lqSupply) :: Double
--       baseInDouble = (fromIntegral baseIn) :: Double

--       xInInvariant = fromIntegral $ (baseAssetBalance + baseIn)
--       xInInvariantWithDegree = (xInInvariant :: Double) ** (xWeightFloat / (10 :: Double)) -- g
--       xInInvariantWith1Degree = (xInInvariant :: Double) ** (1 / (10 :: Double)) -- t
--       xInInvariantWith1WeightDegree = (xInInvariantWith1Degree) ** xWeightFloat
--       maxDen = 15

--       gX = ((read $ List.delete '.' $ showFFloat (Just maxDen) xInInvariantWithDegree "") :: Integer)
--       tX = ((read $ List.delete '.' $ showFFloat (Just maxDen) xInInvariantWith1Degree "") :: Integer)

--       lqIssued = round $ (baseInDouble * lqSupplyDouble) / xValueFloat

--       yToDeposit = round (((lqSupplyDouble + lqIssued) / lqSupplyDouble) - 1) * quoteAssetBalance

--       gYDouble = (fromIntegral $ quoteAssetBalance + yToDeposit) ** (yWeightFloat / (10 :: Double)) -- g
--       tGDouble = (fromIntegral $ quoteAssetBalance + yToDeposit) ** (1 / (10 :: Double)) -- g

--       gY = ((read $ List.delete '.' $ showFFloat (Just maxDen) gYDouble "") :: Integer)
--       tY = ((read $ List.delete '.' $ showFFloat (Just maxDen) tGDouble "") :: Integer)
--   in (gX, tX, gY, tY, yToDeposit, lqIssued)

--- Test cases ---

cutFloat :: Double -> Int -> Integer
cutFloat toCut maxInt = let
    strValue = T.pack $ showFFloat (Just maxInt) toCut ""
    splitted = splitOn "." strValue
  in read $ T.unpack . Prelude.head $ splitted

-- Swap cases start --

correctSwap :: MonadGen m => BalancePoolTestAction m
correctSwap = 
  let
    testAction prevPool@BalancePool{..} = do

      let
        (xCS, xTN) = unAssetClass (poolX config)
        (yCS, yTN) = unAssetClass (poolY config)
        xValue = valueOf value xCS xTN
        yValue = valueOf value yCS yTN

      xToSwap <- integral (Range.constant 10000 10000000)
      (gX, tX, gY, tY, yToSwap) <- calculateGandTSwap xValue (weightX config) yValue (weightY config) (xToSwap) (poolFeeNum config) (treasuryFee config) (invariant config)
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

incorrectSwapGT :: MonadGen m => BalancePoolTestAction m
incorrectSwapGT = 
  let
    testAction prevPool@BalancePool{..} = do

      let
        (xCS, xTN) = unAssetClass (poolX config)
        (yCS, yTN) = unAssetClass (poolY config)
        xValue = valueOf value xCS xTN
        yValue = valueOf value yCS yTN

      xToSwap <- integral (Range.constant 1 ((xValue `div` 2) - 1))
      (gX, tX, gY, tY, yToSwap) <- calculateGandTSwap xValue (weightX config) yValue (weightY config) (xToSwap + 10) (poolFeeNum config) (treasuryFee config) (invariant config)
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
  in BalancePoolTestAction "Incorrect swap GT" testAction

incorrectSwapPoolFinalXValue :: MonadGen m => BalancePoolTestAction m
incorrectSwapPoolFinalXValue = 
  let
    testAction prevPool@BalancePool{..} = do
      let
        (xCS, xTN) = unAssetClass (poolX config)
        (yCS, yTN) = unAssetClass (poolY config)
        xValue = valueOf value xCS xTN
        yValue = valueOf value yCS yTN

      xToSwap <- integral (Range.constant 1 ((xValue `div` 2) - 1))
      incorrectXSwapValue <- integral (Range.constant 1 ((xValue `div` 2) - 1))
      (gX, tX, gY, tY, yToSwap) <- calculateGandTSwap xValue (weightX config) yValue (weightY config) (xToSwap) (poolFeeNum config) (treasuryFee config) (invariant config)
      let
        -- going to withdraw all pool x and y value
        newPoolConfig = config 
          { treasuryX = 0
          , treasuryY = 0
          }

        newPool = prevPool 
          { config = newPoolConfig
          , value  = value <> (assetClassValue (poolX config) (incorrectXSwapValue)) <> (assetClassValue (poolY config) (negate yToSwap))
          }

      pure $ BalancePoolActionResult newPool [] [gX, gY] [tX, tY] (toInteger 15)
  in BalancePoolTestAction "Incorrect pool x final value" testAction

incorrectSwapPoolFinalYValue :: MonadGen m => BalancePoolTestAction m
incorrectSwapPoolFinalYValue = 
  let
    testAction prevPool@BalancePool{..} = do

      let
        (xCS, xTN) = unAssetClass (poolX config)
        (yCS, yTN) = unAssetClass (poolY config)
        xValue = valueOf value xCS xTN
        yValue = valueOf value yCS yTN

      xToSwap <- integral (Range.constant 1 ((xValue `div` 2) - 1))
      incorrectYFinalValue <- integral (Range.constant 1 (yValue - 1))
      (gX, tX, gY, tY, yToSwap) <- calculateGandTSwap xValue (weightX config) yValue (weightY config) (xToSwap) (poolFeeNum config) (treasuryFee config) (invariant config)

      let
        -- going to withdraw all pool x and y value
        newPoolConfig = config 
          { treasuryX = 0
          , treasuryY = 0
          }

        newPool = prevPool 
          { config = newPoolConfig
          , value  = value <> (assetClassValue (poolX config) (xToSwap)) <> (assetClassValue (poolY config) (negate incorrectYFinalValue))
          }

      pure $ BalancePoolActionResult newPool [] [gX, gY] [tX, tY] (toInteger 15)
  in BalancePoolTestAction "Incorrect pool y final value" testAction

-- Swap cases end --

-- Deposit cases start --

-- correctDeposit :: MonadGen m => BalancePoolTestAction m
-- correctDeposit = 
--   let
--     testAction prevPool@BalancePool{..} = do

--       let
--         (xCS, xTN) = unAssetClass (poolX config)
--         (yCS, yTN) = unAssetClass (poolY config)
--         (lqCS, lqTN) = unAssetClass (poolLq config)
--         xValue = valueOf value xCS xTN
--         yValue = valueOf value yCS yTN
--         lqValue = valueOf value lqCS lqCS

--       xToDeposit <- integral (Range.constant 1 ((xValue `div` 2) - 1))

--       let
--         lqIssued = 0x7fffffffffffffff - (round lqValue)

--         (gX, tX, gY, tY, yToDeposit, lqIssued) = calculateGandTDeposit xValue (weightX config) yValue (weightY config) (xToSwap) lqIssued (poolFeeNum config) (treasuryFee config) (invariant config)

--         newInvariant = gX * gY

--         -- going to withdraw all pool x and y value
--         newPoolConfig = config 
--           { treasuryX = 0
--           , treasuryY = 0
--           , invariant = newInvariant
--           }

--         newPool = prevPool 
--           { config = newPoolConfig
--           , value  = value <> (assetClassValue (poolX config) (xToSwap)) <> (assetClassValue (poolY config) (yToDeposit)) <> (assetClassValue (poolLq config) (negate lqIssued))
--           }

--       pure $ BalancePoolActionResult newPool [] [gX, gY] [tX, tY] (toInteger 15)
--   in BalancePoolTestAction "Correct deposit all tokens" testAction

-- Deposit cases end --