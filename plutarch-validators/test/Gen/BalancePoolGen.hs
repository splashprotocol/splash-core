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
precisionAdditionalDec = 10

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

getDecimalNum :: BigDecimal -> Integer
getDecimalNum toCut =
  let
    currentScale = getScale toCut
    inString = T.unpack (T.dropEnd (fromInteger currentScale) (T.pack . show $ (Data.BigDecimal.getValue toCut)))
  in (read inString) :: Integer

genBalancePool :: MonadGen f => [PubKeyHash] -> Integer -> Bool -> f BalancePool
genBalancePool adminsPkhs threshold lpFeeIsEditable = do
  (x, y, lq, nft) <- tuple4 genAssetClass

  stakeHash <- genPkh
  
  -- todo: error on big values such as 10 000 000 000 000 000  
  (yQty :: Int) <- integral (Range.constant 10000000000 10000000000000000)

  (xWeight :: Integer) <- integral (Range.constant 1 5)
  (xQty :: Integer) <- integral (Range.constant 1000000000 1000000000000)
  poolFee <- integral (Range.constant 80000 feeDen)
  trFee <- integral (Range.constant 1 1000)
  -- let trFee = 683
  -- let poolFee = 86472
  -- let xWeight = 5
  -- let yWeight = 5
  -- let xQty = 303457031315

  treasuryAddress <- genValidatorHash
  let
    yWeight = 10 - xWeight

    yQty = xQty * yWeight
    nftQty = 1

    xQtyFloat = (fromIntegral xQty) :: Double
    yQtyFloat = (fromIntegral yQty) :: Double

    yPartLength  = toInteger $ RIO.length . show $ xQty
    xValueLength = toInteger $ RIO.length . show $ yQty

    maxPrecision = (if (yPartLength > xValueLength) then yPartLength else xValueLength) + precisionAdditionalDec

    -- invariant = (xQtyFloat**(xWeightFloat / 10)) * (yQtyFloat**(yWeightFloat / 10))
    invariantT = ((BigDecimal xQty 0) ** (fromRational $ (fromIntegral xWeight) / 10)) * ( (BigDecimal yQty 0) ** (fromRational $ (fromIntegral yWeight) / 10))
    invariant = getDecimalNum invariantT

    lqQty  = 0x7fffffffffffffff - invariant

    daoContract =
        StakingHash . ScriptCredential . ValidatorHash . getScriptHash . scriptHash $ (unMintingPolicyScript (daoMintPolicyValidator nft adminsPkhs threshold lpFeeIsEditable))

    leftSide = (BigDecimal invariant 0) / ((BigDecimal xQty 0) ** (fromRational $ (fromIntegral xWeight) / 10))

  let
    poolConfig = BalancePoolConfig
      { poolNft = nft
      , poolX   = x
      , weightX = xWeight
      , poolY   = y
      , weightY = yWeight
      , poolLq  = lq
      , poolFeeNum  = poolFee
      , treasuryFee = trFee
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

      maxPrecision = (if (yPartLength > xValueLength) then yPartLength else xValueLength) + precisionAdditionalDec

      invariantLength = toInteger $ RIO.length . show $ prevInvariant

      xValueFloat = (fromIntegral baseAssetBalance) :: BigDecimal
      invariantFloat = (BigDecimal prevInvariant 0) :: BigDecimal
      xWeightFloat = (fromIntegral baseAssetWeight) :: BigDecimal
      yValueFloat = (fromIntegral quoteAssetBalance) :: BigDecimal
      yWeightFloat = (fromIntegral quoteAssetWeghit) :: BigDecimal
      treasuryFeeNum = (fromIntegral treasuryFee) :: Double
      lpFeeNum = (fromIntegral lpFee) :: Double

      additionalPart = (BigDecimal (fromIntegral baseIn) 0) * (fromRational $ (fromIntegral (lpFee - treasuryFee)) / fromIntegral feeDen) --     xInInvariant = fromIntegral $ baseAssetBalance + round ((fromIntegral baseIn) * ((lpFeeNum - treasuryFeeNum) / fromIntegral feeDen))
      -- no decimals after point
      xInInvariantBigDecimal = xValueFloat + additionalPart
      -- xInInvariantBigDecimal in degree `(xWieght / 10)`
      xInInvariantWithDegree = (xInInvariantBigDecimal ** (fromRational $ ((fromIntegral baseAssetWeight) / 10))) -- g
      xInInvariantWith1Degree = (xInInvariantBigDecimal) ** (fromRational $ (1 / 10)) -- t

      gX = ((takeNBigDecimal xInInvariantWithDegree (maxPrecision)) :: Integer)
      tX = ((takeNBigDecimal xInInvariantWith1Degree (maxPrecision)) :: Integer)

      -- test
      invDivision = invariantFloat / xInInvariantWithDegree
      invDivisionInReverseDegree = nthRoot (invDivision ** 10) (fromInteger quoteAssetWeghit) (DOWN, (Just . toInteger $ 0))
      -- denum = 10 ^ (yPartLength - xValueLength)
        
      invDivisionInReverseDegreeBigDecimalRounded = takeNBigDecimal invDivisionInReverseDegree (yPartLength)

      yToSwap = quoteAssetBalance - invDivisionInReverseDegreeBigDecimalRounded

      gYDouble = ((BigDecimal (quoteAssetBalance - yToSwap) 0) ** (fromRational $ (fromIntegral quoteAssetWeghit) / 10)) :: BigDecimal -- g
      tGDouble = (((BigDecimal (quoteAssetBalance - yToSwap) 0) ** (fromRational $ (1 / 10)))) :: BigDecimal -- g

      gY = takeNBigDecimal gYDouble (maxPrecision) -- ((getValue gYDouble))
      tY = takeNBigDecimal tGDouble (maxPrecision) -- ((getValue tGDouble))

      spotPriceWithoutFee = (((BigDecimal baseAssetBalance 0)) / ((BigDecimal baseAssetWeight 0))) / (((BigDecimal quoteAssetBalance 0)) / ((BigDecimal quoteAssetWeghit 0))) :: BigDecimal
      spotPriceWithFee = spotPriceWithoutFee * (fromRational $ (fromIntegral (lpFee - treasuryFee) / fromIntegral (feeDen)))

  pure (gX, tX, gY, tY, yToSwap)

-- BaseAssetBalance -> BaseAssetWeight -> QuoteAssetBalance -> QuoteAssetWeghit -> BaseIn -> lqSupply -> lpFee -> treasuryFee -> (gBase, tBase, gQuote, tQuote, quoteToDeposit, lqOut)
calculateGandTDeposit :: MonadGen m => Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> m (Integer, Integer, Integer, Integer, Integer, Integer, Integer)
calculateGandTDeposit baseAssetBalance baseAssetWeight quoteAssetBalance quoteAssetWeghit lqIssued lqSupply lpFee treasuryFee prevInvariant = do
  let
      lqIssuedDec = (fromIntegral lqIssued) :: BigDecimal
      xToDeposit = roundBD ((lqIssuedDec * xValueFloat) / lqSupplyDouble) (UP, (Just . toInteger $ 0))
      yToDeposit = roundBD ((lqIssuedDec * yValueFloat) / lqSupplyDouble) (UP, (Just . toInteger $ 0))

          -- yToDeposit = (((lqSupplyDouble + lqIssued) / lqSupplyDouble) - 1) * yValueFloat
      yPartLength = toInteger $ RIO.length . show $ quoteAssetBalance
      xValueLength = toInteger $ RIO.length . show $ (xValueFloat + xToDeposit)

      maxPrecision = (if (yPartLength > xValueLength) then yPartLength else xValueLength)

      xValueFloat = (fromIntegral baseAssetBalance) :: BigDecimal
      invariantFloat = fromIntegral prevInvariant :: BigDecimal
      xWeightFloat = (fromIntegral baseAssetWeight) :: BigDecimal
      yValueFloat = (fromIntegral quoteAssetBalance) :: BigDecimal
      yWeightFloat = (fromIntegral quoteAssetWeghit) :: BigDecimal
      treasuryFeeNum = (fromIntegral treasuryFee) :: BigDecimal
      lpFeeNum = (fromIntegral lpFee) :: BigDecimal
      lqSupplyDouble = (fromIntegral lqSupply) :: BigDecimal
      -- no decimals after point
      xInInvariantBigDecimal = xValueFloat + xToDeposit
      -- xInInvariantBigDecimal in degree `(xWieght / 10)`
      xInInvariantWithDegree = (xInInvariantBigDecimal ** (fromRational $ ((fromIntegral baseAssetWeight) / 10))) -- g
      xInInvariantWith1Degree = (xInInvariantBigDecimal) ** (fromRational $ (1 / 10)) -- t

      gX = ((takeNBigDecimal xInInvariantWithDegree (maxPrecision)) :: Integer)
      tX = ((takeNBigDecimal xInInvariantWith1Degree (maxPrecision)) :: Integer)

      gBase = yValueFloat + yToDeposit
      gBaseRounded = BigDecimal (getDecimalNum (yValueFloat + yToDeposit)) 0
      gYDouble = nthRoot (gBaseRounded ** yWeightFloat) 10 (UP, (Just . toInteger $ maxPrecision)) :: BigDecimal -- g

      tGDoubleTest = nthRoot gBaseRounded 10 (DOWN, (Just . toInteger $ maxPrecision))
      tGDouble     = nthRoot gBaseRounded 10 (DOWN, (Just . toInteger $ maxPrecision))
      
      gY = takeNBigDecimal gYDouble (maxPrecision)
      tY = takeNBigDecimal tGDouble (maxPrecision)

  pure (gX, tX, gY, tY, getDecimalNum xToDeposit, getDecimalNum yToDeposit, getDecimalNum lqIssuedDec)

calculateGandTRedeem :: MonadGen m => Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> m (Integer, Integer, Integer, Integer, Integer, Integer)
calculateGandTRedeem baseAssetBalance baseAssetWeight quoteAssetBalance quoteAssetWeghit lqRedeem lqSupply lpFee treasuryFee prevInvariant = do
  let
      lqRedeemDec = (fromIntegral lqRedeem) :: BigDecimal
      xToRedeem = roundBD ((lqRedeemDec * xValueFloat) / lqSupplyDouble) (DOWN, (Just . toInteger $ 0))
      yToRedeem = roundBD ((lqRedeemDec * yValueFloat) / lqSupplyDouble) (DOWN, (Just . toInteger $ 0))

      yPartLength = toInteger $ RIO.length . show $ quoteAssetBalance
      xValueLength = toInteger $ RIO.length . show $ (xValueFloat - xToRedeem)

      maxPrecision = (if (yPartLength > xValueLength) then yPartLength else xValueLength)

      xValueFloat = (fromIntegral baseAssetBalance) :: BigDecimal
      invariantFloat = fromIntegral prevInvariant :: BigDecimal
      xWeightFloat = (fromIntegral baseAssetWeight) :: BigDecimal
      yValueFloat = (fromIntegral quoteAssetBalance) :: BigDecimal
      yWeightFloat = (fromIntegral quoteAssetWeghit) :: BigDecimal
      treasuryFeeNum = (fromIntegral treasuryFee) :: BigDecimal
      lpFeeNum = (fromIntegral lpFee) :: BigDecimal
      lqSupplyDouble = (fromIntegral lqSupply) :: BigDecimal
      -- no decimals after point
      xInInvariantBigDecimal = xValueFloat - xToRedeem
      -- xInInvariantBigDecimal in degree `(xWieght / 10)`
      xInInvariantWithDegree = (xInInvariantBigDecimal ** (fromRational $ ((fromIntegral baseAssetWeight) / 10))) -- g
      xInInvariantWith1Degree = (xInInvariantBigDecimal) ** (fromRational $ (1 / 10)) -- t

      gX = ((takeNBigDecimal xInInvariantWithDegree (maxPrecision)) :: Integer)
      tX = ((takeNBigDecimal xInInvariantWith1Degree (maxPrecision)) :: Integer)

      gBase = yValueFloat - yToRedeem
      gBaseRounded = BigDecimal (getDecimalNum (yValueFloat - yToRedeem)) 0
      gYDouble = nthRoot (gBaseRounded ** yWeightFloat) 10 (UP, (Just . toInteger $ maxPrecision)) :: BigDecimal -- g

      tGDoubleTest = nthRoot gBaseRounded 10 (DOWN, (Just . toInteger $ maxPrecision))
      tGDouble     = nthRoot gBaseRounded 10 (DOWN, (Just . toInteger $ maxPrecision))
      
      gY = takeNBigDecimal gYDouble (maxPrecision)
      tY = takeNBigDecimal tGDouble (maxPrecision)

  pure (gX, tX, gY, tY, getDecimalNum xToRedeem, getDecimalNum yToRedeem)

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
      xToSwap <- integral (Range.constant 100 xValue)
      (gX, tX, gY, tY, yToSwap) <- calculateGandTSwap xValue (weightX config) yValue (weightY config) (xToSwap) (poolFeeNum config) (treasuryFee config) (invariant config)
      let
        -- going to withdraw all pool x and y value
        tFee = treasuryFee config

        newPoolConfig = config 
          { treasuryX = (tFee * xToSwap) `div` feeDen
          , treasuryY = 0
          }

        newPool = prevPool 
          { config = newPoolConfig
          , value  = value <> (assetClassValue (poolX config) (xToSwap)) <> (assetClassValue (poolY config) (negate yToSwap))
          }

      pure $ BalancePoolActionResult newPool [] [gX, gY] [tX, tY]
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
      (gX, tX, gY, tY, yToSwap) <- calculateGandTSwap xValue (weightX config) yValue (weightY config) (xToSwap + 1000) (poolFeeNum config) (treasuryFee config) (invariant config)
      let
        newPool = prevPool 
          { value  = value <> (assetClassValue (poolX config) (xToSwap)) <> (assetClassValue (poolY config) (negate yToSwap))
          }

      pure $ BalancePoolActionResult newPool [] [gX, gY] [tX, tY]
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
        tFee = treasuryFee config

      xToSwap <- integral (Range.constant 1 ((xValue `div` 2) - 1))
      incorrectXSwapValue <- integral (Range.constant 1 ((xValue `div` 2) - 1))
      (gX, tX, gY, tY, yToSwap) <- calculateGandTSwap xValue (weightX config) yValue (weightY config) (xToSwap) (poolFeeNum config) (treasuryFee config) (invariant config)
      let
        tFee = treasuryFee config

        newPoolConfig = config 
          { treasuryX = (tFee * xToSwap) `div` feeDen
          , treasuryY = 0
          }

        newPool = prevPool
          { value  = value <> (assetClassValue (poolX config) (incorrectXSwapValue)) <> (assetClassValue (poolY config) (negate yToSwap))
          }

      pure $ BalancePoolActionResult newPool [] [gX, gY] [tX, tY]
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
        tFee = treasuryFee config

        newPoolConfig = config 
          { treasuryX = (tFee * xToSwap) `div` feeDen
          , treasuryY = 0
          }

        newPool = prevPool 
          { value  = value <> (assetClassValue (poolX config) (xToSwap)) <> (assetClassValue (poolY config) (negate incorrectYFinalValue))
          }

      pure $ BalancePoolActionResult newPool [] [gX, gY] [tX, tY]
  in BalancePoolTestAction "Incorrect pool y final value" testAction

incorrectSwapTrFeeValue :: MonadGen m => BalancePoolTestAction m
incorrectSwapTrFeeValue = 
  let
    testAction prevPool@BalancePool{..} = do

      let
        (xCS, xTN) = unAssetClass (poolX config)
        (yCS, yTN) = unAssetClass (poolY config)
        xValue = valueOf value xCS xTN
        yValue = valueOf value yCS yTN

      xToSwap <- integral (Range.constant 1 ((xValue `div` 2) - 1))
      (gX, tX, gY, tY, yToSwap) <- calculateGandTSwap xValue (weightX config) yValue (weightY config) (xToSwap) (poolFeeNum config) (treasuryFee config) (invariant config)
      let
        treasuryFee_ = treasuryFee config
        newPoolConfig = config 
          { treasuryX = (treasuryFee_ * (xToSwap)) `div` feeDen - 1
          }
        newPool = prevPool 
          { config = newPoolConfig
          , value  = value <> (assetClassValue (poolX config) (xToSwap)) <> (assetClassValue (poolY config) (negate yToSwap))
          }

      pure $ BalancePoolActionResult newPool [] [gX, gY] [tX, tY]
  in BalancePoolTestAction "Incorrect pool treasury X final value" testAction

-- Swap cases end --

-- Deposit all cases start --

correctDeposit :: MonadGen m => BalancePoolTestAction m
correctDeposit = 
  let
    testAction prevPool@BalancePool{..} = do

      let
        (xCS, xTN) = unAssetClass (poolX config)
        (yCS, yTN) = unAssetClass (poolY config)
        (lqCS, lqTN) = unAssetClass (poolLq config)
        xValue = valueOf value xCS xTN
        yValue = valueOf value yCS yTN
        lqValue = valueOf value lqCS lqTN
        lqSupply = 0x7fffffffffffffff - lqValue

      lqIssued <- integral (Range.constant 1 ((lqValue `div` 2) - 1))

      (gX, tX, gY, tY, xToDeposit, yToDeposit, lqIssued) <- calculateGandTDeposit xValue (weightX config) yValue (weightY config) (lqIssued) lqSupply (poolFeeNum config) (treasuryFee config) (invariant config)

      let
        -- lqIssued = 0x7fffffffffffffff - (round lqValue)

        newInvariant = gX * gY

        -- going to withdraw all pool x and y value
        newPoolConfig = config 
          { invariant = newInvariant
          }

        newPool = prevPool 
          { config = newPoolConfig
          , value  = value <> (assetClassValue (poolX config) (xToDeposit)) <> (assetClassValue (poolY config) (yToDeposit)) <> (assetClassValue (poolLq config) (negate lqIssued))
          }

      pure $ BalancePoolActionResult newPool [] [gX, gY] [tX, tY]
  in BalancePoolTestAction "Correct deposit all tokens" testAction

incorrectDepositLqOut :: MonadGen m => BalancePoolTestAction m
incorrectDepositLqOut = 
  let
    testAction prevPool@BalancePool{..} = do

      let
        (xCS, xTN) = unAssetClass (poolX config)
        (yCS, yTN) = unAssetClass (poolY config)
        (lqCS, lqTN) = unAssetClass (poolLq config)
        xValue = valueOf value xCS xTN
        yValue = valueOf value yCS yTN
        lqValue = valueOf value lqCS lqTN
        lqSupply = 0x7fffffffffffffff - lqValue

      lqIssued <- integral (Range.constant 1 ((lqValue `div` 2) - 1))

      (gX, tX, gY, tY, xToDeposit, yToDeposit, lqIssued) <- calculateGandTDeposit xValue (weightX config) yValue (weightY config) (lqIssued) lqSupply (poolFeeNum config) (treasuryFee config) (invariant config)

      let
        -- lqIssued = 0x7fffffffffffffff - (round lqValue)

        newInvariant = gX * gY

        -- going to withdraw all pool x and y value
        newPoolConfig = config 
          { invariant = newInvariant
          }

        newPool = prevPool 
          { config = newPoolConfig
          , value  = value <> (assetClassValue (poolX config) (xToDeposit)) <> (assetClassValue (poolY config) (yToDeposit)) <> (assetClassValue (poolLq config) (negate (lqIssued + 1000)))
          }

      pure $ BalancePoolActionResult newPool [] [gX, gY] [tX, tY]
  in BalancePoolTestAction "Incorrect deposit all tokens. Incorrect lq out" testAction

-- Deposit all cases end --

-- Redeem all cases start --

correctRedeem :: MonadGen m => BalancePoolTestAction m
correctRedeem = 
  let
    testAction prevPool@BalancePool{..} = do

      let
        (xCS, xTN) = unAssetClass (poolX config)
        (yCS, yTN) = unAssetClass (poolY config)
        (lqCS, lqTN) = unAssetClass (poolLq config)
        xValue = valueOf value xCS xTN
        yValue = valueOf value yCS yTN
        lqValue = valueOf value lqCS lqTN
        lqIssued = 0x7fffffffffffffff - lqValue
      -- let lqToRedeem = 85989149586251
      lqToRedeem <- integral (Range.constant 1 ((lqIssued `div` 2) - 1))
      (gX, tX, gY, tY, xToRedeem, yToRedeem) <- calculateGandTRedeem xValue (weightX config) yValue (weightY config) (lqToRedeem) lqIssued (poolFeeNum config) (treasuryFee config) (invariant config)

      let
        -- lqIssued = 0x7fffffffffffffff - (round lqValue)

        newInvariant = gX * gY

        -- going to withdraw all pool x and y value
        newPoolConfig = config 
          { invariant = newInvariant
          }

        newPool = prevPool 
          { config = newPoolConfig
          , value  = value <> (assetClassValue (poolX config) (negate xToRedeem)) <> (assetClassValue (poolY config) (negate yToRedeem)) <> (assetClassValue (poolLq config) (lqToRedeem))
          }

      pure $ BalancePoolActionResult newPool [] [gX, gY] [tX, tY]
  in BalancePoolTestAction "Correct redeem all tokens" testAction

incorrectRedeemLQFinalValue :: MonadGen m => BalancePoolTestAction m
incorrectRedeemLQFinalValue = 
  let
    testAction prevPool@BalancePool{..} = do

      let
        (xCS, xTN) = unAssetClass (poolX config)
        (yCS, yTN) = unAssetClass (poolY config)
        (lqCS, lqTN) = unAssetClass (poolLq config)
        xValue = valueOf value xCS xTN
        yValue = valueOf value yCS yTN
        lqValue = valueOf value lqCS lqTN
        lqIssued = 0x7fffffffffffffff - lqValue

      lqToRedeem <- integral (Range.constant 1000 ((lqIssued `div` 2) - 1))

      (gX, tX, gY, tY, xToRedeem, yToRedeem) <- calculateGandTRedeem xValue (weightX config) yValue (weightY config) (lqToRedeem) lqIssued (poolFeeNum config) (treasuryFee config) (invariant config)

      let
        -- lqIssued = 0x7fffffffffffffff - (round lqValue)

        newInvariant = gX * gY

        -- going to withdraw all pool x and y value
        newPoolConfig = config 
          { invariant = newInvariant
          }

        newPool = prevPool 
          { config = newPoolConfig
          , value  = value <> (assetClassValue (poolX config) (negate xToRedeem)) <> (assetClassValue (poolY config) (negate yToRedeem)) <> (assetClassValue (poolLq config) (lqToRedeem - 100))
          }

      pure $ BalancePoolActionResult newPool [] [gX, gY] [tX, tY]
  in BalancePoolTestAction "InCorrect redeem all tokens. Incorrect final lq value" testAction

-- Redeem all cases end --