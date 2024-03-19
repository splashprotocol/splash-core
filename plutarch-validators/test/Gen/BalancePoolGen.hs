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
  
  -- todo: error on big values such as 10000000000000000
  (xQty :: Integer) <- integral (Range.constant 10000000000 10000000000000000)
  -- (yQty :: Int) <- integral (Range.constant 10000000000 10000000000000000)

  -- todo: doesn't work for non 2/8 pools
  --(xWeight :: Integer) <- integral (Range.constant 1 9)

  poolFee <- integral (Range.constant 80000 feeDen)
  trFee <- integral (Range.constant 0 1000)
  treasuryAddress <- genValidatorHash
  let
    --xQty = 1325954420705621
    xWeight = 2
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
    invariant = getDecimalNum invariantT

    lqQty  = 0x7fffffffffffffff - invariant

    daoContract =
        StakingHash . ScriptCredential . ValidatorHash . getScriptHash . scriptHash $ (unMintingPolicyScript (daoMintPolicyValidator nft adminsPkhs threshold lpFeeIsEditable))

    leftSide = (BigDecimal invariant 0) / ((BigDecimal xQty 0) ** (fromRational $ (fromIntegral xWeight) / 10))

  -- traceM $ T.pack $ "leftInvariant side:" ++ show (((BigDecimal xQty 0) ** (fromRational $ (fromIntegral xWeight) / 10)))
  -- traceM $ T.pack $ "rightInvariant side:" ++ show ( (BigDecimal yQty 0) ** (fromRational $ (fromIntegral yWeight) / 10))
  -- traceM $ T.pack $ "invariantT:" ++ show (invariantT)
  -- traceM $ T.pack $ "invariant:" ++ show (invariant)
  -- traceM $ T.pack $ "invariantT test devision: " ++ show (invariantT / ((BigDecimal xQty 0) ** (fromRational $ (fromIntegral xWeight) / 10)))
  -- traceM $ T.pack $ "invariant test devision: " ++ show ((BigDecimal invariant 0) / ((BigDecimal xQty 0) ** (fromRational $ (fromIntegral xWeight) / 10)))
  -- traceM $ T.pack $ "y part in degree:" ++ show ( (BigDecimal yQty 0) ** (fromRational $ (fromIntegral yWeight) / 10))
  -- traceM $ T.pack $ "y part in reverse: " ++ show ((nthRoot (leftSide ** (10))) 8 (DOWN, (Just . toInteger $ 10)))
  -- traceM $ T.pack $ "y part in original:" ++ show ( (BigDecimal yQty 0) ** (fromRational $ (fromIntegral yWeight) / 10))

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

      maxPrecisionByToken = (if (yPartLength > xValueLength) then yPartLength else xValueLength)

      maxPrecision = (if (yPartLength > xValueLength) then yPartLength else xValueLength) + 5

      xValuePrecise = 10 ^ xValueLength

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
      invDivisionInReverseDegree = nthRoot (invDivision ** 10) (fromInteger quoteAssetWeghit) (DOWN, (Just . toInteger $ 10))
      -- denum = 10 ^ (yPartLength - xValueLength)
        
      invDivisionInReverseDegreeBigDecimalRounded = takeNBigDecimal invDivisionInReverseDegree (yPartLength)

      yToSwap = quoteAssetBalance - invDivisionInReverseDegreeBigDecimalRounded

      gYDouble = ((BigDecimal (quoteAssetBalance - yToSwap) 0) ** (fromRational $ (fromIntegral quoteAssetWeghit) / 10)) :: BigDecimal -- g
      tGDouble = (((BigDecimal (quoteAssetBalance - yToSwap) 0) ** (fromRational $ (1 / 10)))) :: BigDecimal -- g

      gY = takeNBigDecimal gYDouble (maxPrecision) -- ((getValue gYDouble))
      tY = takeNBigDecimal tGDouble (maxPrecision) -- ((getValue tGDouble))



  -------- Max test ---------


  let
    spotPriceWithoutFee = (((BigDecimal baseAssetBalance 0)) / ((BigDecimal baseAssetWeight 0))) / (((BigDecimal quoteAssetBalance 0)) / ((BigDecimal quoteAssetWeghit 0))) :: BigDecimal
    spotPriceWithFee = spotPriceWithoutFee * (fromRational $ (fromIntegral (lpFee - treasuryFee) / fromIntegral (feeDen)))

  -- traceM $ T.pack $ "spotPriceWithoutFee:" ++ show spotPriceWithoutFee
  -- traceM $ T.pack $ "spotPriceWithFee:" ++ show spotPriceWithFee

  -------- Max test End ---------

  -- traceM $ T.pack $ "xValueLength:" ++ show xValueLength
  -- traceM $ T.pack $ "yPartLength:" ++ show yPartLength
  -- traceM $ T.pack $ "maxPrecision:" ++ show maxPrecision
  -- traceM $ T.pack $ "gX:" ++ show gX
  -- traceM $ T.pack $ "tX:" ++ show tX
  
  ---
  -- traceM $ T.pack $ "-----------------------------------------324" ++ show (xInInvariantBigDecimal)

  --traceM $ T.pack $ "x token qty:" ++ show baseAssetBalance
  --traceM $ T.pack $ "y token qty:" ++ show quoteAssetBalance

  -- traceM $ T.pack $ "prev invariant:" ++ show prevInvariant
  -- traceM $ T.pack $ "new invariant: " ++ show (gX * gY)
  -- traceM $ T.pack $ "left invariant side:" ++ show xInInvariantWithDegree
  -- traceM $ T.pack $ "right invariant side:" ++ show invDivision
  -- traceM $ T.pack $ "invariant test:" ++ show (xInInvariantWithDegree * invDivision)

  -- traceM $ T.pack $ "xInInvariantBigDecimal:" ++ show (xInInvariantBigDecimal)
  -- traceM $ T.pack $ "xInInvariantWithDegree:" ++ (show xInInvariantWithDegree)
  -- traceM $ T.pack $ "invDivision:" ++ (show invDivision)
  -- traceM $ T.pack $ "invDivisionInReverseDegree:" ++ (show invDivisionInReverseDegree)
  -- traceM $ T.pack $ "invDivisionInReverseDegreeBigDecimalRounded:" ++ (show invDivisionInReverseDegreeBigDecimalRounded)

  --traceM $ T.pack $ "----------------------------------------- " ++ (show invDivisionInReverseDegree)
  ----
  -- traceM $ T.pack $ "invDivision:" ++ show (invDivision)
  -- -- traceM $ T.pack $ "invDivisionInReverseRoot:" ++ show (invDivisionInReverseRoot)
  -- traceM $ T.pack $ "invDivisionInReverseDegree:" ++ show (invDivisionInReverseDegree)
  -- traceM $ T.pack $ "invDivisionInReverseDegree:" ++ show (invDivisionInReverseDegree)
  -- traceM $ T.pack $ "tX:" ++ show (invDivisionInReverseDegreeBigDecimalRounded)
  -- traceM $ T.pack $ "quoteAssetBalance:" ++ (show quoteAssetBalance)
 -- traceM $ T.pack $ "xToSwap:" ++ (show baseIn)
  --traceM $ T.pack $ "yToSwap:" ++ (show yToSwap)
  -- traceM $ T.pack $ "xInInvariantWith1Degree:" ++ (show xInInvariantWith1Degree)
  -- traceM $ T.pack $ "xInInvariantWith1WeightDegree:" ++ (show xInInvariantWith1WeightDegree)
  --traceM $ T.pack $ "tY:" ++ (show tY)

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

      maxPrecisionByToken = (if (yPartLength > xValueLength) then yPartLength else xValueLength)

      maxPrecision = (if (yPartLength > xValueLength) then yPartLength else xValueLength) + 5

      xValueFloat = (fromIntegral baseAssetBalance) :: BigDecimal
      invariantFloat = fromIntegral prevInvariant :: BigDecimal
      xWeightFloat = (fromIntegral baseAssetWeight) :: BigDecimal
      yValueFloat = (fromIntegral quoteAssetBalance) :: BigDecimal
      yWeightFloat = (fromIntegral quoteAssetWeghit) :: BigDecimal
      treasuryFeeNum = (fromIntegral treasuryFee) :: BigDecimal
      lpFeeNum = (fromIntegral lpFee) :: BigDecimal
      lqSupplyDouble = (fromIntegral lqSupply) :: BigDecimal
      xToDepositDouble = xToDeposit
      additionalPart = xToDepositDouble --     xInInvariant = fromIntegral $ baseAssetBalance + round ((fromIntegral xToDeposit) * ((lpFeeNum - treasuryFeeNum) / fromIntegral feeDen))
      -- no decimals after point
      xInInvariantBigDecimal = xValueFloat + additionalPart
      -- xInInvariantBigDecimal in degree `(xWieght / 10)`
      xInInvariantWithDegree = (xInInvariantBigDecimal ** (fromRational $ ((fromIntegral baseAssetWeight) / 10))) -- g
      xInInvariantWith1Degree = (xInInvariantBigDecimal) ** (fromRational $ (1 / 10)) -- t

      gX = ((takeNBigDecimal xInInvariantWithDegree (maxPrecision)) :: Integer)
      tX = ((takeNBigDecimal xInInvariantWith1Degree (maxPrecision)) :: Integer)

      test = xInInvariantWith1Degree ** (fromIntegral baseAssetWeight)
    
  traceM $ T.pack $ "prevInvariant:" ++ show prevInvariant
  traceM $ T.pack $ "gX:" ++ show gX
  traceM $ T.pack $ "tX:" ++ show tX
  traceM $ T.pack $ "testa:" ++ show xInInvariantWithDegree
  traceM $ T.pack $ "testb:" ++ show test
  traceM $ T.pack $ "x token qty:" ++ show baseAssetBalance
  traceM $ T.pack $ "y token qty:" ++ show quoteAssetBalance

  traceM $ T.pack $ "lqIssued:" ++ show (getDecimalNum lqIssuedDec)
  traceM $ T.pack $ "xToDeposit:" ++ show (getDecimalNum xToDeposit)
  traceM $ T.pack $ "yToDeposit:" ++ show (getDecimalNum yToDeposit)

  let
      gBase = yValueFloat + yToDeposit
      gBaseRounded = BigDecimal (getDecimalNum (yValueFloat + yToDeposit)) 0
      gYDouble = nthRoot (gBaseRounded ** yWeightFloat) 10 (UP, (Just . toInteger $ maxPrecision)) :: BigDecimal -- g

  traceM $ T.pack $ "gYDouble:" ++ show gYDouble
  traceM $ T.pack $ "gBase:" ++ show gBase
  traceM $ T.pack $ "(fromIntegral (getDecimalNum (yValueFloat + yToDeposit))):" ++ show ((fromIntegral (getDecimalNum (yValueFloat + yToDeposit))))
  let    
      tGDoubleTest = nthRoot gBaseRounded 10 (DOWN, (Just . toInteger $ maxPrecision))
      tGDouble     = nthRoot gBaseRounded 10 (DOWN, (Just . toInteger $ maxPrecision))

      tGTest = tGDouble ** (yWeightFloat)
        -- BigDecimal (round ((fromIntegral (getDecimalNum (yValueFloat + yToDeposit))) ** (1 / 10))) 0 -- nthRoot gBase 10 (DOWN, (Just . toInteger $ 10)) :: BigDecimal -- g

  traceM $ T.pack $ "tGDoubleTest:" ++ show tGTest

  let
      gY = takeNBigDecimal gYDouble (maxPrecision) -- ((getValue gYDouble))
      tY = takeNBigDecimal tGDouble (maxPrecision) -- ((getValue tGDouble))

  traceM $ T.pack $ "gY:" ++ show gY
  traceM $ T.pack $ "tY:" ++ show tY
  traceM $ T.pack $ "lqIssued:" ++ show (getDecimalNum lqIssuedDec)

  pure (gX, tX, gY, tY, getDecimalNum xToDeposit, getDecimalNum yToDeposit, getDecimalNum lqIssuedDec)

calculateGandTDepositSingle :: MonadGen m => Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> m (Integer, Integer, Integer, Integer, Integer, Integer, Integer, Integer, Integer, Integer, Integer, Integer, Integer, Integer)
calculateGandTDepositSingle baseAssetBalance baseAssetWeight quoteAssetBalance quoteAssetWeghit baseIn lqSupply lpFee treasuryFee prevInvariant = do
  let
      yPartLength = toInteger $ RIO.length . show $ quoteAssetBalance
      xValueLength = toInteger $ RIO.length . show $ (baseAssetBalance + baseIn)

      maxPrecisionByToken = (if (yPartLength > xValueLength) then yPartLength else xValueLength)

      maxPrecision = (if (yPartLength > xValueLength) then yPartLength else xValueLength) + 10

      xValueFloat = (fromIntegral baseAssetBalance) :: BigDecimal
      invariantFloat = fromIntegral prevInvariant :: BigDecimal
      xWeightFloat = (fromIntegral baseAssetWeight) :: BigDecimal
      yValueFloat = (fromIntegral quoteAssetBalance) :: BigDecimal
      yWeightFloat = (fromIntegral quoteAssetWeghit) :: BigDecimal
      treasuryFeeNum = (fromIntegral treasuryFee) :: BigDecimal
      lpFeeNum = (fromIntegral lpFee) :: BigDecimal
      lqSupplyDouble = (fromIntegral lqSupply) :: BigDecimal
      baseInDouble = (BigDecimal (fromIntegral baseIn) 0) :: BigDecimal

      lqIssued = roundBD ((baseInDouble * lqSupplyDouble) / xValueFloat) (DOWN, (Just . toInteger $ 0))

      -- first gx, tx, gy, ty (Non-ideal)

      additionalPart = baseInDouble --     xInInvariant = fromIntegral $ baseAssetBalance + round ((fromIntegral baseIn) * ((lpFeeNum - treasuryFeeNum) / fromIntegral feeDen))
      -- no decimals after point
      xInInvariantBigDecimal = xValueFloat + additionalPart
      -- xInInvariantBigDecimal in degree `(xWieght / 10)`
      xInInvariantWithDegree = (xInInvariantBigDecimal ** (fromRational $ ((fromIntegral baseAssetWeight) / 10))) -- g
      xInInvariantWith1Degree = (xInInvariantBigDecimal) ** (fromRational $ (1 / 10)) -- t

      gXNonIdeal = ((takeNBigDecimal xInInvariantWithDegree (maxPrecision)) :: Integer)
      tXNonIdeal = ((takeNBigDecimal xInInvariantWith1Degree (maxPrecision)) :: Integer)

      test = xInInvariantWith1Degree ** (fromIntegral baseAssetWeight)
    
  traceM $ T.pack $ "prevInvariant:" ++ show prevInvariant
  traceM $ T.pack $ "gXNonIdeal:" ++ show gXNonIdeal
  traceM $ T.pack $ "tXNonIdeal:" ++ show tXNonIdeal
  traceM $ T.pack $ "testa:" ++ show xInInvariantWithDegree
  traceM $ T.pack $ "testb:" ++ show test

  let
      gBase = yValueFloat
      gBaseRounded = BigDecimal (getDecimalNum (yValueFloat)) 0
      gYDouble = nthRoot (gBaseRounded ** yWeightFloat) 10 (UP, (Just . toInteger $ maxPrecision)) :: BigDecimal -- g

  traceM $ T.pack $ "gYDouble:" ++ show gYDouble
  traceM $ T.pack $ "gBase:" ++ show gBase
  traceM $ T.pack $ "(fromIntegral (getDecimalNum (yValueFloat + yToDeposit))):" ++ show ((fromIntegral (getDecimalNum (yValueFloat))))
  let    
      tGDoubleTest = nthRoot gBaseRounded 10 (DOWN, (Just . toInteger $ maxPrecision))
      tGDouble     = nthRoot gBaseRounded 10 (DOWN, (Just . toInteger $ maxPrecision))

      tGTest = tGDouble ** (yWeightFloat)
        -- BigDecimal (round ((fromIntegral (getDecimalNum (yValueFloat + yToDeposit))) ** (1 / 10))) 0 -- nthRoot gBase 10 (DOWN, (Just . toInteger $ 10)) :: BigDecimal -- g

  traceM $ T.pack $ "gYNonIdeal:" ++ show gYDouble
  traceM $ T.pack $ "tYNonIdeal:" ++ show tGDouble
  traceM $ T.pack $ "tGDoubleTest:" ++ show tGTest

  let
      gYNonIdeal = takeNBigDecimal gYDouble (maxPrecision) -- ((getValue gYDouble))
      tYNonIdeal = takeNBigDecimal tGDouble (maxPrecision) -- ((getValue tGDouble))

  -- non ideal end

  -- ideal

      nonIdealIvariant = xInInvariantWithDegree * gYDouble

  traceM $ T.pack $ "nonIdealIvariant ideal:" ++ show nonIdealIvariant

  let
      xIdealDelta = xValueFloat * (nonIdealIvariant / invariantFloat - (fromIntegral 1))
      yIdealDelta = yValueFloat * (nonIdealIvariant / invariantFloat - (fromIntegral 1))

  traceM $ T.pack $ "xIdealDelta ideal:" ++ show xIdealDelta  
  traceM $ T.pack $ "yIdealDelta ideal:" ++ show yIdealDelta


  let
      additionalPart = baseInDouble --     xInInvariant = fromIntegral $ baseAssetBalance + round ((fromIntegral baseIn) * ((lpFeeNum - treasuryFeeNum) / fromIntegral feeDen))
      -- no decimals after point
      xInInvariantBigDecimal = xValueFloat + xIdealDelta
      -- xInInvariantBigDecimal in degree `(xWieght / 10)`
      xInInvariantWithDegree = nthRoot (xInInvariantBigDecimal ** xWeightFloat) 10 (UP, (Just . toInteger $ maxPrecision)) :: BigDecimal
        --(xInInvariantBigDecimal ** (fromRational $ ((fromIntegral baseAssetWeight) / 10))) -- g
      xInInvariantWith1Degree = nthRoot (roundBD xInInvariantBigDecimal (UP, (Just . toInteger $ 10))) 10 (UP, (Just . toInteger $ maxPrecision)) :: BigDecimal
        --(xInInvariantBigDecimal) ** (fromRational $ (1 / 10)) -- t

      gXIdeal = ((takeNBigDecimal xInInvariantWithDegree (maxPrecision)) :: Integer)
      tXIdeal = ((takeNBigDecimal xInInvariantWith1Degree (maxPrecision)) :: Integer)

      test = (BigDecimal tXIdeal 0) ** (fromIntegral baseAssetWeight) :: BigDecimal
    
  traceM $ T.pack $ "prevInvariant ideal:" ++ show prevInvariant
  traceM $ T.pack $ "gXIdeal:" ++ show gXIdeal
  traceM $ T.pack $ "xInInvariantBigDecimal:" ++ show xInInvariantBigDecimal
  traceM $ T.pack $ "xInInvariantWith1Degree:" ++ show xInInvariantWith1Degree
  traceM $ T.pack $ "tXIdeal:" ++ show tXIdeal
  traceM $ T.pack $ "testa ideal:" ++ show xInInvariantWithDegree
  traceM $ T.pack $ "testb ideal:" ++ show test

  traceM $ T.pack $ "x token qty:" ++ show baseAssetBalance
  traceM $ T.pack $ "y token qty:" ++ show quoteAssetBalance

  traceM $ T.pack $ "lqIssued:" ++ show (getDecimalNum lqIssued)

  let
      gBase = yValueFloat + yIdealDelta
      gBaseRounded = BigDecimal (getDecimalNum (yValueFloat + yIdealDelta)) 0
      gYDouble = nthRoot (gBaseRounded ** yWeightFloat) 10 (UP, (Just . toInteger $ maxPrecision)) :: BigDecimal -- g

  let    
      tGDoubleTest = nthRoot gBaseRounded 10 (DOWN, (Just . toInteger $ maxPrecision))
      tGDouble     = nthRoot gBaseRounded 10 (DOWN, (Just . toInteger $ maxPrecision))

      tGTest = tGDouble ** (yWeightFloat)
        -- BigDecimal (round ((fromIntegral (getDecimalNum (yValueFloat + yToDeposit))) ** (1 / 10))) 0 -- nthRoot gBase 10 (DOWN, (Just . toInteger $ 10)) :: BigDecimal -- g

  traceM $ T.pack $ "gYIdeal:" ++ show gYDouble
  traceM $ T.pack $ "tYIdeal:" ++ show tGDouble
  traceM $ T.pack $ "tGDoubleTest ideal:" ++ show tGTest

  let
      gYIdeal = takeNBigDecimal gYDouble (maxPrecision) -- ((getValue gYDouble))
      tYIdeal = takeNBigDecimal tGDouble (maxPrecision) -- ((getValue tGDouble))

  -- ideal end

  -- real

      additionalPart = baseInDouble --     xInInvariant = fromIntegral $ baseAssetBalance + round ((fromIntegral baseIn) * ((lpFeeNum - treasuryFeeNum) / fromIntegral feeDen))
      -- no decimals after point
      xInInvariantBigDecimal = xValueFloat + additionalPart
      -- xInInvariantBigDecimal in degree `(xWieght / 10)`
      xInInvariantWithDegree = (xInInvariantBigDecimal ** (fromRational $ ((fromIntegral baseAssetWeight) / 10))) -- g
      xInInvariantWith1Degree = (xInInvariantBigDecimal) ** (fromRational $ (1 / 10)) -- t

      gX = ((takeNBigDecimal xInInvariantWithDegree (maxPrecision)) :: Integer)
      tX = ((takeNBigDecimal xInInvariantWith1Degree (maxPrecision)) :: Integer)

      test = xInInvariantWith1Degree ** (fromIntegral baseAssetWeight)
    
  traceM $ T.pack $ "prevInvariant:" ++ show prevInvariant
  traceM $ T.pack $ "gX:" ++ show gX
  traceM $ T.pack $ "tX:" ++ show tX
  traceM $ T.pack $ "testa:" ++ show xInInvariantWithDegree
  traceM $ T.pack $ "testb:" ++ show test

  let
      lqIssued = roundBD ((baseInDouble * lqSupplyDouble) / xValueFloat) (DOWN, (Just . toInteger $ 0))

      yToDeposit = roundBD ((lqIssued * yValueFloat) / lqSupplyDouble) (UP, (Just . toInteger $ 0))

      -- yToDeposit = (((lqSupplyDouble + lqIssued) / lqSupplyDouble) - 1) * yValueFloat

  traceM $ T.pack $ "x token qty:" ++ show baseAssetBalance
  traceM $ T.pack $ "y token qty:" ++ show quoteAssetBalance

  traceM $ T.pack $ "lqIssued:" ++ show (getDecimalNum lqIssued)
  traceM $ T.pack $ "yToDeposit:" ++ show (getDecimalNum yToDeposit)

  let
      gBase = yValueFloat + yToDeposit
      gBaseRounded = BigDecimal (getDecimalNum (yValueFloat + yToDeposit)) 0
      gYDouble = nthRoot (gBaseRounded ** yWeightFloat) 10 (UP, (Just . toInteger $ maxPrecision)) :: BigDecimal -- g

  traceM $ T.pack $ "gYDouble:" ++ show gYDouble
  traceM $ T.pack $ "gBase:" ++ show gBase
  traceM $ T.pack $ "(fromIntegral (getDecimalNum (yValueFloat + yToDeposit))):" ++ show ((fromIntegral (getDecimalNum (yValueFloat + yToDeposit))))
  let    
      tGDoubleTest = nthRoot gBaseRounded 10 (DOWN, (Just . toInteger $ maxPrecision))
      tGDouble     = nthRoot gBaseRounded 10 (DOWN, (Just . toInteger $ maxPrecision))

      tGTest = tGDouble ** (yWeightFloat)
        -- BigDecimal (round ((fromIntegral (getDecimalNum (yValueFloat + yToDeposit))) ** (1 / 10))) 0 -- nthRoot gBase 10 (DOWN, (Just . toInteger $ 10)) :: BigDecimal -- g

  traceM $ T.pack $ "tGDoubleTest:" ++ show tGTest

  let
      gY = takeNBigDecimal gYDouble (maxPrecision) -- ((getValue gYDouble))
      tY = takeNBigDecimal tGDouble (maxPrecision) -- ((getValue tGDouble))

  traceM $ T.pack $ "gY:" ++ show gY
  traceM $ T.pack $ "tY:" ++ show tY
  traceM $ T.pack $ "lqIssued:" ++ show (getDecimalNum lqIssued)

  -- real end

  traceM $ T.pack $ "gY:" ++ show gY
  traceM $ T.pack $ "tY:" ++ show tY
  traceM $ T.pack $ "lqIssued:" ++ show (getDecimalNum lqIssued)

  pure (gXNonIdeal, tXNonIdeal, gYNonIdeal, tYNonIdeal, gXIdeal, tXIdeal, gYIdeal, tYIdeal, gXIdeal, tXIdeal, gYIdeal, tYIdeal, getDecimalNum lqIssued, getDecimalNum xIdealDelta)

calculateGandTRedeem :: MonadGen m => Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> m (Integer, Integer, Integer, Integer, Integer, Integer)
calculateGandTRedeem baseAssetBalance baseAssetWeight quoteAssetBalance quoteAssetWeghit lqRedeemI lqSupply lpFee treasuryFee prevInvariant = do
  let
      yPartLength = toInteger $ RIO.length . show $ quoteAssetBalance
      --xValueLength = toInteger $ RIO.length . show $ (baseAssetBalance + xToDeposit)
      lqLength = toInteger $ RIO.length . show $ lqSupply

      maxPrecisionByToken = lqLength

      maxPrecision = lqLength

      xValueFloat = (fromIntegral baseAssetBalance) :: BigDecimal
      invariantFloat = fromIntegral prevInvariant :: BigDecimal
      xWeightFloat = (fromIntegral baseAssetWeight) :: BigDecimal
      yValueFloat = (fromIntegral quoteAssetBalance) :: BigDecimal
      yWeightFloat = (fromIntegral quoteAssetWeghit) :: BigDecimal
      treasuryFeeNum = (fromIntegral treasuryFee) :: BigDecimal
      lpFeeNum = (fromIntegral lpFee) :: BigDecimal
      lqSupplyDouble = (fromIntegral lqSupply) :: BigDecimal
      --xToDepositDouble = (BigDecimal (fromIntegral xToDeposit) 0) :: BigDecimal
      lqRedeemDouble = (BigDecimal (fromIntegral lqRedeemI) 0) :: BigDecimal

      xToRedeem = roundBD ((lqRedeemDouble * xValueFloat) / lqSupplyDouble) (UP, (Just . toInteger $ 0))
      yToRedeem = roundBD ((lqRedeemDouble * yValueFloat) / lqSupplyDouble) (UP, (Just . toInteger $ 0))

      --additionalPart = xToDepositDouble --     xInInvariant = fromIntegral $ baseAssetBalance + round ((fromIntegral xToDeposit) * ((lpFeeNum - treasuryFeeNum) / fromIntegral feeDen))
      -- no decimals after point
      xInInvariantBigDecimal = xValueFloat - xToRedeem
      -- xInInvariantBigDecimal in degree `(xWieght / 10)`
      xInInvariantWithDegree = (xInInvariantBigDecimal ** (fromRational $ ((fromIntegral baseAssetWeight) / 10))) -- g
      xInInvariantWith1Degree = (xInInvariantBigDecimal) ** (fromRational $ (1 / 10)) -- t

      gX = ((takeNBigDecimal xInInvariantWithDegree (maxPrecision)) :: Integer)
      tX = ((takeNBigDecimal xInInvariantWith1Degree (maxPrecision)) :: Integer)

      test = xInInvariantWith1Degree ** (fromIntegral baseAssetWeight)
    
  traceM $ T.pack $ "prevInvariant:" ++ show prevInvariant
  traceM $ T.pack $ "gX:" ++ show gX
  traceM $ T.pack $ "tX:" ++ show tX
  traceM $ T.pack $ "testa:" ++ show xInInvariantWithDegree
  traceM $ T.pack $ "testb:" ++ show test

      -- yToDeposit = (((lqSupplyDouble + lqIssued) / lqSupplyDouble) - 1) * yValueFloat

  traceM $ T.pack $ "x token qty:" ++ show baseAssetBalance
  traceM $ T.pack $ "y token qty:" ++ show quoteAssetBalance

  let
      gBase = yValueFloat - yToRedeem
      gBaseRounded = BigDecimal (getDecimalNum (yValueFloat - yToRedeem)) 0
      gYDouble = nthRoot (gBaseRounded ** yWeightFloat) 10 (UP, (Just . toInteger $ maxPrecision)) :: BigDecimal -- g

  traceM $ T.pack $ "gYDouble:" ++ show gYDouble
  traceM $ T.pack $ "gBase:" ++ show gBase
  let    
      tGDoubleTest = nthRoot gBaseRounded 10 (DOWN, (Just . toInteger $ maxPrecision))
      tGDouble     = nthRoot gBaseRounded 10 (DOWN, (Just . toInteger $ maxPrecision))

      tGTest = tGDouble ** (yWeightFloat)
        -- BigDecimal (round ((fromIntegral (getDecimalNum (yValueFloat + yToDeposit))) ** (1 / 10))) 0 -- nthRoot gBase 10 (DOWN, (Just . toInteger $ 10)) :: BigDecimal -- g

  traceM $ T.pack $ "tGDoubleTest:" ++ show tGTest

  let
      gY = takeNBigDecimal gYDouble (maxPrecision) -- ((getValue gYDouble))
      tY = takeNBigDecimal tGDouble (maxPrecision) -- ((getValue tGDouble))

  traceM $ T.pack $ "gY:" ++ show gY
  traceM $ T.pack $ "tY:" ++ show tY

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
      --  xToSwap = 1434368
      
      xToSwap <- integral (Range.constant 10000 10000000)
      
      (gX, tX, gY, tY, yToSwap) <- calculateGandTSwap xValue (weightX config) yValue (weightY config) (xToSwap) (poolFeeNum config) (treasuryFee config) (invariant config)
      let
        -- going to withdraw all pool x and y value
        treasuryFee_ = treasuryFee config
        newPoolConfig = config 
          { treasuryX = (treasuryFee_ * xToSwap) `div` feeDen
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
      (gX, tX, gY, tY, yToSwap) <- calculateGandTSwap xValue (weightX config) yValue (weightY config) (xToSwap + 1000) (poolFeeNum config) (treasuryFee config) (invariant config)
      let
        newPool = prevPool 
          { config = config
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
        newPool = prevPool 
          { config = config
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
        newPool = prevPool 
          { config = config
          , value  = value <> (assetClassValue (poolX config) (xToSwap)) <> (assetClassValue (poolY config) (negate incorrectYFinalValue))
          }

      pure $ BalancePoolActionResult newPool [] [gX, gY] [tX, tY] (toInteger 15)
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

      pure $ BalancePoolActionResult newPool [] [gX, gY] [tX, tY] (toInteger 15)
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

        newPool = prevPool 
          { config = config
          , value  = value <> (assetClassValue (poolX config) (xToDeposit)) <> (assetClassValue (poolY config) (yToDeposit)) <> (assetClassValue (poolLq config) (negate lqIssued))
          }

      pure $ BalancePoolActionResult newPool [] [gX, gY] [tX, tY] (toInteger 15)
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
        newPool = prevPool 
          { config = config
          , value  = value <> (assetClassValue (poolX config) (xToDeposit)) <> (assetClassValue (poolY config) (yToDeposit)) <> (assetClassValue (poolLq config) (negate (lqIssued + 1000)))
          }

      pure $ BalancePoolActionResult newPool [] [gX, gY] [tX, tY] (toInteger 15)
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

      lqToRedeem <- integral (Range.constant 1 ((lqIssued `div` 2) - 1))

      (gX, tX, gY, tY, xToRedeem, yToRedeem) <- calculateGandTRedeem xValue (weightX config) yValue (weightY config) (lqToRedeem) lqIssued (poolFeeNum config) (treasuryFee config) (invariant config)

      let
        -- lqIssued = 0x7fffffffffffffff - (round lqValue)

        newInvariant = gX * gY
        newPool = prevPool 
          { config = config
          , value  = value <> (assetClassValue (poolX config) (negate xToRedeem)) <> (assetClassValue (poolY config) (negate yToRedeem)) <> (assetClassValue (poolLq config) (lqToRedeem))
          }

      pure $ BalancePoolActionResult newPool [] [gX, gY] [tX, tY] (toInteger 15)
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
          { treasuryX = 0
          , treasuryY = 0
          , invariant = newInvariant
          }

        newPool = prevPool 
          { config = newPoolConfig
          , value  = value <> (assetClassValue (poolX config) (negate xToRedeem)) <> (assetClassValue (poolY config) (negate yToRedeem)) <> (assetClassValue (poolLq config) (lqToRedeem - 100))
          }

      pure $ BalancePoolActionResult newPool [] [gX, gY] [tX, tY] (toInteger 15)
  in BalancePoolTestAction "InCorrect redeem all tokens. Incorrect final lq value" testAction

-- Redeem all cases end --

-- Deposit single cases start --

correctDepositSingle :: MonadGen m => BalancePoolTestAction m
correctDepositSingle = 
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

      xToDeposit <- integral (Range.constant 1 ((xValue `div` 2) - 1))

      (gXNonIdeal, tXNonIdeal, gYNonIdeal, tYNonIdeal, gXIdeal, tXIdeal, gYIdeal, tYIdeal, gXReal, tXReal, gYReal, tYReal, lqToIssued, gxIdealDeposit) <- calculateGandTDepositSingle xValue (weightX config) yValue (weightY config) (xToDeposit) lqIssued (poolFeeNum config) (treasuryFee config) (invariant config)

      let
        -- lqIssued = 0x7fffffffffffffff - (round lqValue)

        newInvariant = gXReal * gYReal

        -- going to withdraw all pool x and y value
        newPoolConfig = config 
          { treasuryX = 0
          , treasuryY = 0
          , invariant = newInvariant
          }

        newPool = prevPool 
          { config = newPoolConfig
          , value  = value <> (assetClassValue (poolX config) (xToDeposit)) <> (assetClassValue (poolLq config) (negate (lqIssued)))
          }

      pure $ BalancePoolActionResult newPool [] [gXNonIdeal, gYNonIdeal, gXIdeal, gYIdeal, gXReal, gYReal, gxIdealDeposit] [tXNonIdeal, tYNonIdeal, tXIdeal, tYIdeal, tXReal, tYReal] (toInteger 15)
  in BalancePoolTestAction "Correct deposit single tokens" testAction

-- Deposit single cases end --