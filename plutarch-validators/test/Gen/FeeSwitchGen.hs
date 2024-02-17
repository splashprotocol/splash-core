{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGuaGE RankNTypes #-}

module Gen.FeeSwitchGen where

import Plutarch.Prelude
import Plutarch
import Plutarch.Api.V2.Contexts (PScriptContext)
import Plutarch.Prelude

import RIO hiding (Data(..))
import Hedgehog
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
import Gen.Utils

import WhalePoolsDex.Contracts.Pool

-- \|/ todo: to Common generators  \|/ --

mkDatum :: ToData a => a -> Datum
mkDatum = Datum . toBuiltinData

-- \|/ random script hash from cexplorer dapps, used as treasury address \|/ --

randomHashValue :: String
randomHashValue = "9068a7a3f008803edac87af1619860f2cdcde40c26987325ace138ad"

-- /|\ random script hash from cexplorer dapps, used as treasury address /|\ --

poolStakeChangeMintTnValue :: String
poolStakeChangeMintTnValue = "746e"

poolStakeChangeMintTokenName:: Plutus.TokenName
poolStakeChangeMintTokenName = Plutus.TokenName $ BuiltinByteString . mkByteString $ T.pack poolStakeChangeMintTnValue

-- /|\ todo: to Common generators  /|\ --

data Treasury = Treasury 
  { xTokenAC     :: AssetClass
  , xTokenQty    :: Integer
  , yTokenAC     :: AssetClass
  , yTokenQty    :: Integer
  , treasuryAddr :: Address
  } deriving Show

instance ToTxOut Treasury where
  toTxOut Treasury{..} = TxOut
    { txOutAddress  = treasuryAddr
    , txOutValue    = mkValues ((\(ac, qty) -> mkValue ac (fromIntegral qty)) `RIO.map` [(xTokenAC, xTokenQty), (yTokenAC, yTokenQty)]) mempty
    , txOutDatum    = NoOutputDatum
    , txOutReferenceScript = Nothing
    }

instance ToTxInfo Pool where
  toTxInInfo pool = do
    ref <- genTxOutRef
    let txOut = toTxOut pool
    pure $ TxInInfo
      { txInInfoOutRef   = ref
      , txInInfoResolved = txOut
      }

treasuryFeeNumLowerLimit = 1
treasuryFeeNumUpperLimit = 20000

poolFeeNumLowerLimit = 30000
poolFeeNumUpperLimit = 99999

treasuryFeeDen = 100000
poolFeeNumDen  = 100000

genPool :: MonadGen f => [PubKeyHash] -> Integer -> Bool -> f Pool
genPool adminsPkhs threshold lpFeeIsEditable = do
  (x, y, lq, nft) <- tuple4 genAssetClass

  stakeHash <- genPkh
  
  xQty <- int (Range.constant 1000 1000000)
  yQty <- int (Range.constant 1000 1000000)

  poolFee <- integral (Range.constant 0 1000)

  startXTreasuty <- integral (Range.constant 100 500)
  startYTreasuty <- integral (Range.constant 100 500)

  treasuryAddress <- genValidatorHash
  let
    nftQty = 1
    lqQty  = 0x7fffffffffffffff - (round . sqrt . fromIntegral $ xQty * yQty)

  let
    daoContract =
        StakingHash . ScriptCredential . ValidatorHash . getScriptHash . scriptHash $ (unMintingPolicyScript (daoMintPolicyValidator nft adminsPkhs threshold lpFeeIsEditable))

    poolConfig = PoolConfig
      { poolNft = nft
      , poolX   = x
      , poolY   = y
      , poolLq  = lq
      , poolFeeNum  = poolFee
      , treasuryFee = 2
      , treasuryX  = startXTreasuty
      , treasuryY  = startYTreasuty
      , daoPolicy  = [daoContract]
      , lqBound    = 0  -- todo: remove?
      , treasuryAddress = treasuryAddress
      }

    poolValue = mkValues ((\(ac, qty) -> mkValue ac (fromIntegral qty)) `RIO.map` [(x, xQty), (y, yQty), (nft, nftQty), (lq, lqQty)]) mempty

  pure $ Pool poolConfig stakeHash poolValue

daoValidator :: Pool -> [PubKeyHash] -> Integer -> Bool -> ClosedTerm (PData :--> PScriptContext :--> POpaque)
daoValidator Pool{..} admins threshold lpFeeIsEditable = 
  wrapMintingValidator (daoMultisigPolicyValidatorT (pconstant (poolNft config)) (pconstant admins) (pconstant threshold) (pconstant lpFeeIsEditable))

incorrectPoolValue :: Pool -> Pool
incorrectPoolValue prevPool =
  prevPool {
    value = mkAdaValue 300000
  }

changePoolTokensInfo :: MonadGen m => TestAction m
changePoolTokensInfo =
  let
    action prevPool@Pool{..} = do
      (newX, newY, newLq, newNft) <- tuple4 genAssetClass
      newPoolFee <- integral (Range.constant 0 1000)
      newLqBound <- integral (Range.constant 0 1000)
      let
        newConfig = config 
          { poolNft = newNft
          , poolX   = newX
          , poolY   = newY
          , poolLq  = newLq
          , poolFeeNum  = newPoolFee
          , lqBound = newLqBound
          }
      pure $ ActionResult prevPool {
        config = newConfig
      } []
  in TestAction "Change basic fields in pool datum" action

changeTreasuryAddress :: MonadGen m => TestAction m
changeTreasuryAddress =
  let
    action prevPool@Pool{..} = do
      newTreasuryAddress <- genValidatorHash
      let
        newConfig = config 
          { treasuryAddress = newTreasuryAddress
          }
      pure $ ActionResult prevPool {
        config = newConfig
      } []
  in TestAction "Change treasury address" action

changeStakePartOfAddress :: MonadGen m => TestAction m
changeStakePartOfAddress =
  let
    action prevPool@Pool{..} = do
      newStakeHash <- genPkh
      pure $ ActionResult prevPool {
        stakeAddress = newStakeHash
      } []
  in TestAction "Change stake part of address" action

changeTreasuryFee :: MonadGen m => TestAction m -- Pool -> m Pool
changeTreasuryFee = 
  let
    action prevPool@Pool{..} = do
      newCorrectTreasuryFee <- integral (Range.constant treasuryFeeNumLowerLimit treasuryFeeNumUpperLimit)
      let
        newConfig = config {
          treasuryFee = newCorrectTreasuryFee
        }
      pure $ ActionResult prevPool {
        config = newConfig
      } []
  in TestAction "Change treasury fee" action

incorrectTreasuryFee :: MonadGen m => TestAction m -- Pool -> m Pool
incorrectTreasuryFee = 
  let
    action prevPool@Pool{..} = do
      newIncorrectTreasuryFee <- integral (Range.constant (treasuryFeeNumUpperLimit + 1) treasuryFeeDen)
      let
        newConfig = config {
          treasuryFee = newIncorrectTreasuryFee
        }
      pure $ ActionResult prevPool {
        config = newConfig
      } []
  in TestAction "Incorrect treasury fee" action

changePoolConfigTokenToNew :: MonadGen m => (AssetClass -> Pool -> Pool) -> Pool -> m Pool
changePoolConfigTokenToNew tokenUpdater prevPool = do
  randomToken <- genAssetClass
  pure $ tokenUpdater randomToken prevPool

-- will set treasuryX/treasuryY to max values
changePoolTreasury :: forall m. Applicative m => TestAction m --Pool -> m Pool
changePoolTreasury =
  let 
    action prevPool@Pool{..} =
      let
        (xCS, xTN) = unAssetClass (poolX config)
        (yCS, yTN) = unAssetClass (poolY config)
        xValue = valueOf value xCS xTN
        yValue = valueOf value yCS yTN

        newIncorrectConfig = config 
          { treasuryX = xValue
          , treasuryY = yValue
          }
      in pure $ ActionResult prevPool
        { config = newIncorrectConfig
        } []

  in TestAction "Change pool treasury" action

-- return new pool and treasury data
withdrawTreasury :: MonadGen m => TestAction m
withdrawTreasury = 
  let
    testAction prevPool@Pool{..} = do
      toWithdrawX <- integral (Range.constant 1 (treasuryX config))
      toWithdrawY <- integral (Range.constant 1 (treasuryY config))
      let
        newPoolConfig = config 
          { treasuryX = (treasuryX config) - toWithdrawX
          , treasuryY = (treasuryY config) - toWithdrawY
          }

        newPool = prevPool 
          { config = newPoolConfig
          , value  = value <> (assetClassValue (poolX config) (negate toWithdrawX)) <> (assetClassValue (poolY config) (negate toWithdrawY))
          }

        treasury = Treasury 
          { xTokenAC     = poolX config
          , xTokenQty    = toWithdrawX
          , yTokenAC     = poolY config
          , yTokenQty    = toWithdrawY
          , treasuryAddr = scriptHashAddress (treasuryAddress config)
          }
      pure $ ActionResult newPool [toTxOut treasury]
  in TestAction "Correct treasury withdraw" testAction

changePoolFee :: MonadGen m => TestAction m
changePoolFee = 
  let
    action prevPool@Pool{..} = do
      newPoolFee <- integral (Range.constant poolFeeNumLowerLimit poolFeeNumUpperLimit)
      let
        newConfig = config {
          poolFeeNum = newPoolFee
        }
      pure $ ActionResult prevPool {
        config = newConfig
      } []
  in TestAction "Change pool fee" action

incorrectChangePoolFee :: MonadGen m => TestAction m
incorrectChangePoolFee = 
  let
    action prevPool@Pool{..} = do
      incorrectPoolFee <- integral (Range.constant (poolFeeNumUpperLimit + 1) poolFeeNumDen)
      let
        newConfig = config {
          poolFeeNum = incorrectPoolFee
        }
      pure $ ActionResult prevPool {
        config = newConfig
      } []
  in TestAction "Incorrect pool fee" action

incorrectWithdrawValueTreasury :: MonadGen m => TestAction m
incorrectWithdrawValueTreasury = 
  let
    testAction prevPool@Pool{..} = do
      toWithdraw <- integral (Range.constant 1 (treasuryX config))
      let
        (xCS, xTN) = unAssetClass (poolX config)
        (yCS, yTN) = unAssetClass (poolY config)
        xValue = valueOf value xCS xTN
        yValue = valueOf value yCS yTN

        -- going to withdraw all pool x and y value
        newPoolConfig = config 
          { treasuryX = xValue
          , treasuryY = yValue
          }

        newPool = prevPool 
          { config = newPoolConfig
          , value  = value <> (assetClassValue (poolX config) (negate toWithdraw)) <> (assetClassValue (poolY config) (negate toWithdraw))
          }

        treasury = Treasury 
          { xTokenAC     = poolX config
          , xTokenQty    = toWithdraw
          , yTokenAC     = poolY config
          , yTokenQty    = toWithdraw
          , treasuryAddr = scriptHashAddress (treasuryAddress config)
          }
      pure $ ActionResult newPool [toTxOut treasury]
  in TestAction "Incorrect treasury value withdraw" testAction

incorrectWithdrawAddressTreasury :: MonadGen m => TestAction m
incorrectWithdrawAddressTreasury = 
  let
    testAction prevPool@Pool{..} = do
      toWithdraw       <- integral (Range.constant 1 (treasuryX config))
      incorrectAddress <- genValidatorHash
      let
        newPoolConfig = config 
          { treasuryX = (treasuryX config) - toWithdraw
          , treasuryY = (treasuryY config) - toWithdraw
          }

        newPool = prevPool 
          { config = newPoolConfig
          , value  = value <> (assetClassValue (poolX config) (negate toWithdraw)) <> (assetClassValue (poolY config) (negate toWithdraw))
          }

        treasury = Treasury 
          { xTokenAC     = poolX config
          , xTokenQty    = toWithdraw
          , yTokenAC     = poolY config
          , yTokenQty    = toWithdraw
          , treasuryAddr = scriptHashAddress incorrectAddress
          }
      pure $ ActionResult newPool [toTxOut treasury]
  in TestAction "Incorrect treasury address withdraw" testAction

treasuryFeeSwitch :: MonadGen m => TestAction m -- Pool -> m Pool
treasuryFeeSwitch =
  let
    action prevPool@Pool{..} = do
      let currentPoolFee = 1000 - (poolFeeNum config) --todo: 1000 to constant
      newTreasuryFee <- integral (Range.constant 1 currentPoolFee)
      let
        newPoolConfig = config {
          treasuryFee = newTreasuryFee
        }

        newPool = prevPool {
          config = newPoolConfig
        }
      pure $ ActionResult newPool []
  
  in TestAction "Switch treasury fee" action

treasuryAddressSwitch :: MonadGen m => TestAction m -- Pool -> m Pool
treasuryAddressSwitch  = let
  action prevPool@Pool{..} = do
    newValidatorHash <- genValidatorHash
    let
      newPoolConfig = config {
        treasuryAddress = newValidatorHash
      }

      newPool = prevPool {
        config = newPoolConfig
      }
    
    pure $ ActionResult newPool []

  in TestAction "Change treasury address" action

changeDAOAdminAddress :: forall m. MonadGen m => TestAction m
changeDAOAdminAddress = let
  testAction prevPool@Pool{..} = do
    randomDAOContract <- genSCRandom
    let
      newPoolConfig = config {
        daoPolicy = [randomDAOContract]
      }

      newPool = prevPool {
        config = newPoolConfig
      }
    pure $ ActionResult newPool []

  in TestAction 
      { name = "Change dao address" 
      , action = testAction
      }

changePoolValue :: forall m. MonadGen m => TestAction m
changePoolValue = let
  testAction prevPool@Pool{..} = do
    let
      newPool = prevPool {
        value = mkAdaValue 300000
      }
    pure $ ActionResult newPool []

  in TestAction 
      { name = "Change pool value" 
      , action = testAction
      }

createTxInfo :: MonadGen m => Pool -> ActionResult -> [PubKeyHash] -> m TxInfo
createTxInfo prevPool@Pool{..} ActionResult{..} adminPkhs = do
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

createTxInfoCustom :: MonadGen m => Pool -> [TxOut] -> [TxOut] -> [TxOut] -> [PubKeyHash] -> m TxInfo
createTxInfoCustom prevPool@Pool{..} maliciousInputs validOutputs maliciousOutputs adminPkhs = do
  poolTxIn  <- toTxInInfo prevPool
  malicious <- toTxInInfo `RIO.traverse` maliciousInputs
  let
    daoCS = List.head (daoPolicy config)

  pure $ TxInfo
    { txInfoInputs = [poolTxIn] ++ malicious
    , txInfoReferenceInputs = []
    , txInfoOutputs = validOutputs ++ maliciousOutputs
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

daoMintingPurpose :: Pool -> ScriptPurpose
daoMintingPurpose Pool{..} = Rewarding $ List.head (daoPolicy config)
