{-# LANGUAGE OverloadedStrings #-}

module Gen.Models
  ( genTokenName
  , genTxId
  , genTxOutRef
  , genCSRandom
  , genSCRandom
  , random16bs
  , random28bs
  , random32bs
  , mkAdaAssetClass
  , genAssetClass
  , genValidatorHash
  , mkAssetClass
  , mkValue
  , mkAdaValue
  , mkValues
  , mkPoolConfig
  , mkPoolBFeeConfig
  , mkDepositConfig
  , mkPoolRedeemer
  , mkDepositRedeemer
  , mkOrderRedeemer
  , mkRedeemer
  , mkDatum
  , mkDatumHash
  , mkMaxLq
  , mkTxInType
  , mkScriptCredential
  , genPkh
  , mkDepositValidator
  , mkSwapValidator
  , mkPoolValidator
  , mkTxOut
  , mkUserTxOut
  , mkUserTxOutWithDatum
  , mkTxOutWithSC
  , mkTxOut'
  , mkTxIn
  , mkTxInfo
  , mkTxInfoWithSignatures
  , mkTxInfoWithSignaturesAndMinting
  , mkTxInfoOnlyWithSignatures
  , mkPoolTxInfo
  , mkPurpose
  , mkRewardingPurpose
  , mkDelegatingPurpose
  , mkContext
  ) where

import RIO

import Hedgehog
import Hedgehog.Gen   as Gen
import Hedgehog.Range as Range

import qualified Data.ByteString as BS

import PlutusTx.Builtins.Internal
import PlutusLedgerApi.V1.Value 
import qualified PlutusLedgerApi.V1.Value as Value
import PlutusLedgerApi.V2.Tx
import PlutusLedgerApi.V2
import PlutusLedgerApi.V1.Credential
import qualified PlutusLedgerApi.V1.Interval as Interval
import Plutarch.Api.V2 ( validatorHash, datumHash)

import qualified WhalePoolsDex.PValidators             as PScripts
import qualified WhalePoolsDex.Contracts.Pool          as P
import qualified WhalePoolsDex.Contracts.PoolBFee      as PBFee
import qualified WhalePoolsDex.Contracts.Proxy.Deposit as D
import qualified WhalePoolsDex.Contracts.Proxy.Order   as O
import PlutusTx.Builtins as Builtins

genBuiltinByteString :: MonadGen f => Int -> f BuiltinByteString
genBuiltinByteString s = BuiltinByteString <$> Gen.bytes (Range.singleton s)

random32bs :: MonadGen f => f BuiltinByteString
random32bs = genBuiltinByteString 32

random28bs :: MonadGen f => f BuiltinByteString
random28bs = genBuiltinByteString 28

random16bs :: MonadGen f => f BuiltinByteString
random16bs = genBuiltinByteString 16

genTxId :: MonadGen f => f TxId
genTxId = prune $ (genBuiltinByteString idSize) <&> TxId

genTxOutRef :: MonadGen f => f TxOutRef
genTxOutRef = do
  txId <- genTxId
  ix   <- integral $ Range.constant 0 10
  pure $ TxOutRef txId ix

genTokenName :: MonadGen f => f TokenName
genTokenName = do
  bs <- random32bs
  return $ TokenName bs

mkAssetClass :: CurrencySymbol -> TokenName -> AssetClass
mkAssetClass cs tn = AssetClass (cs, tn)

pkhSize = 28
validatorHashSize = 28
csSize = 28
tnSize = 16
idSize = 32

genTNRandom :: MonadGen f => f TokenName
genTNRandom = (genBuiltinByteString tnSize) <&> TokenName

genCSRandom :: MonadGen f => f CurrencySymbol
genCSRandom = (genBuiltinByteString csSize) <&> CurrencySymbol

genSCRandom :: MonadGen f => f StakingCredential
genSCRandom = (genBuiltinByteString validatorHashSize) <&> (\hash -> StakingHash $ ScriptCredential $ ValidatorHash $ hash)

genAssetClass :: MonadGen f => f AssetClass
genAssetClass = do
  cs <- genCSRandom
  tn <- genTNRandom
  pure $ AssetClass (cs, tn)

genValidatorHash :: MonadGen f => f ValidatorHash
genValidatorHash = (genBuiltinByteString pkhSize) <&> ValidatorHash

mkAdaAssetClass :: AssetClass
mkAdaAssetClass = mkAssetClass adaSymbol adaToken

mkValue :: AssetClass -> Integer -> Value
mkValue (AssetClass (cs, tn)) qty = Value.singleton cs tn qty

mkAdaValue :: Integer -> Value
mkAdaValue qty = mkValue mkAdaAssetClass qty

mkValues :: [Value] -> Value -> Value
mkValues (x:xs) acc = mkValues xs (x <> acc)
mkValues [] acc = acc

-- fee will be used as feeX and feeY
mkPoolConfig :: AssetClass -> AssetClass -> AssetClass -> AssetClass -> Integer -> Integer -> Integer -> Integer -> [StakingCredential] -> Integer -> ValidatorHash -> P.PoolConfig
mkPoolConfig nft x y lq fee treausuryFee treasuryX treasuryY daoPolicy lqBound treasuryAddress = 
  P.PoolConfig nft x y lq fee treausuryFee treasuryX treasuryY daoPolicy lqBound treasuryAddress

mkPoolBFeeConfig :: AssetClass -> AssetClass -> AssetClass -> AssetClass -> Integer -> Integer -> Integer -> Integer -> Integer -> [StakingCredential] -> Integer -> ValidatorHash -> PBFee.PoolConfig
mkPoolBFeeConfig nft x y lq feeX feeY treausuryFee treasuryX treasuryY daoPolicy lqBound treasuryAddress = 
  PBFee.PoolConfig nft x y lq feeX feeY treausuryFee treasuryX treasuryY daoPolicy lqBound treasuryAddress

mkDepositConfig :: AssetClass -> AssetClass -> AssetClass -> AssetClass -> Integer -> PubKeyHash -> Integer -> D.DepositConfig
mkDepositConfig nft x y lq fee pkh cFee = D.DepositConfig nft x y lq fee pkh Nothing cFee

mkPoolRedeemer :: Integer -> P.PoolAction -> P.PoolRedeemer
mkPoolRedeemer ix action = P.PoolRedeemer action ix

mkDepositRedeemer :: Integer -> Integer -> Integer -> O.OrderRedeemer
mkDepositRedeemer a b c = O.OrderRedeemer a b c O.Apply

mkOrderRedeemer :: Integer -> Integer -> Integer -> O.OrderRedeemer
mkOrderRedeemer a b c = O.OrderRedeemer a b c O.Apply

mkRedeemer :: ToData a => a -> Redeemer
mkRedeemer = Redeemer . toBuiltinData

mkDatum :: ToData a => a -> Datum
mkDatum = Datum . toBuiltinData

mkDatumHash :: Datum -> DatumHash
mkDatumHash = datumHash

mkMaxLq :: Integer
mkMaxLq = 0x7fffffffffffffff

mkTxInType :: Datum -> Redeemer -> TxInType
mkTxInType datum redeemer = ConsumeScriptAddress PScripts.poolValidator redeemer datum 

mkScriptCredential :: Credential
mkScriptCredential = ScriptCredential $ validatorHash PScripts.poolValidator

genPkh :: MonadGen f => f PubKeyHash
genPkh = genBuiltinByteString 28 <&> PubKeyHash

mkDepositValidator :: ValidatorHash
mkDepositValidator = validatorHash PScripts.depositValidator

mkPoolValidator :: ValidatorHash
mkPoolValidator = validatorHash PScripts.poolValidator

mkSwapValidator :: ValidatorHash
mkSwapValidator = validatorHash PScripts.swapValidator

mkTxOut :: OutputDatum -> Value -> ValidatorHash -> TxOut
mkTxOut od v vh =
  TxOut
    { txOutAddress = Address (ScriptCredential vh) Nothing
    , txOutValue   = v
    , txOutDatum   = od
    , txOutReferenceScript = Nothing
    }

mkUserTxOut :: Value -> PubKeyHash -> TxOut
mkUserTxOut v pkh =
  TxOut
    { txOutAddress = Address (PubKeyCredential pkh) Nothing
    , txOutValue   = v
    , txOutDatum   = NoOutputDatum
    , txOutReferenceScript = Nothing
    }

mkUserTxOutWithDatum :: OutputDatum -> Value -> PubKeyHash -> TxOut
mkUserTxOutWithDatum od v pkh =
  TxOut
    { txOutAddress = Address (PubKeyCredential pkh) Nothing
    , txOutValue   = v
    , txOutDatum   = od
    , txOutReferenceScript = Nothing
    }

mkTxOutWithSC :: OutputDatum -> Value -> ValidatorHash -> Maybe StakingCredential -> TxOut
mkTxOutWithSC od v vh sc =
  TxOut
    { txOutAddress = Address (ScriptCredential vh) sc
    , txOutValue   = v
    , txOutDatum   = od
    , txOutReferenceScript = Nothing
    }

mkTxOut' :: OutputDatum -> Value -> PubKeyHash -> TxOut
mkTxOut' od v pkh =
  TxOut
    { txOutAddress  = Address (PubKeyCredential pkh) Nothing
    , txOutValue    = v
    , txOutDatum    = od
    , txOutReferenceScript = Nothing
    }

mkTxIn :: TxOutRef -> TxOut -> TxInInfo
mkTxIn ref out =
  TxInInfo
    { txInInfoOutRef   = ref
    , txInInfoResolved = out
    }

mkPoolTxInfo :: TxInInfo -> TxOut -> TxInfo
mkPoolTxInfo pIn pOut =
  TxInfo
    { txInfoInputs = [pIn]
    , txInfoReferenceInputs = []
    , txInfoOutputs = [pOut]
    , txInfoFee = mempty
    , txInfoMint = mempty
    , txInfoDCert = []
    , txInfoWdrl = fromList []
    , txInfoValidRange = Interval.always
    , txInfoSignatories = mempty
    , txInfoRedeemers = fromList []
    , txInfoData = fromList []
    , txInfoId = "b0"
    }

mkTxInfo :: TxInInfo -> TxInInfo -> TxOut -> TxOut -> TxInfo
mkTxInfo pIn oIn pOut oOut =
  TxInfo
    { txInfoInputs = [pIn, oIn]
    , txInfoReferenceInputs = []
    , txInfoOutputs = [pOut, oOut]
    , txInfoFee = mempty
    , txInfoMint = mempty
    , txInfoDCert = []
    , txInfoWdrl = fromList []
    , txInfoValidRange = Interval.always
    , txInfoSignatories = mempty
    , txInfoRedeemers = fromList []
    , txInfoData = fromList []
    , txInfoId = "b0"
    }

mkTxInfoWithSignatures :: [TxInInfo] -> [TxOut] -> [PubKeyHash] -> TxInfo
mkTxInfoWithSignatures pIns pOuts sigs =
  TxInfo
    { txInfoInputs = pIns
    , txInfoReferenceInputs = []
    , txInfoOutputs = pOuts
    , txInfoFee = mempty
    , txInfoMint = mempty
    , txInfoDCert = []
    , txInfoWdrl = fromList []
    , txInfoValidRange = Interval.always
    , txInfoSignatories = sigs
    , txInfoRedeemers = fromList []
    , txInfoData = fromList []
    , txInfoId = "b0"
    }

mkTxInfoWithSignaturesAndMinting :: [TxInInfo] -> TxOut -> [PubKeyHash] -> StakingCredential -> TxInfo
mkTxInfoWithSignaturesAndMinting pIn pOut sigs sc =
  TxInfo
    { txInfoInputs = pIn
    , txInfoReferenceInputs = []
    , txInfoOutputs = [pOut]
    , txInfoFee = mempty
    , txInfoMint = mempty
    , txInfoDCert = []
    , txInfoWdrl = fromList [(sc, 0)]
    , txInfoValidRange = Interval.always
    , txInfoSignatories = sigs
    , txInfoRedeemers = fromList []
    , txInfoData = fromList []
    , txInfoId = "b0"
    }

mkTxInfoOnlyWithSignatures :: [PubKeyHash] -> TxInfo
mkTxInfoOnlyWithSignatures sigs =
  TxInfo
    { txInfoInputs = []
    , txInfoReferenceInputs = []
    , txInfoOutputs = []
    , txInfoFee = mempty
    , txInfoMint = mempty
    , txInfoDCert = []
    , txInfoWdrl = fromList []
    , txInfoValidRange = Interval.always
    , txInfoSignatories = sigs
    , txInfoRedeemers = fromList []
    , txInfoData = fromList []
    , txInfoId = "b0"
    }

mkPurpose :: TxOutRef -> ScriptPurpose
mkPurpose = Spending

mkRewardingPurpose :: StakingCredential -> ScriptPurpose
mkRewardingPurpose sc = Rewarding sc

mkDelegatingPurpose :: StakingCredential -> PubKeyHash -> ScriptPurpose
mkDelegatingPurpose sc pkh = Certifying $ DCertDelegDelegate sc pkh

mkContext :: TxInfo -> ScriptPurpose -> ScriptContext
mkContext cxt purpose = ScriptContext cxt purpose