{-# LANGUAGE BlockArguments #-}

module Main(main) where

import WhalePoolsDex.PMintingValidators

import Tests.Deposit
import Tests.Pool
import Tests.PoolBFee
import Tests.Swap
import Tests.Redeem
import Tests.Staking
import Tests.Api
import Tests.FeeSwitch
import Tests.FeeSwitchBFee
import Tests.BalancePool
-- import Tests.RoyaltyWithdraw

import Test.Tasty
import Test.Tasty.HUnit

import WhalePoolsDex.PValidators
import PlutusLedgerApi.V2 as PV2
import Plutarch.Api.V2
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Codec.Serialise (serialise, deserialise)
import qualified Data.ByteString.Base16  as Hex
import qualified Data.Text.Encoding      as E
import qualified Data.ByteString.Short  as SBS
import qualified PlutusLedgerApi.V2 as PlutusV2
import qualified Data.Text as T
import PlutusTx.Builtins.Internal (BuiltinByteString(..))
import PlutusLedgerApi.V1.Value
import Debug.Trace

mkPubKeyHash :: String -> PubKeyHash
mkPubKeyHash str = PubKeyHash $ BuiltinByteString $ mkByteString . T.pack $ str

mkByteString :: T.Text -> BS.ByteString
mkByteString input = unsafeFromEither (Hex.decode . E.encodeUtf8 $ input)

unsafeFromEither :: (Show b) => Either b a -> a
unsafeFromEither (Left err)    = Prelude.error ("Err:" ++ show err)
unsafeFromEither (Right value) = value

mintingPolicyHash :: MintingPolicy -> MintingPolicyHash
mintingPolicyHash =
    MintingPolicyHash
  . getScriptHash
  . scriptHash
  . PlutusV2.getMintingPolicy

main :: IO ()
main = do
  -- defaultMain test123
  let
  --   nftCS = "e07f13716ab4dfffb388ad7c67a6ed6642dc9f1145f5c3077ecd33e8"
  --   nftTN = "6e6674"

  --   cs = CurrencySymbol $ BuiltinByteString $ mkByteString $ T.pack nftCS
  --   tn = TokenName $ BuiltinByteString $ mkByteString $ T.pack nftTN
  --   ac = AssetClass (cs, tn)

    pk1 = (mkByteString . T.pack $ "0bb1d2db22f9b641f0afe8d8a398279cb778d8f86167500f7e63ebbdc35b4d69")
    pk2 = (mkByteString . T.pack $ "4e8221615500dbf6737b02992610ffeed82da6826dc3d9729febf1d32d766615")
    pk3 = (mkByteString . T.pack $ "ae536160ccec4f078982396125773d509072397e35ed6fab7af2a762ca147318")
    pk4 = (mkByteString . T.pack $ "04e3a257bcb0306c27e796bc16d1b7bde8f2306dc1d6aa344f6043ef48bd7fd8")
    pk5 = (mkByteString . T.pack $ "83d3aa4ccd1c72ff7a27c032394f44b699778b6050290e37fd82bc295f5caf18")
    pk6 = (mkByteString . T.pack $ "a7d30e99673c57638bdb65b5a0554ddee3135131940a41bbd3534b0d4c709506")
    -- hash = validatorHash 

    doubelRoyaltyDaoValidator = doubleRoyaltyPoolDAOV1Validator [pk1, pk2, pk3, pk4, pk5, pk6] 4 True

    doubleRoyaltyWithdrawPoolHash = mintingPolicyHash doubleRoyaltyWithdrawPoolValidator
    royaltyPoolHash = validatorHash doubleRoyaltyPoolValidator
    daoV1ValidatorHash = mintingPolicyHash doubelRoyaltyDaoValidator
    royaltyDoubleDepositHash = validatorHash doubleRoyaltyDepositValidator
    royaltyDoubleRedeemHash = validatorHash doubleRoyaltyRedeemValidator
    -- daoV1OrderValidatorHash = validatorHash royaltyPooldaoV1ActionOrderValidator

    doubleRoyaltyWithdrawPool = LBS.toStrict $ serialise (unMintingPolicyScript doubleRoyaltyWithdrawPoolValidator)
    --royaltyWithdrawOrder = LBS.toStrict $ serialise (unValidatorScript royaltyWithdrawOrderValidator)
    doubelRoyaltyPool = LBS.toStrict $ serialise (unValidatorScript doubleRoyaltyPoolValidator)
    doubleRoyaltyPoolDeposit = LBS.toStrict $ serialise (unValidatorScript doubleRoyaltyDepositValidator)
    doubleRoyaltyPoolRedeem = LBS.toStrict $ serialise (unValidatorScript doubleRoyaltyRedeemValidator)
    daoV1Validator = LBS.toStrict $ serialise (unMintingPolicyScript doubelRoyaltyDaoValidator)
    -- daoV1OrderValidator = LBS.toStrict $ serialise (unValidatorScript royaltyPooldaoV1ActionOrderValidator)

  traceM $ "royaltyWithdraw doueble pool hash: " ++ show doubleRoyaltyWithdrawPoolHash
  traceM $ "roaylty doeuble pool hash: " ++ show royaltyPoolHash
  traceM $ "roaylty double dao v1 hash: " ++ show daoV1ValidatorHash
  traceM $ "roaylty double pool deposit: " ++ show royaltyDoubleDepositHash
  traceM $ "roaylty double pool redeem: " ++ show royaltyDoubleRedeemHash
  --traceM $ "roaylty dao v1 order hash: " ++ show daoV1OrderValidatorHash
  BS.writeFile ("/home/bromel/projects/whalepools-core/plutarch-validators/doubleRoyaltyWithdrawPool.uplc") doubleRoyaltyWithdrawPool
  -- BS.writeFile ("/home/bromel/projects/whalepools-core/plutarch-validators/royaltyWithdrawOrder.uplc") royaltyWithdrawOrder
  BS.writeFile ("/home/bromel/projects/whalepools-core/plutarch-validators/doubleRoyaltyPool.uplc") doubelRoyaltyPool
  BS.writeFile ("/home/bromel/projects/whalepools-core/plutarch-validators/doubleRoyaltyDeposit.uplc") doubleRoyaltyPoolDeposit
  BS.writeFile ("/home/bromel/projects/whalepools-core/plutarch-validators/doubleRoyaltyRedeem.uplc") doubleRoyaltyPoolRedeem
  BS.writeFile ("/home/bromel/projects/whalepools-core/plutarch-validators/doubleRoyaltyDAOV1.uplc") daoV1Validator
  --BS.writeFile ("/home/bromel/projects/whalepools-core/plutarch-validators/royaltyDAOV1Order.uplc") daoV1OrderValidator
  pure ()

-- test123 = testGroup "TestGroup"
--   [ royaltyWithdraw ]

tests = testGroup "Contracts"
  [ feeSwitch
  , feeSwitchBFee
  , balancePool
  , checkPValueLength
  , checkPool
  , checkPoolRedeemer
  , checkPoolBFee
  , checkPoolBFeeRedeemer
  , checkRedeem
  , checkRedeemIdentity
  , checkRedeemIsFair
  , checkRedeemRedeemer
  , checkDeposit 
  , checkDepositChange
  , checkDepositRedeemer
  , checkDepositIdentity
  , checkDepositLq
  , checkDepositTokenReward
  , checkSwap
  , checkSwapRedeemer
  , checkSwapIdentity
  ]