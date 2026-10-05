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
import System.Directory (createDirectoryIfMissing)
import System.Environment (lookupEnv)
import System.FilePath ((</>))

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
main = defaultMain $ testGroup "Contracts"
  [ tests
  , after AllSucceed "contract checks" $
      testCase "export royalty-pool deployment artifacts" exportRoyaltyPoolArtifacts
  ]

-- | Export the exact serialized scripts used for a royalty-pool deployment.
-- Set SPLASH_ARTIFACTS_DIR to override the default repository-local output
-- directory. A write failure fails the containing Tasty test case.
exportRoyaltyPoolArtifacts :: IO ()
exportRoyaltyPoolArtifacts = do
  artifactRoot <- maybe "artifacts/plutarch" id <$> lookupEnv "SPLASH_ARTIFACTS_DIR"
  let outputDir = artifactRoot </> "royalty-pools"
  createDirectoryIfMissing True outputDir
  let
    pk1 = (mkByteString . T.pack $ "0bb1d2db22f9b641f0afe8d8a398279cb778d8f86167500f7e63ebbdc35b4d69")
    pk2 = (mkByteString . T.pack $ "4e8221615500dbf6737b02992610ffeed82da6826dc3d9729febf1d32d766615")
    pk3 = (mkByteString . T.pack $ "ae536160ccec4f078982396125773d509072397e35ed6fab7af2a762ca147318")
    pk4 = (mkByteString . T.pack $ "04e3a257bcb0306c27e796bc16d1b7bde8f2306dc1d6aa344f6043ef48bd7fd8")
    pk5 = (mkByteString . T.pack $ "83d3aa4ccd1c72ff7a27c032394f44b699778b6050290e37fd82bc295f5caf18")
    pk6 = (mkByteString . T.pack $ "a7d30e99673c57638bdb65b5a0554ddee3135131940a41bbd3534b0d4c709506")
    -- hash = validatorHash 

    admins = [pk1, pk2, pk3, pk4, pk5, pk6]
    royaltyDaoValidator = royaltyPoolDAOV1Validator admins 4 True
    doubleRoyaltyDaoValidator = doubleRoyaltyPoolDAOV1Validator admins 4 True

    doubleRoyaltyPoolHash = validatorHash doubleRoyaltyPoolValidator
    royaltyPoolHash = validatorHash royaltyPoolValidator
    royaltyDaoValidatorHash = mintingPolicyHash royaltyDaoValidator
    doubleRoyaltyDaoValidatorHash = mintingPolicyHash doubleRoyaltyDaoValidator
    royaltyDoubleDepositHash = validatorHash doubleRoyaltyDepositValidator
    royaltyDoubleRedeemHash = validatorHash doubleRoyaltyRedeemValidator
    royaltyDepositHash = validatorHash royaltyDepositValidator
    royaltyRedeemHash = validatorHash royaltyRedeemValidator
    daoV1OrderValidatorHash = validatorHash royaltyPooldaoV1ActionOrderValidator
    royaltyWithdrawOrderValidatorHash = validatorHash royaltyWithdrawOrderValidator
    doubleRoyaltyWithdrawOrderValidatorHash = validatorHash doubleRoyaltyWithdrawOrderValidator
    royaltyWithdrawPoolPolicyHash = mintingPolicyHash royaltyWithdrawPoolValidator

    royaltyWithdrawOrder = LBS.toStrict $ serialise (unValidatorScript royaltyWithdrawOrderValidator)
    doubleRoyaltyWithdrawOrder = LBS.toStrict $ serialise (unValidatorScript doubleRoyaltyWithdrawOrderValidator)
    doubleRoyaltyPool = LBS.toStrict $ serialise (unValidatorScript doubleRoyaltyPoolValidator)
    royaltyPool = LBS.toStrict $ serialise (unValidatorScript royaltyPoolValidator)
    doubleRoyaltyPoolDeposit = LBS.toStrict $ serialise (unValidatorScript doubleRoyaltyDepositValidator)
    doubleRoyaltyPoolRedeem = LBS.toStrict $ serialise (unValidatorScript doubleRoyaltyRedeemValidator)
    royaltyPoolDeposit = LBS.toStrict $ serialise (unValidatorScript royaltyDepositValidator)
    royaltyPoolRedeem = LBS.toStrict $ serialise (unValidatorScript royaltyRedeemValidator)
    royaltyWithdrawPoolPolicy = LBS.toStrict $ serialise (unMintingPolicyScript royaltyWithdrawPoolValidator)
    royaltyDaoPolicy = LBS.toStrict $ serialise (unMintingPolicyScript royaltyDaoValidator)
    doubleRoyaltyDaoPolicy = LBS.toStrict $ serialise (unMintingPolicyScript doubleRoyaltyDaoValidator)
    daoV1OrderValidator = LBS.toStrict $ serialise (unValidatorScript royaltyPooldaoV1ActionOrderValidator)

  mapM_ (writeArtifact outputDir)
    [ ("royalty-pool.uplc", royaltyPool)
    , ("double-royalty-pool.uplc", doubleRoyaltyPool)
    , ("royalty-deposit.uplc", royaltyPoolDeposit)
    , ("royalty-redeem.uplc", royaltyPoolRedeem)
    , ("double-royalty-deposit.uplc", doubleRoyaltyPoolDeposit)
    , ("double-royalty-redeem.uplc", doubleRoyaltyPoolRedeem)
    , ("royalty-withdraw-order.uplc", royaltyWithdrawOrder)
    , ("double-royalty-withdraw-order.uplc", doubleRoyaltyWithdrawOrder)
    , ("royalty-withdraw-pool-policy.uplc", royaltyWithdrawPoolPolicy)
    , ("royalty-dao-v1-policy.uplc", royaltyDaoPolicy)
    , ("double-royalty-dao-v1-policy.uplc", doubleRoyaltyDaoPolicy)
    , ("royalty-dao-v1-action-order.uplc", daoV1OrderValidator)
    ]
  writeFile (outputDir </> "script-hashes.txt") $ unlines
    [ "royalty-pool=" ++ show royaltyPoolHash
    , "double-royalty-pool=" ++ show doubleRoyaltyPoolHash
    , "royalty-dao-v1-policy=" ++ show royaltyDaoValidatorHash
    , "double-royalty-dao-v1-policy=" ++ show doubleRoyaltyDaoValidatorHash
    , "double-royalty-deposit=" ++ show royaltyDoubleDepositHash
    , "double-royalty-redeem=" ++ show royaltyDoubleRedeemHash
    , "royalty-deposit=" ++ show royaltyDepositHash
    , "royalty-redeem=" ++ show royaltyRedeemHash
    , "royalty-dao-v1-action-order=" ++ show daoV1OrderValidatorHash
    , "royalty-withdraw-order=" ++ show royaltyWithdrawOrderValidatorHash
    , "double-royalty-withdraw-order=" ++ show doubleRoyaltyWithdrawOrderValidatorHash
    , "royalty-withdraw-pool-policy=" ++ show royaltyWithdrawPoolPolicyHash
    ]

writeArtifact :: FilePath -> (FilePath, BS.ByteString) -> IO ()
writeArtifact outputDir (fileName, bytes) = BS.writeFile (outputDir </> fileName) bytes

-- test123 = testGroup "TestGroup"
--   [ royaltyWithdraw ]

tests = testGroup "contract checks"
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
