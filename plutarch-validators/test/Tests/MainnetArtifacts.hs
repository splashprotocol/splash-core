{-# LANGUAGE OverloadedStrings #-}

module Tests.MainnetArtifacts (mainnetArtifacts) where

import Codec.Serialise (serialise)
import Control.Monad (forM_, unless, when)
import qualified Data.Aeson as JSON
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as Hex
import qualified Data.ByteString.Lazy as LBS
import Data.Char (isSpace)
import Data.List (nub)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Version (showVersion)
import Plutarch.Api.V2 (scriptHash)
import PlutusLedgerApi.V1.Crypto (PubKeyHash (..))
import PlutusLedgerApi.V1.Scripts
  ( Script, getScriptHash, unMintingPolicyScript, unValidatorScript )
import PlutusTx.Builtins (fromBuiltin, toBuiltin)
import System.Directory (createDirectory, createDirectoryIfMissing, doesPathExist)
import System.Environment (lookupEnv)
import System.FilePath (isAbsolute, takeDirectory, (</>))
import System.Info (compilerName, compilerVersion)
import System.Process (readProcess)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))
import qualified WhalePoolsDex.PContracts.PRoyaltyDAOV1ActionOrder as RoyaltyOrder
import qualified WhalePoolsDex.PContracts.PRoyaltyPool as RoyaltyPool
import WhalePoolsDex.PMintingValidators
import WhalePoolsDex.PValidators

-- Production public identities in their original order. No signing material
-- is needed to compile these scripts. Royalty DAO V1 uses full public keys;
-- ordinary DAO policies use payment key hashes.
royaltyAdminHex :: [String]
royaltyAdminHex =
  [ "0bb1d2db22f9b641f0afe8d8a398279cb778d8f86167500f7e63ebbdc35b4d69"
  , "4e8221615500dbf6737b02992610ffeed82da6826dc3d9729febf1d32d766615"
  , "ae536160ccec4f078982396125773d509072397e35ed6fab7af2a762ca147318"
  , "04e3a257bcb0306c27e796bc16d1b7bde8f2306dc1d6aa344f6043ef48bd7fd8"
  , "83d3aa4ccd1c72ff7a27c032394f44b699778b6050290e37fd82bc295f5caf18"
  , "a7d30e99673c57638bdb65b5a0554ddee3135131940a41bbd3534b0d4c709506"
  ]

paymentAdminHex :: [String]
paymentAdminHex =
  [ "68aa59a87dbdbf8f78386dc6b83e63149d8c13939a1b0cda39707f9a"
  , "518a9c32deedc0b82604972692a2b7eb6c10b020d77c3c72e764b156"
  , "f3a12554ca0ccd1a220b1de839a1940bc1006f08768b636fa98c66c9"
  , "0ff61bd4fdda8767414642f83f1be47bd674381ef32fc0e8164aa37d"
  , "d350803d45e327f8808469d5dde9e0f6fdc6e6637d85ed44cc37a12c"
  , "3703634e3e34c0e7fd9a7348ad213f9272279535c73f4ec00efe00bd"
  ]

decodeHex :: String -> BS.ByteString
decodeHex = either (error . show) id . Hex.decode . Text.encodeUtf8 . Text.pack

encodeHex :: BS.ByteString -> String
encodeHex = Text.unpack . Text.decodeUtf8 . Hex.encode

hashBytes :: Script -> BS.ByteString
hashBytes = fromBuiltin . getScriptHash . scriptHash

hashHex :: Script -> String
hashHex = encodeHex . hashBytes

-- (artifact name, previously published Mainnet hash, freshly compiled script)
type Artifact = (String, String, Script)

artifacts :: [Artifact]
artifacts =
  [ ("pool", "9af018bcc2b47a139a1dbf7121035c4ede6a3775fc3ab3788a6e0f1f", unValidatorScript poolValidator)
  , ("pool-bfee", "a32d92afdbcf2d90aeab7a7876423425d41f376542bf5b7edf8e7eab", unValidatorScript poolBFeeValidator)
  , ("pool-dao-policy", "038c5045480b4e31f8cdecfc0c49a342e8a7411035eff2184201ae93", unMintingPolicyScript $ daoMintPolicyValidator paymentAdmins 4 True)
  , ("pool-bfee-dao-policy", "f5981b0103830fdd8aed2f6774bb355878448547810ca3b78453500c", unMintingPolicyScript $ daoBFeeMintPolicyValidator paymentAdmins 4 True)
  , ("royalty-pool", "72ed0eaa3594bfadd006992ed574dee50bbced070955c0c296fb8f8e", unValidatorScript royaltyPoolValidator)
  , ("double-royalty-pool", "ca6c752e43e47c6287d137b6d3f0f0d22a5c23b61910e3abcdcf81b2", unValidatorScript doubleRoyaltyPoolValidator)
  , ("royalty-deposit", "5a04e05f6fd8b6e03c19619417abfec145b44f237d58bed30dc9f694", unValidatorScript royaltyDepositValidator)
  , ("royalty-redeem", "fceb645fdc06550d62065bc0f95318e8dcd71e6a8c13c05f313b246d", unValidatorScript royaltyRedeemValidator)
  , ("double-royalty-deposit", "821a8ed3a90050b9a7f17c7ccdb94d4a79e3e425cff22d7bc0771d0b", unValidatorScript doubleRoyaltyDepositValidator)
  , ("double-royalty-redeem", "16635db270fd379d0d3467dc09b6d0aa613e9ffe6fdf5a02fa5a2154", unValidatorScript doubleRoyaltyRedeemValidator)
  , ("royalty-withdraw-order", "92c094b90cf3637a96a13e9bc9a04ce8bb7e48c7ed0b5d1cc5ca7332", unValidatorScript royaltyWithdrawOrderValidator)
  , ("double-royalty-withdraw-order", "6710f3002759d028a90a08e1538b25d4eaf48c72d88157f71d68dcc0", unValidatorScript doubleRoyaltyWithdrawOrderValidator)
  , ("royalty-withdraw-pool-policy", "e0de1016c69f04483037d4cb6cc85bb6d42bc75b80774bc5bc48eb2f", unMintingPolicyScript royaltyWithdrawPoolValidator)
  , ("royalty-dao-v1-policy", "76847f451fb7c4e1d4b93284bb5ec32439770053d86804b7b687c60f", singleDao)
  , ("double-royalty-dao-v1-policy", "45613319ca4393e41499ffe47cd055718ddf2a1a1fe10c8fd879d0ee", doubleDao)
  , ("royalty-dao-v1-action-order", "6a6af4b2aaccd011734da14b6886c95f252eddf68715172d6ce0f050", unValidatorScript $ royaltyPooldaoV1ActionOrderValidatorFor $ hashBytes singleDao)
  , ("double-royalty-dao-v1-action-order", "e25f37a05bb240778c270090f579e2ff73c3b071431a73a63ce657a5", unValidatorScript $ royaltyPooldaoV1ActionOrderValidatorFor $ hashBytes doubleDao)
  ]
  where
    paymentAdmins = map (PubKeyHash . toBuiltin . decodeHex) paymentAdminHex

singleDao, doubleDao :: Script
singleDao = unMintingPolicyScript $ royaltyPoolDAOV1Validator (map decodeHex royaltyAdminHex) 4 True
doubleDao = unMintingPolicyScript $ doubleRoyaltyPoolDAOV1Validator (map decodeHex royaltyAdminHex) 4 True

-- Verified from the raw CBOR exported at e1c2754fe8e4aec81a678df6ae1fe7c5fbf23fdb.
-- Double pool CBOR SHA-256: d0c087c4d359351e9c1fa36abb6999ddd11ea7ef95b4e7b380338ba492700a7d.
-- The tuple retains the previously published hash for the comparison report.
expectedHash :: Artifact -> String
expectedHash (name, published, _)
  | name == "double-royalty-pool" = "b6a3a2e8f7ed717a9c98257a908637dd06f4703f9c03df55a9b2c66e"
  | otherwise = published

comparison :: Artifact -> String
comparison artifact@(_, published, script)
  | hashHex script /= expectedHash artifact = "UNEXPECTED"
  | expectedHash artifact /= published = "PINNED_CHANGE"
  | otherwise = "MATCH"

-- Pin all 17 script hashes to the verified artifact set. Hash equality is not
-- deployment approval. Behavior is checked separately by PoolSafety.
mainnetArtifacts :: TestTree
mainnetArtifacts = testCase "Mainnet script hashes and export" $ do
  assertBool "expected six distinct 32-byte Mainnet verification keys" $
    length royaltyAdminHex == 6 && length (nub royaltyAdminHex) == 6 &&
    all ((== 32) . BS.length . decodeHex) royaltyAdminHex
  assertBool "expected six distinct 28-byte Mainnet payment key hashes" $
    length paymentAdminHex == 6 && length (nub paymentAdminHex) == 6 &&
    all ((== 28) . BS.length . decodeHex) paymentAdminHex
  length artifacts @?= 17
  forM_ artifacts $ \artifact@(name, published, script) ->
    putStrLn $ name ++ "=" ++ hashHex script ++ " published=" ++ published ++
      " expected=" ++ expectedHash artifact ++ " " ++ comparison artifact
  assertBool "unexpected script hash change; see rows above" $
    all ((/= "UNEXPECTED") . comparison) artifacts
  toBuiltin (hashBytes $ unMintingPolicyScript royaltyWithdrawPoolValidator)
    @?= RoyaltyPool.royaltyWithdrawPoolScriptHash
  toBuiltin (hashBytes singleDao) @?= RoyaltyOrder.daoV1RoyaltyPoolScriptHash
  hashHex (unValidatorScript royaltyPooldaoV1ActionOrderValidator)
    @?= hashHex (unValidatorScript $ royaltyPooldaoV1ActionOrderValidatorFor $ hashBytes singleDao)
  outputDir <- lookupEnv "MAINNET_ARTIFACTS_DIR"
  case outputDir of
    Nothing -> putStrLn "Hash checks complete. Set MAINNET_ARTIFACTS_DIR to a new absolute directory to export candidates."
    Just path -> exportArtifacts path

exportArtifacts :: FilePath -> IO ()
exportArtifacts outputDir = do
  unless (isAbsolute outputDir) $ fail "MAINNET_ARTIFACTS_DIR must be an absolute path"
  exists <- doesPathExist outputDir
  when exists $ fail "Artifact directory already exists; choose a new directory to avoid mixing exports"
  trackedChanges <- readProcess "git" ["status", "--porcelain=v1", "--untracked-files=no"] ""
  unless (null trackedChanges) $ fail "Commit or restore tracked changes before exporting; the source tree must match HEAD"
  sourceCommit <- trim <$> readProcess "git" ["rev-parse", "HEAD"] ""
  cabalVersion <- trim <$> readProcess "cabal" ["--numeric-version"] ""
  createDirectoryIfMissing True (takeDirectory outputDir)
  createDirectory outputDir
  let rawDir = outputDir </> "raw-cbor"
      hexDir = outputDir </> "hex"
  createDirectory rawDir
  createDirectory hexDir
  forM_ artifacts $ \(name, _, script) -> do
    -- Serialise Script exactly once; .uplc is binary CBOR, .hex encodes those
    -- same bytes. Never serialise the resulting ByteString a second time.
    let bytes = LBS.toStrict $ serialise script
    BS.writeFile (rawDir </> (name ++ ".uplc")) bytes
    writeFile (hexDir </> (name ++ ".hex")) (encodeHex bytes ++ "\n")
    putStrLn $ "Exported " ++ name ++ ": " ++ show (BS.length bytes) ++ " bytes"
  writeFile (rawDir </> "script-hashes.txt") $ unlines
    [name ++ "=" ++ hashHex script | (name, _, script) <- artifacts]
  writeFile (outputDir </> "hash-comparison.csv") $ unlines $
    "script,published_hash,compiled_hash,status" :
    [name ++ "," ++ published ++ "," ++ hashHex script ++ "," ++ comparison artifact
      | artifact@(name, published, script) <- artifacts]
  LBS.writeFile (outputDir </> "export-parameters.json") $ JSON.encode $ JSON.object
    [ "network" JSON..= ("mainnet" :: String)
    , "status" JSON..= ("candidate-awaiting-byte-verification" :: String)
    , "deploymentApproved" JSON..= False
    , "sourceCommit" JSON..= sourceCommit
    , "trackedWorkingTreeClean" JSON..= True
    , "compiler" JSON..= (compilerName ++ "-" ++ showVersion compilerVersion)
    , "cabalInstallVersion" JSON..= cabalVersion
    , "scriptLanguage" JSON..= ("PlutusV2" :: String)
    , "tracingMode" JSON..= ("NoTracing" :: String)
    , "threshold" JSON..= (4 :: Integer)
    , "lpFeeIsEditable" JSON..= True
    , "daoAdminVerificationKeys" JSON..= royaltyAdminHex
    , "paymentKeyHashes" JSON..= paymentAdminHex
    , "expectedHashChanges" JSON..= (["double-royalty-pool"] :: [String])
    , "externalDoubleRoyaltyWithdrawalPolicyHash" JSON..=
        ("a062da72f9f3280fbed90b3e95ea943aabd7eff9c82343324a9726ad" :: String)
    ]
  writeFile (outputDir </> "README.txt") $ unlines
    [ "Mainnet candidate scripts built from " ++ sourceCommit
    , "The double royalty pool has a new WithdrawRoyalty asset-preservation guard."
    , "All 17 script hashes are pinned to the verified e1c2754 artifact set."
    , "16 match published hashes; double-royalty-pool must match its new pinned hash."
    , "raw-cbor/*.uplc: binary serialised Plutus V2 Script. hex/*.hex: the same bytes in hex."
    , "DAO keys are production PUBLIC keys only, threshold 4, lpFeeIsEditable=True."
    , "The external Plutus V3 double royalty withdrawal policy is NOT exported by this test."
    , "BalancePool is outside this release. Nothing is published or submitted by this test."
    , "Passing hash checks does not establish behavior or approve deployment."
    , "Return the entire directory together with the regression-test log for byte verification."
    ]
  -- Last file marks a finished export; an interrupted run must not be mistaken
  -- for a complete bundle. Review the test log as well before using the files.
  writeFile (outputDir </> "EXPORT_COMPLETE") (sourceCommit ++ "\n")
  putStrLn $ "Candidate artifacts written to: " ++ outputDir
  where
    trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace
