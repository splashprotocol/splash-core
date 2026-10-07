{-# LANGUAGE OverloadedStrings #-}

-- | Export DAO V1 scripts for Preprod, or the complete royalty script set for
-- Mainnet. Mainnet parameters are pinned below to prevent a test-key export.
--
-- The input JSON is public and contains raw Ed25519 verification keys plus
-- descriptive Cardano payment-key hashes. Mnemonics and private keys are deliberately outside this exporter and must never be passed
-- to it or stored in the output directory.
module Main (main) where

import Control.Monad (forM_, unless, when)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as Hex
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Codec.Serialise (serialise)
import PlutusLedgerApi.V1.Scripts (getScriptHash, unMintingPolicyScript, unValidatorScript)
import PlutusTx.Builtins (fromBuiltin)
import Plutarch.Api.V2 (scriptHash)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, listDirectory)
import System.Environment (getArgs)
import System.Exit (die)
import System.FilePath ((</>))
import WhalePoolsDex.PMintingValidators
  ( royaltyPoolDAOV1Validator
  , doubleRoyaltyPoolDAOV1Validator
  , royaltyWithdrawPoolValidator
  )
import WhalePoolsDex.PValidators
  ( royaltyPooldaoV1ActionOrderValidatorFor
  , royaltyPoolValidator
  , doubleRoyaltyPoolValidator
  , royaltyDepositValidator
  , royaltyRedeemValidator
  , doubleRoyaltyDepositValidator
  , doubleRoyaltyRedeemValidator
  , royaltyWithdrawOrderValidator
  , doubleRoyaltyWithdrawOrderValidator
  )

data Input = Input
  { network :: Text.Text
  , daoAdminVerificationKeys :: [Text.Text]
  , threshold :: Integer
  , lpFeeIsEditable :: Bool
  }

instance Aeson.FromJSON Input where
  parseJSON = Aeson.withObject "Input" $ \o -> Input
    <$> o Aeson..: "network"
    <*> o Aeson..: "daoAdminVerificationKeys"
    <*> o Aeson..: "threshold"
    <*> o Aeson..: "lpFeeIsEditable"

hexToVerificationKey :: Text.Text -> Either String BS.ByteString
hexToVerificationKey t = do
  let encoded = Text.encodeUtf8 t
  bytes <- either (Left . show) Right (Hex.decode encoded)
  if BS.length bytes == 32
    then Right bytes
    else Left $ "DAO administrator Ed25519 verification key must be 32 bytes, got " ++ show (BS.length bytes)

-- These are the ordered production verification keys supplied for the royalty
-- DAO. A different administrator set requires an explicit source review.
mainnetAdministrators :: [Text.Text]
mainnetAdministrators =
  [ "0bb1d2db22f9b641f0afe8d8a398279cb778d8f86167500f7e63ebbdc35b4d69"
  , "4e8221615500dbf6737b02992610ffeed82da6826dc3d9729febf1d32d766615"
  , "ae536160ccec4f078982396125773d509072397e35ed6fab7af2a762ca147318"
  , "04e3a257bcb0306c27e796bc16d1b7bde8f2306dc1d6aa344f6043ef48bd7fd8"
  , "83d3aa4ccd1c72ff7a27c032394f44b699778b6050290e37fd82bc295f5caf18"
  , "a7d30e99673c57638bdb65b5a0554ddee3135131940a41bbd3534b0d4c709506"
  ]

main :: IO ()
main = do
  args <- getArgs
  case args of
    [inputFile, outputDir] -> export inputFile outputDir
    _ -> die "usage: export-royalty-dao-v1 <public-admin-manifest.json> <output-dir>"

export :: FilePath -> FilePath -> IO ()
export inputFile outputDir = do
  inputBytes <- BS.readFile inputFile
  input <- either die pure $ Aeson.eitherDecodeStrict' inputBytes
  when (network input /= "preprod" && network input /= "mainnet") $
    die "network must be preprod or mainnet"
  when (length (daoAdminVerificationKeys input) /= 6) $
    die "exactly six DAO administrators are required"
  when (threshold input < 1 || threshold input > fromIntegral (length (daoAdminVerificationKeys input))) $
    die "threshold must be between 1 and the number of DAO administrators"
  admins <- either die pure $ traverse hexToVerificationKey (daoAdminVerificationKeys input)
  when (network input == "mainnet") $ do
    unless (daoAdminVerificationKeys input == mainnetAdministrators) $
      die "mainnet DAO administrator keys differ from the pinned production set"
    unless (threshold input == 4 && lpFeeIsEditable input) $
      die "mainnet requires threshold=4 and lpFeeIsEditable=true"
  exists <- doesDirectoryExist outputDir
  when exists $ do
    existing <- listDirectory outputDir
    unless (null existing) $
      die "output directory must be empty to avoid mixing script versions"
  createDirectoryIfMissing True outputDir
  let royalty = royaltyPoolDAOV1Validator admins (threshold input) (lpFeeIsEditable input)
      doubleRoyalty = doubleRoyaltyPoolDAOV1Validator admins (threshold input) (lpFeeIsEditable input)
      royaltyBytes = LBS.toStrict $ serialise (unMintingPolicyScript royalty)
      doubleRoyaltyBytes = LBS.toStrict $ serialise (unMintingPolicyScript doubleRoyalty)
      scriptHashHex =
        Text.unpack
          . Text.decodeUtf8
          . Hex.encode
          . fromBuiltin
          . getScriptHash
          . scriptHash
          . unMintingPolicyScript
      royaltyHash = scriptHashHex royalty
      doubleRoyaltyHash = scriptHashHex doubleRoyalty
      royaltyHashBytes = either (error . show) id $ Hex.decode (Text.encodeUtf8 $ Text.pack royaltyHash)
      doubleRoyaltyHashBytes = either (error . show) id $ Hex.decode (Text.encodeUtf8 $ Text.pack doubleRoyaltyHash)
      royaltyRequest = royaltyPooldaoV1ActionOrderValidatorFor royaltyHashBytes
      doubleRoyaltyRequest = royaltyPooldaoV1ActionOrderValidatorFor doubleRoyaltyHashBytes
      royaltyRequestBytes = LBS.toStrict $ serialise (unValidatorScript royaltyRequest)
      doubleRoyaltyRequestBytes = LBS.toStrict $ serialise (unValidatorScript doubleRoyaltyRequest)
      validatorHashHex =
        Text.unpack
          . Text.decodeUtf8
          . Hex.encode
          . fromBuiltin
          . getScriptHash
          . scriptHash
          . unValidatorScript
      royaltyRequestHash = validatorHashHex royaltyRequest
      doubleRoyaltyRequestHash = validatorHashHex doubleRoyaltyRequest
      policyScripts =
        [ ("royalty-dao-v1-policy", royaltyBytes, royaltyHash)
        , ("double-royalty-dao-v1-policy", doubleRoyaltyBytes, doubleRoyaltyHash)
        , ("royalty-dao-v1-action-order", royaltyRequestBytes, royaltyRequestHash)
        , ("double-royalty-dao-v1-action-order", doubleRoyaltyRequestBytes, doubleRoyaltyRequestHash)
        ]
      mainnetScripts =
        [ ("royalty-pool", LBS.toStrict $ serialise $ unValidatorScript royaltyPoolValidator, validatorHashHex royaltyPoolValidator)
        , ("double-royalty-pool", LBS.toStrict $ serialise $ unValidatorScript doubleRoyaltyPoolValidator, validatorHashHex doubleRoyaltyPoolValidator)
        , ("royalty-deposit", LBS.toStrict $ serialise $ unValidatorScript royaltyDepositValidator, validatorHashHex royaltyDepositValidator)
        , ("royalty-redeem", LBS.toStrict $ serialise $ unValidatorScript royaltyRedeemValidator, validatorHashHex royaltyRedeemValidator)
        , ("double-royalty-deposit", LBS.toStrict $ serialise $ unValidatorScript doubleRoyaltyDepositValidator, validatorHashHex doubleRoyaltyDepositValidator)
        , ("double-royalty-redeem", LBS.toStrict $ serialise $ unValidatorScript doubleRoyaltyRedeemValidator, validatorHashHex doubleRoyaltyRedeemValidator)
        , ("royalty-withdraw-order", LBS.toStrict $ serialise $ unValidatorScript royaltyWithdrawOrderValidator, validatorHashHex royaltyWithdrawOrderValidator)
        , ("double-royalty-withdraw-order", LBS.toStrict $ serialise $ unValidatorScript doubleRoyaltyWithdrawOrderValidator, validatorHashHex doubleRoyaltyWithdrawOrderValidator)
        , ("royalty-withdraw-pool-policy", LBS.toStrict $ serialise $ unMintingPolicyScript royaltyWithdrawPoolValidator, scriptHashHex royaltyWithdrawPoolValidator)
        ]
      scripts = if network input == "mainnet" then mainnetScripts ++ policyScripts else policyScripts
  forM_ scripts $ \(name, bytes, _) -> BS.writeFile (outputDir </> (name ++ ".uplc")) bytes
  writeFile (outputDir </> "script-hashes.txt") $ unlines
    [ name ++ "=" ++ hash | (name, _, hash) <- scripts ]
  BS.writeFile (outputDir </> "export-parameters.json") inputBytes
