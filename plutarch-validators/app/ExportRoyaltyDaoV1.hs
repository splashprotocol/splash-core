{-# LANGUAGE OverloadedStrings #-}

-- | Export the parameterized DAO V1 minting policies used exclusively by the
-- Preprod royalty-pool integration environment.
--
-- The input JSON is public and contains raw Ed25519 verification keys plus
-- descriptive Cardano payment-key hashes. Mnemonics and private keys are deliberately outside this exporter and must never be passed
-- to it or stored in the output directory.
module Main (main) where

import Control.Monad (unless, when)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as Hex
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Codec.Serialise (serialise)
import PlutusLedgerApi.V1.Scripts (getScriptHash, scriptHash, unMintingPolicyScript)
import System.Directory (createDirectoryIfMissing)
import System.Environment (getArgs)
import System.Exit (die)
import System.FilePath ((</>))
import WhalePoolsDex.PMintingValidators
  ( royaltyPoolDAOV1Validator
  , doubleRoyaltyPoolDAOV1Validator
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

main :: IO ()
main = do
  args <- getArgs
  case args of
    [inputFile, outputDir] -> export inputFile outputDir
    _ -> die "usage: export-royalty-dao-v1 <public-admin-manifest.json> <output-dir>"

export :: FilePath -> FilePath -> IO ()
export inputFile outputDir = do
  decoded <- Aeson.eitherDecodeStrict' <$> BS.readFile inputFile
  input <- either die pure decoded
  when (network input /= "preprod") $
    die "the exporter is restricted to a manifest declaring network=preprod"
  when (length (daoAdminVerificationKeys input) /= 6) $
    die "exactly six DAO administrators are required"
  when (threshold input < 1 || threshold input > fromIntegral (length (daoAdminVerificationKeys input))) $
    die "threshold must be between 1 and the number of DAO administrators"
  admins <- either die pure $ traverse hexToVerificationKey (daoAdminVerificationKeys input)
  unless (length admins == length (daoAdminVerificationKeys input)) $
    die "invalid DAO administrator list"
  createDirectoryIfMissing True outputDir
  let royalty = royaltyPoolDAOV1Validator admins (threshold input) (lpFeeIsEditable input)
      doubleRoyalty = doubleRoyaltyPoolDAOV1Validator admins (threshold input) (lpFeeIsEditable input)
      royaltyBytes = LBS.toStrict $ serialise (unMintingPolicyScript royalty)
      doubleRoyaltyBytes = LBS.toStrict $ serialise (unMintingPolicyScript doubleRoyalty)
      royaltyHash = show . getScriptHash . scriptHash . unMintingPolicyScript $ royalty
      doubleRoyaltyHash = show . getScriptHash . scriptHash . unMintingPolicyScript $ doubleRoyalty
  BS.writeFile (outputDir </> "royalty-dao-v1-policy.uplc") royaltyBytes
  BS.writeFile (outputDir </> "double-royalty-dao-v1-policy.uplc") doubleRoyaltyBytes
  writeFile (outputDir </> "script-hashes.txt") $ unlines
    [ "royalty-dao-v1-policy=" ++ royaltyHash
    , "double-royalty-dao-v1-policy=" ++ doubleRoyaltyHash
    ]
