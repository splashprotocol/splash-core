{-# LANGUAGE OverloadedStrings #-}

-- | Export the ordinary constant-product pool validators. These are separate
-- from the royalty bundle and do not contain DAO administrator parameters.
module Main (main) where

import Control.Monad (forM_, unless, when)
import Codec.Serialise (serialise)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as Hex
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Plutarch.Api.V2 (scriptHash)
import PlutusLedgerApi.V1.Crypto (PubKeyHash (..))
import PlutusLedgerApi.V1.Scripts (getScriptHash, unMintingPolicyScript, unValidatorScript)
import PlutusTx.Builtins (fromBuiltin, toBuiltin)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, listDirectory)
import System.Environment (getArgs)
import System.Exit (die)
import System.FilePath ((</>))
import WhalePoolsDex.PMintingValidators (daoMintPolicyValidator, daoBFeeMintPolicyValidator)
import WhalePoolsDex.PValidators (poolValidator, poolBFeeValidator)

data Input = Input
  { network :: Text.Text
  , paymentKeyHashes :: [Text.Text]
  , threshold :: Integer
  , lpFeeIsEditable :: Bool
  }

instance Aeson.FromJSON Input where
  parseJSON = Aeson.withObject "Input" $ \o -> Input
    <$> o Aeson..: "network"
    <*> o Aeson..: "paymentKeyHashes"
    <*> o Aeson..: "threshold"
    <*> o Aeson..: "lpFeeIsEditable"

-- These are payment key hashes for the approved production administrators.
-- The ordinary FeeSwitch policy checks txInfo.signatories, unlike Royalty DAO V1.
mainnetAdminPkhs :: [Text.Text]
mainnetAdminPkhs =
  [ "68aa59a87dbdbf8f78386dc6b83e63149d8c13939a1b0cda39707f9a"
  , "518a9c32deedc0b82604972692a2b7eb6c10b020d77c3c72e764b156"
  , "f3a12554ca0ccd1a220b1de839a1940bc1006f08768b636fa98c66c9"
  , "0ff61bd4fdda8767414642f83f1be47bd674381ef32fc0e8164aa37d"
  , "d350803d45e327f8808469d5dde9e0f6fdc6e6637d85ed44cc37a12c"
  , "3703634e3e34c0e7fd9a7348ad213f9272279535c73f4ec00efe00bd"
  ]

hexToPkh :: Text.Text -> Either String PubKeyHash
hexToPkh value = do
  bytes <- either (Left . show) Right $ Hex.decode $ Text.encodeUtf8 value
  if BS.length bytes == 28
    then Right $ PubKeyHash $ toBuiltin bytes
    else Left $ "administrator payment key hash must be 28 bytes, got " ++ show (BS.length bytes)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [inputFile, outputDir] -> export inputFile outputDir
    _ -> die "usage: export-simple-pools <public-admin-manifest.json> <empty-output-dir>"

export :: FilePath -> FilePath -> IO ()
export inputFile outputDir = do
  inputBytes <- BS.readFile inputFile
  input <- either die pure $ Aeson.eitherDecodeStrict' inputBytes
  unless (network input == "mainnet") $
    die "the ordinary-pool release exporter requires network=mainnet"
  unless (paymentKeyHashes input == mainnetAdminPkhs) $
    die "ordinary DAO payment key hashes differ from the pinned production set"
  unless (threshold input == 4 && lpFeeIsEditable input) $
    die "mainnet requires threshold=4 and lpFeeIsEditable=true"
  admins <- either die pure $ traverse hexToPkh (paymentKeyHashes input)
  exists <- doesDirectoryExist outputDir
  when exists $ do
    existing <- listDirectory outputDir
    unless (null existing) $
      die "output directory must be empty to avoid mixing script versions"
  createDirectoryIfMissing True outputDir
  let validatorHashHex =
        Text.unpack
          . Text.decodeUtf8
          . Hex.encode
          . fromBuiltin
          . getScriptHash
          . scriptHash
          . unValidatorScript
      validators =
        [ ("pool", poolValidator)
        , ("pool-bfee", poolBFeeValidator)
        ]
      validatorScripts =
        [ (name, LBS.toStrict $ serialise $ unValidatorScript validator, validatorHashHex validator)
        | (name, validator) <- validators
        ]
      policy = daoMintPolicyValidator admins (threshold input) (lpFeeIsEditable input)
      bfeePolicy = daoBFeeMintPolicyValidator admins (threshold input) (lpFeeIsEditable input)
      policyHashHex =
        Text.unpack
          . Text.decodeUtf8
          . Hex.encode
          . fromBuiltin
          . getScriptHash
          . scriptHash
          . unMintingPolicyScript
      scripts = validatorScripts ++
        [ ("pool-dao-policy", LBS.toStrict $ serialise $ unMintingPolicyScript policy, policyHashHex policy)
        , ("pool-bfee-dao-policy", LBS.toStrict $ serialise $ unMintingPolicyScript bfeePolicy, policyHashHex bfeePolicy)
        ]
  forM_ scripts $ \(name, bytes, _) ->
    BS.writeFile (outputDir </> (name ++ ".uplc")) bytes
  writeFile (outputDir </> "script-hashes.txt") $ unlines
    [ name ++ "=" ++ hash | (name, _, hash) <- scripts ]
  BS.writeFile (outputDir </> "export-parameters.json") inputBytes
