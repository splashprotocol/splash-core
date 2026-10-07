{-# LANGUAGE OverloadedStrings #-}

-- | Export the ordinary constant-product pool validators. These are separate
-- from the royalty bundle and do not contain DAO administrator parameters.
module Main (main) where

import Control.Monad (forM_, unless, when)
import Codec.Serialise (serialise)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as Hex
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Plutarch.Api.V2 (scriptHash)
import PlutusLedgerApi.V1.Scripts (getScriptHash, unValidatorScript)
import PlutusTx.Builtins (fromBuiltin)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, listDirectory)
import System.Environment (getArgs)
import System.Exit (die)
import System.FilePath ((</>))
import WhalePoolsDex.PValidators (poolValidator, poolBFeeValidator)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [outputDir] -> export outputDir
    _ -> die "usage: export-simple-pools <empty-output-dir>"

export :: FilePath -> IO ()
export outputDir = do
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
      scripts =
        [ (name, LBS.toStrict $ serialise $ unValidatorScript validator, validatorHashHex validator)
        | (name, validator) <- validators
        ]
  forM_ scripts $ \(name, bytes, _) ->
    BS.writeFile (outputDir </> (name ++ ".uplc")) bytes
  writeFile (outputDir </> "script-hashes.txt") $ unlines
    [ name ++ "=" ++ hash | (name, _, hash) <- scripts ]
