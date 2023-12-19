module WhalePoolsDex.PConstants where

import qualified Data.Text as T

import PlutusTx.Builtins.Internal

import Plutarch
import Plutarch.Api.V2 (mkMintingPolicy, scriptHash, PCurrencySymbol(..), PTokenName(..))
import Plutarch.Api.V1.Scripts
import Plutarch.Api.V1.Address
import Plutarch.Prelude

import qualified PlutusLedgerApi.V1 as Plutus

import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16  as Hex
import qualified Data.Text.Encoding      as E
import qualified Data.Text as T

poolValidatorHashValue :: String
poolValidatorHashValue = "6e5b9b7fbf5690879606ef5624052a9a00cf9b3541d059c7c615461b"

poolValidatorHash :: Plutus.ValidatorHash
poolValidatorHash = Plutus.ValidatorHash $ BuiltinByteString . mkByteString $ T.pack poolValidatorHashValue

poolValidatorHashP :: Term s PValidatorHash
poolValidatorHashP = pcon $ PValidatorHash $ phexByteStr poolValidatorHashValue

poolCredP :: Term s PCredential
poolCredP = pcon $ (PScriptCredential $ pdcons # (pdata poolValidatorHashP) # pdnil)

poolStakeChangeMintTnValue :: String
poolStakeChangeMintTnValue = "746e"

poolStakeChangeMintTokenName:: Plutus.TokenName
poolStakeChangeMintTokenName = Plutus.TokenName $ BuiltinByteString . mkByteString $ T.pack poolStakeChangeMintTnValue

poolStakeChangeMintTokenNameP :: Term s PTokenName
poolStakeChangeMintTokenNameP = pcon $ PTokenName $ phexByteStr poolStakeChangeMintTnValue

mkByteString :: T.Text -> BS.ByteString
mkByteString input = unsafeFromEither (Hex.decode . E.encodeUtf8 $ input)

unsafeFromEither :: (Show b) => Either b a -> a
unsafeFromEither (Left err)    = Prelude.error ("Err:" ++ show err)
unsafeFromEither (Right value) = value