module WhalePoolsDex.PContracts.PRoyaltyDAOV1Request where

import qualified GHC.Generics as GHC

import WhalePoolsDex.PContracts.PApi         (tletUnwrap, containsSignature, getRewardValue')
import WhalePoolsDex.PContracts.POrder

import PExtra.Monadic
import Plutarch
import Plutarch.Api.V2 
import Plutarch.Prelude
import Plutarch.Extra.TermCont
import Plutarch.DataRepr
import Plutarch.Builtin             (pasInt, pforgetData, PIsData(..))
import Plutarch.Unsafe              (punsafeCoerce)
import Plutarch.Api.V1.Scripts
import Plutarch.Api.V1.AssocMap     as Map
import Plutarch.Extra.Maybe         as Maybe
import PlutusTx.Builtins.Internal
import PExtra.API                   (assetClassValueOf, PAssetClass(..))
import Plutarch.Internal.PlutusType (pcon', pmatch')
import PlutusLedgerApi.V2           hiding (getValue)
import PExtra.Ada
import Data.Text                    as T
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16  as Hex
import qualified Data.Text.Encoding      as E

mkByteString :: T.Text -> BS.ByteString
mkByteString input = unsafeFromEither (Hex.decode . E.encodeUtf8 $ input)

unsafeFromEither :: (Show b) => Either b a -> a
unsafeFromEither (Left err)    = Prelude.error ("Err:" ++ show err)
unsafeFromEither (Right value) = value

daoV1RoyaltyPoolScriptHash :: BuiltinByteString
daoV1RoyaltyPoolScriptHash = BuiltinByteString $ mkByteString . T.pack $ "8f164daa025635c07896e067a2a952a6bc6192550169080738459ff0"

daoV1RoyaltyPoolCred :: Term s PStakingCredential
daoV1RoyaltyPoolCred = pconstant (StakingHash . ScriptCredential . ValidatorHash $ daoV1RoyaltyPoolScriptHash)

data DAOAction (s :: S) = WithdrawTreasury | ChangeStakePart | ChangeTreasuryFee | ChangeTreasuryAddress | ChangeAdminAddress | ChangePoolFee

instance PIsData DAOAction where
    pfromDataImpl tx =
        let x = pasInt # pforgetData tx
         in pmatch' x pcon
    pdataImpl x = pmatch x (punsafeCoerce . pdata . pcon')

instance PlutusType DAOAction where
    type PInner DAOAction = PInteger

    pcon' WithdrawTreasury = 0
    pcon' ChangeStakePart = 1
    pcon' ChangeTreasuryFee = 2
    pcon' ChangeTreasuryAddress = 3
    pcon' ChangeAdminAddress = 4
    pcon' ChangePoolFee = 5

    pmatch' x f =
        pif
            (x #== 0)
            (f WithdrawTreasury)
            ( pif
                (x #== 1)
                (f ChangeStakePart)
                ( pif
                    (x #== 2)
                    (f ChangeTreasuryFee)
                    ( pif 
                        (x #== 3) 
                        (f ChangeTreasuryAddress)
                        (pif (x #== 4) (f ChangeAdminAddress) (f ChangePoolFee))
                    )
                )
            )

{-|

This configuration mirrors DataToSign but omits fields like signatures, poolX, poolY, and poolNft.
- Fields not required for contract evaluation are used by the batcher to construct the final pool UTXO view.
- Fields used by the contract support validation of UTXO spending accuracy.

|-}

newtype DAOV1RequestConfig (s :: S)
    = DAOV1RequestConfig
        ( Term
            s
            ( PDataRecord
                '[ "daoAction"           ':= DAOAction
                 , "poolNft"             ':= PAssetClass
                 , "poolFee"             ':= PInteger
                 , "treasuryFee"         ':= PInteger
                 , "adminAddress"        ':= PBuiltinList (PAsData PStakingCredential)
                 , "poolAddress"         ':= PAddress
                 , "treasuryAddress"     ':= PValidatorHash
                 , "treasuryXWithdraw"   ':= PInteger
                 , "treasuryYWithdraw"   ':= PInteger
                 , "requestorPkh"        ':= PPubKeyHash
                 , "signatures"          ':= PBuiltinList (PAsData PByteString)
                 , "exFee"               ':= PInteger
                 ]
            )
        )
    deriving stock (GHC.Generic)
    deriving
        (PIsData, PDataFields, PlutusType)

instance DerivePlutusType DAOV1RequestConfig where type DPTStrat _ = PlutusTypeData

{-|

The DAO V1 Request contract stores essential data within the UTXO datum, representing necessary parameters for DAO action execution. 
It also includes logic to validate the correctness of UTXO spending but does not directly execute DAO actions itself. 

Actions:
---------
The contract manages two primary actions, focusing on validation and handling: Apply and Refund.

1. **Apply Action**
    - Verifies the presence of the correct pool UTXO by checking for a unique NFT, confirming valid pool operation.
    - Ensures accurate execution fee deduction:
      - Calculated as `exFee = inputLovelace − outputLovelace`, where outputLovelace derives from the requestorPkh's address.
    - Requires exactly two inputs in the transaction.
    - Confirms proper DAO V1 contract execution by verifying withdrawal details in the transaction.

2. **Refund Action**
    - Ensures the transaction is signed by the private key corresponding to the public key hash requestorPkh.
-}

daoV1RequestValidator :: Term s (DAOV1RequestConfig :--> OrderRedeemer :--> PScriptContext :--> PBool)
daoV1RequestValidator = plam $ \config redeemer' ctx' -> unTermCont $ do
  ctx      <- pletFieldsC @'["txInfo", "purpose"] ctx'
  config'  <- pletFieldsC @'["daoAction", "poolNft", "treasuryXWithdraw", "treasuryYWithdraw", "requestorPkh", "exFee"] config
  let
    txInfo'  = getField @"txInfo" ctx

  txInfo   <- pletFieldsC @'["inputs", "outputs", "signatories"] txInfo'
  redeemer <- pletFieldsC @'["poolInIx", "orderInIx", "rewardOutIx", "action"] redeemer'
  let
    poolNft      = getField @"poolNft"   config'
    requestorPkh = getField @"requestorPkh" config'
    exFee        = getField @"exFee" config'

    poolInIx     = getField @"poolInIx" redeemer
    rewardOutIx  = getField @"rewardOutIx" redeemer
    action       = getField @"action"   redeemer
  
  pure $
    pmatch action $ \case
        Refund ->
            let sigs = pfromData $ getField @"signatories" txInfo
                in containsSignature # sigs # requestorPkh
        Apply -> unTermCont $ do
            inputs    <- tletUnwrap $ getField @"inputs" txInfo
            outputs   <- tletUnwrap $ getField @"outputs" txInfo
            poolIn'   <- tlet $ pelemAt # poolInIx # inputs
            poolIn    <- pletFieldsC @'["outRef", "resolved"] poolIn'
            let 
                orderInIx = getField @"orderInIx" redeemer
                pool      = getField @"resolved"  poolIn

            selfIn'   <- tlet $ pelemAt # orderInIx # inputs
            selfIn    <- pletFieldsC @'["outRef", "resolved"] selfIn'
            selfValue <-
                    let self = pfromData $ getField @"resolved" selfIn
                    in tletField @"value" self

            inputAdaValue <- tlet $ assetClassValueOf # selfValue # pAdaAssetClass

            requestorOut      <- tlet $ pelemAt # rewardOutIx # outputs
            requestorOutValue <- tlet $ getRewardValue' # requestorOut # requestorPkh # pdnothing

            PSpending selfRef' <- pmatchC $ getField @"purpose" ctx

            wdrl     <- tletField @"wdrl" txInfo'
            let
                headWithdrawl = plookup # daoV1RoyaltyPoolCred # wdrl
                daoV1ContractIsInvoked = Maybe.pisJust # headWithdrawl

            poolValue      <- tletField @"value" pool
            let 
                poolIdentity = (assetClassValueOf # poolValue # poolNft) #== 1

                strictInputs =
                    let inputsLength = plength # inputs
                    in inputsLength #== 2

                selfIdentity =
                    let selfRef   = pfield @"_0" # selfRef'
                        selfInRef = getField @"outRef" selfIn
                    in selfRef #== selfInRef -- check that orderInIx points to the actual order

                adaOutputValue = assetClassValueOf # requestorOutValue # pAdaAssetClass

                correctOutputFinalValue = adaOutputValue #== (inputAdaValue - exFee)

            pure $ poolIdentity #&& strictInputs #&& selfIdentity #&& daoV1ContractIsInvoked #&& correctOutputFinalValue