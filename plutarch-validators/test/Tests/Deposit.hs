module Tests.Deposit where

import qualified WhalePoolsDex.PContracts.PDeposit as PDeposit
import WhalePoolsDex.PValidators

import Eval
import Gen.Utils

import Hedgehog

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog as HH

import Gen.Models
import Gen.DepositGen
import Gen.PoolGen

import PlutusLedgerApi.V2
import PlutusLedgerApi.V1.Value

import Plutarch
import Plutarch.Prelude
import Plutarch.DataRepr
import Plutarch.Api.V2.Contexts
import Plutarch.Lift
import PExtra.Ada
import qualified PExtra.API as API

checkDeposit = testGroup "CheckDepositContract"
  [ HH.testProperty "deposit_is_correct" successDeposit
  , HH.testProperty "deposit_is_correct_x_is_ada" successDepositXIsAda
  , HH.testProperty "deposit_is_correct_y_is_ada" successDepositYIsAda
  ]

checkDepositRedeemer = testGroup "DepositRedeemer"
  [ HH.testProperty "fail_if_poolInIx_is_incorrect" incorrectPoolInIx
  , HH.testProperty "fail_if_orderInIx_is_incorrect" incorrectOrderInIx
  , HH.testProperty "fail_if_rewardOutIx_is_incorrect" incorrectRewardOutIx
  ]

checkDepositIdentity = testGroup "CheckDepositIdentity"
  [ HH.testProperty "fail_if_selfIdentity_is_incorrect" incorrectSelfIdentity
  , HH.testProperty "fail_if_poolIdentity_is_incorrect" incorrectPoolIdentity
  ]

checkDepositLq = testGroup "CheckDepositLq"
  [ HH.testProperty "fail_if_lq_is_less_than_min" lqIsLessThanMin
  , HH.testProperty "fail_if_lq_is_incorrect_token" lqOutTokenIsIncorrect
  ]

checkDepositTokenReward = testGroup "CheckDepositLq"
  [ HH.testProperty "fail_if_token_reward_is_incorrect" incorrectTokenReward
  ]

checkDepositChange = testGroup "CheckDepositChange"
  [ HH.testProperty "deposit_has_incorrect_change_with_x_is_ada"incorrectChangeWithAda
  ]
  
successDeposit :: Property
successDeposit = withTests 1 $ property $ do
  let (x, y, nft, lq) = genAssetClasses
  pkh             <- forAll genPkh
  treasuryAddress <- forAll genValidatorHash      
  treasuryAddress <- forAll genValidatorHash      
  orderTxRef      <- forAll genTxOutRef
  let
    (cfgData, dh) = genDConfig x y nft lq 2 pkh 1
    orderTxIn     = genTxIn orderTxRef dh x 10 y 10 5000000
    orderTxOut    = genTxOut dh lq 10 2482704 pkh
  
  poolTxRef <- forAll genTxOutRef
  let
    (pcfg, pdh) = genPConfig x y nft lq 9950 1 [] 0 treasuryAddress
    poolTxIn    = genPTxIn poolTxRef pdh x 1000 y 1000 lq 9223372036854774807 nft 1 5000000
    poolTxOut   = genPTxOut pdh x 1010 y 1010 lq 9223372036854774797 nft 1 3000000
  
  let
    txInfo  = mkTxInfo poolTxIn orderTxIn poolTxOut orderTxOut
    purpose = mkPurpose orderTxRef

    cxtToData          = toData $ mkContext txInfo purpose
    orderRedeemToData  = toData $ mkDepositRedeemer 0 1 1

    result = eraseRight $ evalWithArgs (wrapValidator PDeposit.depositValidatorT) [cfgData, orderRedeemToData, cxtToData]

  result === Right ()

successDepositXIsAda :: Property
successDepositXIsAda = withTests 1 $ property $ do
  let (_, y, nft, lq) = genAssetClasses
  pkh             <- forAll genPkh
  treasuryAddress <- forAll genValidatorHash      
  treasuryAddress <- forAll genValidatorHash      
  orderTxRef      <- forAll genTxOutRef
  let
    x             = mkAdaAssetClass 
    (cfgData, dh) = genDConfig x y nft lq 2 pkh 10000
    orderTxIn     = genTxIn orderTxRef dh x 10012 y 10 0
    orderTxOut    = genTxOut dh lq 10 10000 pkh
  
  poolTxRef <- forAll genTxOutRef
  let
    (pcfg, pdh) = genPConfig x y nft lq 9950 1 [] 0 treasuryAddress
    poolTxIn    = genPTxIn poolTxRef pdh x 1000 y 1000 lq 9223372036854774807 nft 1 0
    poolTxOut   = genPTxOut pdh x 1010 y 1010 lq 9223372036854774797 nft 1 0
  
  let
    txInfo  = mkTxInfo poolTxIn orderTxIn poolTxOut orderTxOut
    purpose = mkPurpose orderTxRef

    cxtToData          = toData $ mkContext txInfo purpose
    orderRedeemToData  = toData $ mkDepositRedeemer 0 1 1

    result = eraseRight $ evalWithArgs (wrapValidator PDeposit.depositValidatorT) [cfgData, orderRedeemToData, cxtToData]

  result === Right ()

successDepositYIsAda :: Property
successDepositYIsAda = withTests 1 $ property $ do
  let (x, _, nft, lq) = genAssetClasses
  pkh             <- forAll genPkh
  treasuryAddress <- forAll genValidatorHash      
  treasuryAddress <- forAll genValidatorHash      
  orderTxRef      <- forAll genTxOutRef
  let
    y             = mkAdaAssetClass 
    (cfgData, dh) = genDConfig x y nft lq 2 pkh 10000
    orderTxIn     = genTxIn orderTxRef dh x 10 y 10 10004
    orderTxOut    = genTxOut dh lq 10 10000 pkh
  
  poolTxRef <- forAll genTxOutRef
  let
    (pcfg, pdh) = genPConfig x y nft lq 9950 1 [] 0 treasuryAddress
    poolTxIn    = genPTxIn poolTxRef pdh x 10 y 10 lq 9223372036854775797 nft 1 0
    poolTxOut   = genPTxOut pdh x 20 y 20 lq 9223372036854775787 nft 1 0
  
  let
    txInfo  = mkTxInfo poolTxIn orderTxIn poolTxOut orderTxOut
    purpose = mkPurpose orderTxRef

    cxtToData          = toData $ mkContext txInfo purpose
    orderRedeemToData  = toData $ mkDepositRedeemer 0 1 1

    result = eraseRight $ evalWithArgs (wrapValidator PDeposit.depositValidatorT) [cfgData, orderRedeemToData, cxtToData]

  result === Right ()

incorrectChangeWithAda :: Property
incorrectChangeWithAda = withTests 1 $ property $ do
  let (_, y, nft, lq) = genAssetClasses
  pkh             <- forAll genPkh
  treasuryAddress <- forAll genValidatorHash      
  treasuryAddress <- forAll genValidatorHash      
  orderTxRef      <- forAll genTxOutRef
  let
    x             = mkAdaAssetClass 
    (cfgData, dh) = genDConfig x y nft lq 2 pkh 10000
    orderTxIn     = genTxIn orderTxRef dh x 10 y 10 10000
    orderTxOut    = genTxOut dh lq 10 10000 pkh
  
  poolTxRef <- forAll genTxOutRef
  let
    (pcfg, pdh) = genPConfig x y nft lq 9950 1 [] 0 treasuryAddress
    poolTxIn    = genPTxIn poolTxRef pdh x 10 y 10 lq 9223372036854775797 nft 1 10000
    poolTxOut   = genPTxOut pdh x 20 y 20 lq 9223372036854775787 nft 1 10000
  
  let
    txInfo  = mkTxInfo poolTxIn orderTxIn poolTxOut orderTxOut
    purpose = mkPurpose orderTxRef

    cxtToData          = toData $ mkContext txInfo purpose
    orderRedeemToData  = toData $ mkDepositRedeemer 0 1 1

    result = eraseBoth $ evalWithArgs (wrapValidator PDeposit.depositValidatorT) [cfgData, orderRedeemToData, cxtToData]

  result === Left ()

incorrectPoolInIx :: Property
incorrectPoolInIx = withTests 1 $ property $ do
  let (x, y, nft, lq) = genAssetClasses
  pkh             <- forAll genPkh
  treasuryAddress <- forAll genValidatorHash      
  treasuryAddress <- forAll genValidatorHash      
  orderTxRef      <- forAll genTxOutRef
  let
    (cfgData, dh) = genDConfig x y nft lq 2 pkh 1
    orderTxIn     = genTxIn orderTxRef dh x 10 y 10 5000000
    orderTxOut    = genTxOut dh lq 10 2482704 pkh
  
  poolTxRef <- forAll genTxOutRef
  let
    (pcfg, pdh) = genPConfig x y nft lq 9950 1 [] 0 treasuryAddress
    poolTxIn    = genPTxIn poolTxRef pdh x 10 y 10 lq 9223372036854775797 nft 1 5000000
    poolTxOut   = genPTxOut pdh x 20 y 20 lq 9223372036854775787 nft 1 3000000
  
  let
    txInfo  = mkTxInfo poolTxIn orderTxIn poolTxOut orderTxOut
    purpose = mkPurpose orderTxRef

    cxtToData          = toData $ mkContext txInfo purpose
    orderRedeemToData  = toData $ mkDepositRedeemer 1 1 1

    result = eraseBoth $ evalWithArgs (wrapValidator PDeposit.depositValidatorT) [cfgData, orderRedeemToData, cxtToData]

  result === Left ()

incorrectOrderInIx :: Property
incorrectOrderInIx = withTests 1 $ property $ do
  let (x, y, nft, lq) = genAssetClasses
  pkh             <- forAll genPkh
  treasuryAddress <- forAll genValidatorHash      
  treasuryAddress <- forAll genValidatorHash      
  orderTxRef      <- forAll genTxOutRef
  let
    (cfgData, dh) = genDConfig x y nft lq 2 pkh 1
    orderTxIn     = genTxIn orderTxRef dh x 10 y 10 5000000
    orderTxOut    = genTxOut dh lq 10 2482704 pkh
  
  poolTxRef <- forAll genTxOutRef
  let
    (pcfg, pdh) = genPConfig x y nft lq 9950 1 [] 0 treasuryAddress
    poolTxIn    = genPTxIn poolTxRef pdh x 10 y 10 lq 9223372036854775797 nft 1 5000000
    poolTxOut   = genPTxOut pdh x 20 y 20 lq 9223372036854775787 nft 1 3000000
  
  let
    txInfo  = mkTxInfo poolTxIn orderTxIn poolTxOut orderTxOut
    purpose = mkPurpose orderTxRef

    cxtToData          = toData $ mkContext txInfo purpose
    orderRedeemToData  = toData $ mkDepositRedeemer 0 0 1

    result = eraseBoth $ evalWithArgs (wrapValidator PDeposit.depositValidatorT) [cfgData, orderRedeemToData, cxtToData]

  result === Left ()

incorrectRewardOutIx :: Property
incorrectRewardOutIx = withTests 1 $ property $ do
  let (x, y, nft, lq) = genAssetClasses
  pkh             <- forAll genPkh
  treasuryAddress <- forAll genValidatorHash      
  treasuryAddress <- forAll genValidatorHash      
  orderTxRef      <- forAll genTxOutRef
  let
    (cfgData, dh) = genDConfig x y nft lq 2 pkh 1
    orderTxIn     = genTxIn orderTxRef dh x 10 y 10 5000000
    orderTxOut    = genTxOut dh lq 10 2482704 pkh
  
  poolTxRef <- forAll genTxOutRef
  let
    (pcfg, pdh) = genPConfig x y nft lq 9950 1 [] 0 treasuryAddress
    poolTxIn    = genPTxIn poolTxRef pdh x 10 y 10 lq 9223372036854775797 nft 1 5000000
    poolTxOut   = genPTxOut pdh x 20 y 20 lq 9223372036854775787 nft 1 3000000
  
  let
    txInfo  = mkTxInfo poolTxIn orderTxIn poolTxOut orderTxOut
    purpose = mkPurpose orderTxRef

    cxtToData          = toData $ mkContext txInfo purpose
    orderRedeemToData  = toData $ mkDepositRedeemer 0 1 0

    result = eraseBoth $ evalWithArgs (wrapValidator PDeposit.depositValidatorT) [cfgData, orderRedeemToData, cxtToData]

  result === Left ()

incorrectSelfIdentity :: Property
incorrectSelfIdentity = withTests 1 $ property $ do
  let (x, y, nft, lq) = genAssetClasses
  pkh             <- forAll genPkh
  treasuryAddress <- forAll genValidatorHash      
  treasuryAddress <- forAll genValidatorHash      
  orderTxRef      <- forAll genTxOutRef
  let
    (cfgData, dh) = genDConfig x y nft lq 2 pkh 1
    orderTxIn     = genTxIn orderTxRef dh x 10 y 10 5000000
    orderTxOut    = genTxOut dh lq 10 2482704 pkh
  
  poolTxRef <- forAll genTxOutRef
  let
    (pcfg, pdh) = genPConfig x y nft lq 9950 1 [] 0 treasuryAddress
    poolTxIn    = genPTxIn poolTxRef pdh x 10 y 10 lq 9223372036854775797 nft 1 5000000
    poolTxOut   = genPTxOut pdh x 20 y 20 lq 9223372036854775787 nft 1 3000000
  
  let
    txInfo  = mkTxInfo poolTxIn orderTxIn poolTxOut orderTxOut
    purpose = mkPurpose poolTxRef

    cxtToData          = toData $ mkContext txInfo purpose
    orderRedeemToData  = toData $ mkDepositRedeemer 0 1 1

    result = eraseBoth $ evalWithArgs (wrapValidator PDeposit.depositValidatorT) [cfgData, orderRedeemToData, cxtToData]

  result === Left ()

incorrectPoolIdentity :: Property
incorrectPoolIdentity = withTests 1 $ property $ do
  let (x, y, nft, lq) = genAssetClasses
  pkh             <- forAll genPkh
  treasuryAddress <- forAll genValidatorHash      
  treasuryAddress <- forAll genValidatorHash      
  orderTxRef      <- forAll genTxOutRef
  let
    (cfgData, dh) = genDConfig x y nft lq 2 pkh 1
    orderTxIn     = genTxIn orderTxRef dh x 10 y 10 5000000
    orderTxOut    = genTxOut dh lq 10 2482704 pkh
  
  poolTxRef <- forAll genTxOutRef
  let
    (pcfg, pdh) = genPConfig x y nft lq 9950 1 [] 0 treasuryAddress
    poolTxIn    = genPTxIn poolTxRef pdh x 10 y 10 lq 9223372036854775797 nft 2 5000000
    poolTxOut   = genPTxOut pdh x 20 y 20 lq 9223372036854775787 nft 1 3000000
  
  let
    txInfo  = mkTxInfo poolTxIn orderTxIn poolTxOut orderTxOut
    purpose = mkPurpose orderTxRef

    cxtToData          = toData $ mkContext txInfo purpose
    orderRedeemToData  = toData $ mkDepositRedeemer 0 1 1

    result = eraseBoth $ evalWithArgs (wrapValidator PDeposit.depositValidatorT) [cfgData, orderRedeemToData, cxtToData]

  result === Left ()

lqIsLessThanMin :: Property
lqIsLessThanMin = withTests 1 $ property $ do
  let (x, y, nft, lq) = genAssetClasses
  pkh             <- forAll genPkh
  treasuryAddress <- forAll genValidatorHash      
  treasuryAddress <- forAll genValidatorHash      
  orderTxRef      <- forAll genTxOutRef
  let
    (cfgData, dh) = genDConfig x y nft lq 2 pkh 1
    orderTxIn     = genTxIn orderTxRef dh x 10 y 10 5000000
    orderTxOut    = genTxOut dh lq 1 2482704 pkh
  
  poolTxRef <- forAll genTxOutRef
  let
    (pcfg, pdh) = genPConfig x y nft lq 9950 1 [] 0 treasuryAddress
    poolTxIn    = genPTxIn poolTxRef pdh x 10 y 10 lq 9223372036854775797 nft 1 5000000
    poolTxOut   = genPTxOut pdh x 20 y 20 lq 9223372036854775787 nft 1 3000000
  
  let
    txInfo  = mkTxInfo poolTxIn orderTxIn poolTxOut orderTxOut
    purpose = mkPurpose poolTxRef

    cxtToData          = toData $ mkContext txInfo purpose
    orderRedeemToData  = toData $ mkDepositRedeemer 0 1 1

    result = eraseBoth $ evalWithArgs (wrapValidator PDeposit.depositValidatorT) [cfgData, orderRedeemToData, cxtToData]

  result === Left ()

lqOutTokenIsIncorrect:: Property
lqOutTokenIsIncorrect = withTests 1 $ property $ do
  let (x, y, nft, lq) = genAssetClasses
  pkh             <- forAll genPkh
  treasuryAddress <- forAll genValidatorHash      
  treasuryAddress <- forAll genValidatorHash      
  orderTxRef      <- forAll genTxOutRef
  incorrectLq     <- forAll genAssetClass
  let
    (cfgData, dh) = genDConfig x y nft lq 2 pkh 1
    orderTxIn     = genTxIn orderTxRef dh x 10 y 10 5000000
    orderTxOut    = genTxOut dh incorrectLq 10 2482704 pkh
  
  poolTxRef <- forAll genTxOutRef
  let
    (pcfg, pdh) = genPConfig x y nft lq 9950 1 [] 0 treasuryAddress
    poolTxIn    = genPTxIn poolTxRef pdh x 10 y 10 lq 9223372036854775797 nft 1 5000000
    poolTxOut   = genPTxOut pdh x 20 y 20 lq 9223372036854775787 nft 1 3000000
  
  let
    txInfo  = mkTxInfo poolTxIn orderTxIn poolTxOut orderTxOut
    purpose = mkPurpose poolTxRef

    cxtToData          = toData $ mkContext txInfo purpose
    orderRedeemToData  = toData $ mkDepositRedeemer 0 1 1

    result = eraseBoth $ evalWithArgs (wrapValidator PDeposit.depositValidatorT) [cfgData, orderRedeemToData, cxtToData]

  result === Left ()

incorrectTokenReward :: Property
incorrectTokenReward = withTests 1 $ property $ do
  let (x, y, nft, lq) = genAssetClasses
  pkh             <- forAll genPkh
  treasuryAddress <- forAll genValidatorHash      
  treasuryAddress <- forAll genValidatorHash      
  orderTxRef      <- forAll genTxOutRef
  let
    (cfgData, dh) = genDConfig x y nft lq 2 pkh 1
    orderTxIn     = genTxIn orderTxRef dh x 100 y 10 5000000
    orderTxOut    = genTxOut dh lq 10 2482704 pkh
  
  poolTxRef <- forAll genTxOutRef
  let
    (pcfg, pdh) = genPConfig x y nft lq 9950 1 [] 0 treasuryAddress
    poolTxIn    = genPTxIn poolTxRef pdh x 10 y 10 lq 9223372036854775797 nft 1 5000000
    poolTxOut   = genPTxOut pdh x 110 y 20 lq 9223372036854775787 nft 1 3000000
  
  let
    txInfo  = mkTxInfo poolTxIn orderTxIn poolTxOut orderTxOut
    purpose = mkPurpose poolTxRef

    cxtToData          = toData $ mkContext txInfo purpose
    orderRedeemToData  = toData $ mkDepositRedeemer 0 1 1

    result = eraseBoth $ evalWithArgs (wrapValidator PDeposit.depositValidatorT) [cfgData, orderRedeemToData, cxtToData]

  result === Left ()