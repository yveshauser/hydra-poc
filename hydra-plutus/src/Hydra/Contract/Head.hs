{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-specialize #-}

module Hydra.Contract.Head where

import PlutusTx.Prelude

import Hydra.Contract.Commit (SerializedTxOut (..))
import qualified Hydra.Contract.Commit as Commit
import Hydra.Contract.HeadState (Input (..), State (..))
import qualified Hydra.Contract.Initial as Initial
import Plutus.Extras (ValidatorType, scriptValidatorHash, wrapValidator)
import Plutus.V2.Ledger.Api (
  Address,
  Script,
  ToData (toBuiltinData),
  TxOut (..),
  Validator (getValidator),
  ValidatorHash,
  mkValidatorScript,
 )
import PlutusTx (CompiledCode)
import qualified PlutusTx
import qualified PlutusTx.Builtins as Builtins

-- REVIEW: Functions not re-exported "as V2", but using the same data types.
import Plutus.V1.Ledger.Address (scriptHashAddress)

-- * Custom ScriptContext

data TxInfo = TxInfo
  { -- | Transaction inputs
    txInfoInputs :: BuiltinData
  , -- | Transaction reference inputs
    txInfoReferenceInputs :: BuiltinData
  , -- | Transaction outputs
    txInfoOutputs :: [BuiltinData]
  , -- | The fee paid by this transaction.
    txInfoFee :: BuiltinData
  , -- | The 'Value' minted by this transaction.
    txInfoMint :: BuiltinData
  , -- | Digests of certificates included in this transaction
    txInfoDCert :: BuiltinData
  , -- | Withdrawals
    txInfoWdrl :: BuiltinData
  , -- | The valid range for the transaction.
    txInfoValidRange :: BuiltinData
  , -- | Signatures provided with the transaction, attested that they all signed the tx
    txInfoSignatories :: BuiltinData
  , txInfoRedeemers :: BuiltinData
  , txInfoData :: BuiltinData
  , -- | Hash of the pending transaction (excluding witnesses)
    txInfoId :: BuiltinData
  }

PlutusTx.makeIsDataIndexed ''TxInfo [('TxInfo, 0)]

data ScriptContext = ScriptContext {scriptContextTxInfo :: TxInfo, scriptContextPurpose :: BuiltinData}

PlutusTx.makeIsDataIndexed ''ScriptContext [('ScriptContext, 0)]

-- * Head validator

type DatumType = State
type RedeemerType = Input

hydraHeadV1 :: BuiltinByteString
hydraHeadV1 = "HydraHeadV1"

{-# INLINEABLE headValidator #-}
headValidator ::
  -- | Commit script address. NOTE: Used to identify inputs from commits and
  -- likely could be replaced by looking for PTs.
  Address ->
  -- | Inital script address. NOTE: Used to identify inputs from initials and
  -- likely could be replaced by looking for PTs.
  Address ->
  State ->
  Input ->
  ScriptContext ->
  Bool
headValidator _commitAddress _initialAddress oldState input context =
  case (oldState, input) of
    (Initial{}, CollectCom) ->
      True
    (Initial{}, Abort) ->
      True
    (Open{}, Close{}) ->
      True
    (Closed{}, Contest{}) ->
      True
    (Closed{utxoHash}, Fanout{numberOfFanoutOutputs}) ->
      checkFanout utxoHash numberOfFanoutOutputs context
    _ ->
      traceError "invalid head state transition"

checkFanout ::
  BuiltinByteString ->
  Integer ->
  ScriptContext ->
  Bool
checkFanout utxoHash numberOfFanoutOutputs ScriptContext{scriptContextTxInfo = txInfo} =
  traceIfFalse "fannedOutUtxoHash /= closedUtxoHash" $ fannedOutUtxoHash == utxoHash
 where
  fannedOutUtxoHash = hashTxOuts $ take numberOfFanoutOutputs txInfoOutputs
  TxInfo{txInfoOutputs} = txInfo
{-# INLINEABLE checkFanout #-}

(&) :: a -> (a -> b) -> b
(&) = flip ($)
{-# INLINEABLE (&) #-}

hashSerializedTxOuts :: [SerializedTxOut] -> BuiltinByteString
hashSerializedTxOuts =
  sha2_256 . Builtins.serialiseData . toBuiltinData
{-# INLINEABLE hashSerializedTxOuts #-}

hashTxOuts :: [BuiltinData] -> BuiltinByteString
hashTxOuts =
  sha2_256 . Builtins.serialiseData . toBuiltinData
{-# INLINEABLE hashTxOuts #-}

-- TODO: Use validatorHash directly in headValidator arguments
compiledValidator :: CompiledCode ValidatorType
compiledValidator =
  $$(PlutusTx.compile [||\ca ia -> wrap (headValidator ca ia)||])
    `PlutusTx.applyCode` PlutusTx.liftCode (scriptHashAddress Commit.validatorHash)
    `PlutusTx.applyCode` PlutusTx.liftCode (scriptHashAddress Initial.validatorHash)
 where
  wrap = wrapValidator @DatumType @RedeemerType

validatorScript :: Script
validatorScript = getValidator $ mkValidatorScript compiledValidator

validatorHash :: ValidatorHash
validatorHash = scriptValidatorHash validatorScript
