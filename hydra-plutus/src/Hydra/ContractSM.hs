{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-specialize #-}

-- | Simplified SM-based contract for the purpose of developing the interface
-- between Node and Chain
module Hydra.ContractSM where

import Hydra.Prelude hiding (State, find, fmap, foldMap, map, mapMaybe, mempty, pure, zip, ($), (&&), (+), (<$>), (<>), (==))
import PlutusTx.Prelude hiding (Eq, mempty)

import Control.Lens (makeClassyPrisms)
import qualified Data.Map as Map
import Hydra.Contract.ContestationPeriod (ContestationPeriod)
import Hydra.Contract.Party (Party)
import Ledger (CurrencySymbol, PubKeyHash (..), TxOut (txOutValue), TxOutTx (txOutTxOut), pubKeyAddress, pubKeyHash)
import Ledger.AddressMap (outputsMapFromTxForAddress)
import Ledger.Constraints (mustPayToPubKey)
import qualified Ledger.Typed.Scripts as Scripts
import Ledger.Typed.Tx (tyTxOutData, typeScriptTxOut)
import Ledger.Value (AssetClass, TokenName (..), assetClass, flattenValue, singleton)
import Plutus.Contract (
  AsContractError (..),
  Contract,
  ContractError (..),
  Empty,
  Endpoint,
  endpoint,
  logInfo,
  nextTransactionsAt,
  ownPubKey,
  tell,
 )
import Plutus.Contract.StateMachine (StateMachine, StateMachineClient)
import qualified Plutus.Contract.StateMachine as SM
import qualified Plutus.Contracts.Currency as Currency
import qualified PlutusTx
import Plutus.Contract.StateMachine.ThreadToken
import Hydra.Prelude (mempty)

data State
  = Initial ContestationPeriod [Party]
  | Open
  | Final
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

PlutusTx.unstableMakeIsData ''State

data Input
  = CollectCom
  | Abort
  deriving (Generic, Show)

PlutusTx.unstableMakeIsData ''Input

data HydraPlutusError
  = -- | State machine operation failed
    SMError SM.SMContractError
  | -- | Endpoint, coin selection, etc. failed
    PlutusError ContractError
  | -- | Thread token could not be created
    ThreadTokenError Currency.CurrencyError
  | -- | Arbitrary error
    HydraError String
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

makeClassyPrisms ''HydraPlutusError

instance AsContractError HydraPlutusError where
  _ContractError = _PlutusError

instance SM.AsSMContractError HydraPlutusError where
  _SMContractError = _SMError

instance Currency.AsCurrencyError HydraPlutusError where
  _CurrencyError = _ThreadTokenError

{-# INLINEABLE hydraStateMachine #-}
hydraStateMachine :: ThreadToken -> StateMachine State Input
hydraStateMachine threadToken =
  SM.mkStateMachine (Just threadToken) hydraTransition isFinal
 where
  isFinal Final{} = True
  isFinal _ = False

{-# INLINEABLE hydraTransition #-}
hydraTransition :: SM.State State -> Input -> Maybe (SM.TxConstraints SM.Void SM.Void, SM.State State)
hydraTransition oldState input =
  case (SM.stateData oldState, input) of
    (Initial{}, CollectCom) ->
      Just (mempty, oldState{SM.stateData = Open})
    _ -> Nothing

-- | The script instance of the auction state machine. It contains the state
-- machine compiled to a Plutus core validator script. The 'AssetClass' serves
-- two roles here:
--
--   1. Parameterizing the script, such that we get a unique address and allow
--   for multiple instances of it
--
--   2. Identify the 'state thread token', which should be passed in
--   transactions transitioning the state machine and provide "contract
--   continuity"
typedValidator :: ThreadToken -> Scripts.TypedValidator (StateMachine State Input)
typedValidator threadToken =
  let val =
        $$(PlutusTx.compile [||validatorParam||])
          `PlutusTx.applyCode` PlutusTx.liftCode threadToken
      validatorParam c = SM.mkValidator (hydraStateMachine c)
      wrap = Scripts.wrapValidator @State @Input
   in Scripts.mkTypedValidator @(StateMachine State Input)
        val
        $$(PlutusTx.compile [||wrap||])

-- | The machine client of the hydra state machine. It contains both, the script
-- instance with the on-chain code, and the Haskell definition of the state
-- machine for off-chain use.
machineClient ::
  -- | Thread token of the instance
  ThreadToken ->
  StateMachineClient State Input
machineClient threadToken =
  let machine = hydraStateMachine threadToken
      inst = typedValidator threadToken
   in SM.mkStateMachineClient (SM.StateMachineInstance machine inst)

participationTokenName :: PubKeyHash -> TokenName
participationTokenName = TokenName . getPubKeyHash

threadTokenName :: TokenName
threadTokenName = "thread token"

mkThreadToken :: CurrencySymbol -> AssetClass
mkThreadToken symbol =
  assetClass symbol threadTokenName

-- | Parameters for starting a head.
-- NOTE: kinda makes sense to have them separate because it couuld be the
-- case that the parties (hdyra nodes taking part in the head consensus) and th
-- participants (people commiting UTxOs) and posting transaction in the head
-- are different
data InitParams = InitParams
  { contestationPeriod :: ContestationPeriod
  , cardanoPubKeys :: [PubKeyHash]
  , hydraParties :: [Party]
  }
  deriving (Generic, Show, FromJSON, ToJSON)

setup :: Contract () (Endpoint "init" InitParams) HydraPlutusError ()
setup = do
  -- NOTE: These are the cardano/chain keys to send PTs to
  InitParams{contestationPeriod, cardanoPubKeys, hydraParties} <-
    endpoint @"init" @InitParams

  let tokens = map ((,1) . participationTokenName) cardanoPubKeys
  logInfo $ "Forging tokens: " <> show @String tokens
  ownPK <- pubKeyHash <$> ownPubKey
  symbol <- Currency.currencySymbol <$> Currency.mintContract ownPK tokens
  let tokenValues = map (uncurry (singleton symbol)) tokens
  logInfo $ "Done, PTs currency symbol: " <> show @String symbol

  threadToken <- SM.getThreadToken
  let client = machineClient threadToken
  let constraints = foldMap (uncurry mustPayToPubKey) $ zip cardanoPubKeys tokenValues
  void $ SM.runInitialiseWith mempty constraints client (Initial contestationPeriod hydraParties) mempty
  logInfo $ "Triggered Init " <> show @String cardanoPubKeys

-- | Parameters as they are available in the 'Initial' state.
data InitialParams = InitialParams
  { contestationPeriod :: ContestationPeriod
  , parties :: [Party]
  }
  deriving (Generic, Show, FromJSON, ToJSON)

instance Arbitrary InitialParams where
  shrink = genericShrink
  arbitrary = genericArbitrary

-- TODO(SN): This needs to be done differently: observe the full transaction in
-- which we got the PTs payed to us and decode the 'Initial' datum from there?
-- | Watch 'initialAddress' (with hard-coded parameters) and report all datums
-- seen on each run.
--
-- This needs to be safe! Because if you we identify the *wrong* contract, we
-- would be committing funds to a potential attacker.
watchInit :: Contract (Last InitialParams) Empty ContractError ()
watchInit = do
  logInfo @String $ "watchInit: Looking for an init tx and it's parties"
  pubKey <- ownPubKey
  let address = pubKeyAddress pubKey
      pkh = pubKeyHash pubKey
  forever $ do
    txs <- nextTransactionsAt address
    let tokenCandidates = txs >>= mapMaybe (findToken pkh) . Map.elems . outputsMapFromTxForAddress address
    logInfo $ "found token candidates: " <> show @String tokenCandidates
    case tokenCandidates of
      [token] -> do
        let datums = txs >>= rights . fmap (lookupDatum token) . Map.elems . outputsMapFromTxForAddress (scriptAddress token)
        logInfo @String $ "found init tx(s) with datums: " <> show datums
        case datums of
          [Initial contestationPeriod parties] ->
            tell . Last . Just $ InitialParams{contestationPeriod, parties}
          _ -> pure ()
      _ -> pure ()
 where
  -- Find candidates for a Hydra Head threadToken 'AssetClass', that is if the
  -- 'TokenName' matches our public key
  findToken :: PubKeyHash -> TxOutTx -> Maybe ThreadToken
  findToken pkh txout =
    let value = txOutValue $ txOutTxOut txout
        flat = flattenValue value
        mres = find (\(_, tokenName, amount) -> amount == 1 && tokenName == participationTokenName pkh) flat
     in case mres of
          Just (_symbol, _, _) -> Nothing -- Just $ mkThreadToken symbol
          Nothing -> Nothing

  scriptAddress = Scripts.validatorAddress . typedValidator

  lookupDatum token txOutTx = tyTxOutData <$> typeScriptTxOut (typedValidator token) txOutTx
