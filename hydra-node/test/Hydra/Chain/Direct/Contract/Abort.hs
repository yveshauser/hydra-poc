{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications #-}

-- | Mutation-based script validator tests for the abort transaction where a
-- 'healthyAbortTx' gets mutated by an arbitrary 'AbortMutation'.
module Hydra.Chain.Direct.Contract.Abort where

import Hydra.Cardano.Api

import qualified Cardano.Api.UTxO as UTxO
import Data.List (intersectBy)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Hydra.Chain (HeadParameters (..))
import Hydra.Chain.Direct.Contract.Mutation (
  Mutation (..),
  SomeMutation (..),
  addPTWithQuantity,
  anyPayToPubKeyTxOut,
  changeMintedValueQuantityFrom,
  headTxIn,
 )
import Hydra.Chain.Direct.Fixture (genForParty, testNetworkId, testPolicyId, testSeedInput)
import Hydra.Chain.Direct.Tx (InitObservation (..), UTxOWithScript, abortTx, assetNameFromVerificationKey, headValue, mkCommitDatum, mkHeadOutputInitial, mkHeadTokenScript)
import qualified Hydra.Contract.Commit as Commit
import qualified Hydra.Contract.Head as Head
import qualified Hydra.Contract.HeadState as Head
import qualified Hydra.Contract.Initial as Initial
import Hydra.Ledger.Cardano (adaOnly, genOneUTxOFor, genVerificationKey)
import Hydra.Party (Party, partyToChain)
import Hydra.Prelude
import Plutus.V1.Ledger.Api (toData)
import Test.QuickCheck (Property, choose, counterexample, elements, oneof, suchThat, vectorOf)

--
-- AbortTx
--

healthyAbortTx :: HasCallStack => (Tx, UTxO)
healthyAbortTx =
  (tx, lookupUTxO)
 where
  lookupUTxO =
    UTxO.singleton (headInput, toUTxOContext headOutput)
      <> UTxO (Map.fromList (drop3rd <$> healthyInitials))
      <> UTxO (Map.fromList (drop3rd <$> healthyCommits))

  drop3rd (a, b, _) = (a, b)

  tx =
    either (error . show) id $
      abortTx
        somePartyCardanoVerificationKey
        initObservation

  initObservation =
    InitObservation
      { threadOutput = (headInput, toUTxOContext headOutput, headDatum)
      , initials = healthyInitials
      , commits = healthyCommits
      , headId = arbitrary `generateWith` 42
      , headTokenScript = headTokenScript -- TODO: get rid of this / compute from headId
      , parties = undefined -- TODO: not needed for commits
      , contestationPeriod = undefined -- TODO: not needed for commits
      }

  somePartyCardanoVerificationKey = flip generateWith 42 $ do
    genForParty genVerificationKey <$> elements healthyParties

  headInput = generateWith arbitrary 42

  headTokenScript = mkHeadTokenScript testSeedInput

  headOutput = mkHeadOutputInitial testNetworkId testPolicyId headParameters

  headParameters =
    HeadParameters
      { contestationPeriod = 10
      , parties = healthyParties
      }

  headDatum = unsafeGetDatum headOutput

  -- XXX: We loose type information by dealing with 'TxOut CtxTx' where datums
  -- are optional
  unsafeGetDatum = fromJust . getScriptData

healthyInitials :: [UTxOWithScript]
healthyCommits :: [UTxOWithScript]
(healthyInitials, healthyCommits) =
  -- TODO: Refactor this to be an AbortTx generator because we actually want
  -- to test healthy abort txs with varied combinations of inital and commit
  -- outputs
  generateWith (genAbortableOutputs healthyParties `suchThat` thereIsAtLeastOneCommit) 42

genAbortableOutputs :: [Party] -> Gen ([UTxOWithScript], [UTxOWithScript])
genAbortableOutputs parties =
  go `suchThat` notConflict
 where
  go = do
    (initParties, commitParties) <- (`splitAt` parties) <$> choose (0, length parties)
    initials <- mapM genInitial initParties
    commits <- fmap (\(a, (b, c)) -> (a, b, c)) . Map.toList <$> generateCommitUTxOs commitParties
    pure (initials, commits)

  notConflict (is, cs) =
    null $ intersectBy (\(i, _, _) (c, _, _) -> i == c) is cs

  genInitial p =
    mkInitial (genVerificationKey `genForParty` p) <$> arbitrary

  mkInitial ::
    VerificationKey PaymentKey ->
    TxIn ->
    UTxOWithScript
  mkInitial vk txin =
    ( txin
    , initialTxOut vk
    , fromPlutusData (toData initialDatum)
    )

  initialTxOut :: VerificationKey PaymentKey -> TxOut CtxUTxO
  initialTxOut vk =
    toUTxOContext $
      TxOut
        (mkScriptAddress @PlutusScriptV1 testNetworkId initialScript)
        ( headValue
            <> valueFromList
              [ (AssetId testPolicyId (assetNameFromVerificationKey vk), 1)
              ]
        )
        (mkTxOutDatum initialDatum)

  initialScript = fromPlutusScript Initial.validatorScript

  initialDatum = Initial.datum ()

-- | Generate a UTXO representing /commit/ outputs for a given list of `Party`.
-- FIXME: This function is very complicated and it's hard to understand it after a while
generateCommitUTxOs :: [Party] -> Gen (Map.Map TxIn (TxOut CtxUTxO, ScriptData))
generateCommitUTxOs parties = do
  txins <- vectorOf (length parties) (arbitrary @TxIn)
  let vks = (\p -> (genVerificationKey `genForParty` p, p)) <$> parties
  committedUTxO <-
    vectorOf (length parties) $
      oneof
        [ do
            singleUTxO <- fmap adaOnly <$> (genOneUTxOFor =<< arbitrary)
            pure $ head <$> nonEmpty (UTxO.pairs singleUTxO)
        , pure Nothing
        ]
  let commitUTxO =
        zip txins $
          uncurry mkCommitUTxO <$> zip vks committedUTxO
  pure $ Map.fromList commitUTxO
 where
  mkCommitUTxO :: (VerificationKey PaymentKey, Party) -> Maybe (TxIn, TxOut CtxUTxO) -> (TxOut CtxUTxO, ScriptData)
  mkCommitUTxO (vk, party) utxo =
    ( toUTxOContext $
        TxOut
          (mkScriptAddress @PlutusScriptV1 testNetworkId commitScript)
          commitValue
          (mkTxOutDatum commitDatum)
    , fromPlutusData (toData commitDatum)
    )
   where
    commitValue =
      mconcat
        [ lovelaceToValue (Lovelace 2000000)
        , maybe mempty (txOutValue . snd) utxo
        , valueFromList
            [ (AssetId testPolicyId (assetNameFromVerificationKey vk), 1)
            ]
        ]
    commitScript = fromPlutusScript Commit.validatorScript
    commitDatum = mkCommitDatum party Head.validatorHash utxo

thereIsAtLeastOneCommit :: ([UTxOWithScript], [UTxOWithScript]) -> Bool
thereIsAtLeastOneCommit (is, cs) = not (null cs) && not (null is)

healthyParties :: [Party]
healthyParties =
  [ generateWith arbitrary i | i <- [1 .. 3]
  ]

propHasInitial :: (Tx, UTxO) -> Property
propHasInitial (_, utxo) =
  any paysToInitialScript utxo
    & counterexample ("UTxO: " <> decodeUtf8 (encodePretty utxo))
    & counterexample ("Looking for Initial Script: " <> show addr)
 where
  addr = mkScriptAddress @PlutusScriptV1 testNetworkId (fromPlutusScript Initial.validatorScript)
  paysToInitialScript txOut =
    txOutAddress txOut == addr

propHasCommit :: (Tx, UTxO) -> Property
propHasCommit (_, utxo) =
  any paysToCommitScript utxo
    & counterexample ("UTxO: " <> decodeUtf8 (encodePretty utxo))
    & counterexample ("Looking for Commit Script: " <> show addr)
 where
  addr = mkScriptAddress @PlutusScriptV1 testNetworkId (fromPlutusScript Commit.validatorScript)
  paysToCommitScript txOut =
    txOutAddress txOut == addr

data AbortMutation
  = MutateParties
  | DropOneCommitOutput
  | MutateHeadScriptInput
  | BurnOneTokenMore
  | MutateThreadTokenQuantity
  | DropCollectedInput
  | MutateRequiredSigner
  deriving (Generic, Show, Enum, Bounded)

genAbortMutation :: (Tx, UTxO) -> Gen SomeMutation
genAbortMutation (tx, utxo) =
  oneof
    [ SomeMutation MutateParties . ChangeHeadDatum <$> do
        moreParties <- (: healthyParties) <$> arbitrary
        c <- arbitrary
        pure $ Head.Initial c (partyToChain <$> moreParties)
    , SomeMutation DropOneCommitOutput
        . RemoveOutput
        <$> choose (0, fromIntegral (length (txOuts' tx) - 1))
    , SomeMutation MutateHeadScriptInput <$> (ChangeInput (headTxIn utxo) <$> anyPayToPubKeyTxOut <*> pure Nothing)
    , SomeMutation MutateThreadTokenQuantity <$> changeMintedValueQuantityFrom tx (-1)
    , SomeMutation BurnOneTokenMore <$> addPTWithQuantity tx (-1)
    , SomeMutation DropCollectedInput . RemoveInput <$> elements (txIns' tx)
    , SomeMutation MutateRequiredSigner <$> do
        newSigner <- verificationKeyHash <$> genVerificationKey
        pure $ ChangeRequiredSigners [newSigner]
    ]
