{-# LANGUAGE TypeApplications #-}

module Hydra.Chain.Direct.State.Gen where

import Hydra.Cardano.Api
import Hydra.Prelude

import Hydra.Chain (
  HeadParameters (..),
  OnChainTx,
 )
import Hydra.Chain.Direct.State (
  HeadStateKind (..),
  ObserveTx(..),
  OnChainHeadState,
  close,
  collect,
  commit,
  idleOnChainHeadState,
  initialize,
 )
import Hydra.Ledger.Cardano (
  genOneUTxOFor,
  genTxIn,
  genVerificationKey,
 )
import Hydra.Party (Party)
import Test.QuickCheck (
  choose,
  elements,
  frequency,
  vector,
 )

--
-- Generators
--

-- Some 'global' (to all generator) context from which generators can pick
-- values for generation. This allows to write fairly independent generators
-- which however still make sense with one another within the context of a head.
--
-- For example, one can generate a head's _party_ from that global list, whereas
-- other functions may rely on all parties and thus, we need both generation to
-- be coherent.
data HydraContext = HydraContext
  { ctxVerificationKeys :: [VerificationKey PaymentKey]
  , ctxParties :: [Party]
  , ctxNetworkId :: NetworkId
  , ctxContestationPeriod :: DiffTime
  }
  deriving (Show)

ctxHeadParameters ::
  HydraContext ->
  HeadParameters
ctxHeadParameters HydraContext{ctxContestationPeriod, ctxParties} =
  HeadParameters ctxContestationPeriod ctxParties

-- TODO: allow bigger hydra heads than 3 parties
genHydraContext :: Gen HydraContext
genHydraContext = do
  n <- choose (1, 3)
  genHydraContextOf n

genHydraContextOf :: Int -> Gen HydraContext
genHydraContextOf n = do
  ctxVerificationKeys <- replicateM n genVerificationKey
  ctxParties <- vector n
  ctxNetworkId <- Testnet . NetworkMagic <$> arbitrary
  ctxContestationPeriod <- arbitrary
  pure $
    HydraContext
      { ctxVerificationKeys
      , ctxParties
      , ctxNetworkId
      , ctxContestationPeriod
      }

genStIdle ::
  HydraContext ->
  Gen (OnChainHeadState 'StIdle)
genStIdle HydraContext{ctxVerificationKeys, ctxNetworkId, ctxParties} = do
  ownParty <- elements ctxParties
  ownVerificationKey <- elements ctxVerificationKeys
  pure $ idleOnChainHeadState ctxNetworkId ownVerificationKey ownParty ctxVerificationKeys

genStInitialized ::
  HydraContext ->
  Gen (OnChainHeadState 'StInitialized)
genStInitialized ctx = do
  stIdle <- genStIdle ctx
  seedInput <- genTxIn
  let initTx = initialize (ctxHeadParameters ctx) (ctxVerificationKeys ctx) seedInput stIdle
  pure $ snd $ unsafeObserveTx @_ @'StInitialized initTx stIdle

genStOpen ::
  HydraContext ->
  Gen (OnChainHeadState 'StOpen)
genStOpen ctx = do
  initTx <- genInitTx ctx
  commits <- genCommits ctx initTx
  stInitialized <- executeCommits initTx commits <$> genStIdle ctx
  let collectComTx = collect stInitialized
  pure $ snd $ unsafeObserveTx @_ @'StOpen collectComTx stInitialized

genStClosed ::
  HydraContext ->
  Gen (OnChainHeadState 'StClosed)
genStClosed ctx = do
  stOpen <- genStOpen ctx
  snapshot <- arbitrary
  let closeTx = close snapshot stOpen
  pure $ snd $ unsafeObserveTx @_ @'StClosed closeTx stOpen

genInitTx ::
  HydraContext ->
  Gen Tx
genInitTx ctx =
  initialize (ctxHeadParameters ctx) (ctxVerificationKeys ctx)
    <$> genTxIn
    <*> genStIdle ctx

genCommits ::
  HydraContext ->
  Tx ->
  Gen [Tx]
genCommits ctx initTx = do
  forM (zip (ctxVerificationKeys ctx) (ctxParties ctx)) $ \(p, vk) -> do
    let stIdle = idleOnChainHeadState (ctxNetworkId ctx) p vk (ctxVerificationKeys ctx)
    let (_, stInitialized) = unsafeObserveTx @_ @'StInitialized initTx stIdle
    utxo <- genCommit
    pure $ unsafeCommit utxo stInitialized

genCommit :: Gen UTxO
genCommit =
  frequency
    [ (1, pure mempty)
    , (10, genVerificationKey >>= genOneUTxOFor)
    ]



--
-- Here be dragons
--

unsafeCommit ::
  HasCallStack =>
  UTxO ->
  OnChainHeadState 'StInitialized ->
  Tx
unsafeCommit u =
  either (error . show) id . commit u

unsafeObserveTx ::
  forall st st'. (ObserveTx st st', HasCallStack) =>
  Tx ->
  OnChainHeadState st ->
  (OnChainTx Tx, OnChainHeadState st')
unsafeObserveTx tx st =
  fromMaybe (error hopefullyInformativeMessage) (observeTx @st @st' tx st)
 where
  hopefullyInformativeMessage =
    "unsafeObserveTx:"
    <> "\n  From:\n    " <> show st
    <> "\n  Via:\n    " <> renderTx tx

executeCommits ::
  Tx ->
  [Tx] ->
  OnChainHeadState 'StIdle ->
  OnChainHeadState 'StInitialized
executeCommits initTx commits stIdle =
  flip execState stInitialized $ do
    forM_ commits $ \commitTx -> do
      st <- get
      let (_, st') = unsafeObserveTx @_ @'StInitialized commitTx st
      put st'
 where
  (_, stInitialized) = unsafeObserveTx @_ @'StInitialized initTx stIdle
