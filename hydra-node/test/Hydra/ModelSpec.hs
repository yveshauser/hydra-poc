{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Hydra.ModelSpec where

import Hydra.Cardano.Api
import Hydra.Prelude
import Test.Hydra.Prelude

-- This is completely safe
import Unsafe.Coerce (unsafeCoerce)

import qualified Cardano.Api.UTxO as UTxO
import Control.Monad.IOSim (Failure (FailureException), IOSim, runSimTrace, traceResult)
import Data.Map ((!))
import qualified Data.Map as Map
import qualified Data.Set as Set
import Hydra.BehaviorSpec (TestHydraNode (..))
import Hydra.Chain.Direct.Fixture (testNetworkId)
import Hydra.ClientInput (ClientInput (..))
import Hydra.Model (LocalState (..), Nodes (Nodes, nodes), OffChainState (..), WorldState (..))
import Hydra.Party (Party, deriveParty)
import Hydra.ServerOutput (ServerOutput (..))
import qualified Hydra.ServerOutput as ServerOutput
import Test.QuickCheck (Property, counterexample, property)
import Test.QuickCheck.Gen.Unsafe (Capture (Capture), capture)
import Test.QuickCheck.Monadic (PropertyM, assert, monadic', monitor, run)
import Test.QuickCheck.StateModel (Actions, runActions, pattern Actions)
import Test.Util (printTrace, traceInIOSim)
import qualified Prelude

spec :: Spec
spec =
  modifyMaxSuccess (const 1000) $
    prop "implementation respects model" prop_checkModel

prop_checkModel :: AnyActions -> Property
prop_checkModel (AnyActions actions) =
  property $
    runIOSimProp $
      monadic' $ do
        (WorldState{hydraParties, hydraState}, _symEnv) <- runActions actions
        run $ lift waitUntilTheEndOfTime
        let parties = Set.fromList $ deriveParty . fst <$> hydraParties
        nodes <- run $ gets nodes
        assert (parties == Map.keysSet nodes)
        forM_ parties $ \p -> do
          assertNodeSeesAndReportsAllExpectedCommits hydraState nodes p
          assertOpenHeadWithAllExpectedCommits hydraState nodes p

assertNodeSeesAndReportsAllExpectedCommits ::
  LocalState ->
  Map Party (TestHydraNode Tx (IOSim s)) ->
  Party ->
  PropertyM (StateT (Nodes (IOSim s)) (IOSim s)) ()
assertNodeSeesAndReportsAllExpectedCommits world nodes p = do
  let node = nodes ! p
  case world of
    Initial{commits} -> do
      outputs <- run $ lift $ serverOutputs @Tx node
      let expectedCommitted =
            fmap (\(sk, value) -> TxOut (mkVkAddress testNetworkId (getVerificationKey sk)) value TxOutDatumNone)
              <$> commits
      let actualCommitted =
            Map.fromList
              [ (party, Map.elems (UTxO.toMap utxo))
              | Committed{party, ServerOutput.utxo = utxo} <- outputs
              ]
      monitor $
        counterexample $
          toString $
            unlines
              [ "Actual committed: (" <> show p <> ") " <> show actualCommitted
              , "Expected committed: (" <> show p <> ") " <> show expectedCommitted
              ]
      assert (actualCommitted == expectedCommitted)
    _ -> do
      pure ()

assertOpenHeadWithAllExpectedCommits ::
  LocalState ->
  Map Party (TestHydraNode Tx (IOSim s)) ->
  Party ->
  PropertyM (StateT (Nodes (IOSim s)) (IOSim s)) ()
assertOpenHeadWithAllExpectedCommits world nodes p = do
  let node = nodes ! p
  case world of
    Open{offChainState = OffChainState{confirmedUTxO}} -> do
      utxo <- run $ getUTxO node
      let expectedBalance =
            Map.fromListWith
              (<>)
              [ (unwrapAddress addr, value)
              | (sk, value) <- confirmedUTxO
              , let addr = mkVkAddress testNetworkId (getVerificationKey sk)
              , valueToLovelace value /= Just 0
              ]
      let actualBalance =
            Map.fromListWith (<>) $
              [ (unwrapAddress addr, value)
              | (TxOut addr value _) <- Map.elems (UTxO.toMap utxo)
              , valueToLovelace value /= Just 0
              ]
      monitor $
        counterexample $
          toString $
            unlines
              [ "Actual balance: (" <> show p <> ") " <> show actualBalance
              , "Expected balance: (" <> show p <> ") " <> show expectedBalance
              , "Difference: (" <> show p <> ") " <> show (Map.difference actualBalance expectedBalance)
              ]
      assert (expectedBalance == actualBalance)
    _ -> do
      pure ()
 where
  getUTxO node = lift $ do
    node `send` GetUTxO
    let loop =
          waitForNext node >>= \case
            GetUTxOResponse u -> pure u
            _ -> loop
    loop

  unwrapAddress = \case
    ShelleyAddressInEra addr -> serialiseToBech32 addr
    ByronAddressInEra{} -> error "Byron."

-- NOTE: This is only sound to run in IOSim, because delays are instant. It
-- allows to make sure we wait long-enough for remaining asynchronous actions /
-- events to complete before we make any test assertion.
waitUntilTheEndOfTime :: MonadDelay m => m ()
waitUntilTheEndOfTime = threadDelay 1000000000000

--

-- * Utilities for `IOSim`

--

-- | Specialised runner similar to <runSTGen https://hackage.haskell.org/package/QuickCheck-2.14.2/docs/src/Test.QuickCheck.Monadic.html#runSTGen>.
runIOSimProp :: (forall s. Gen (StateT (Nodes (IOSim s)) (IOSim s) Property)) -> Gen Property
runIOSimProp p = do
  Capture eval <- capture
  let tr = runSimTrace $ evalStateT (eval p) (Nodes mempty traceInIOSim)
      traceDump = printTrace (Proxy :: Proxy Tx) tr
  case traceResult False tr of
    Right x ->
      pure x
    Left (FailureException (SomeException ex)) -> do
      pure $ counterexample (show ex <> "\ntrace:\n" <> toString traceDump) $ property False
    Left ex ->
      pure $ counterexample (show ex <> "\ntrace:\n" <> toString traceDump) $ property False

newtype AnyActions = AnyActions {unAnyActions :: forall s. Actions (WorldState (IOSim s))}

instance Show AnyActions where
  show (AnyActions acts) = Prelude.show (acts @())

instance Arbitrary AnyActions where
  arbitrary = do
    Capture eval <- capture
    return (AnyActions (eval arbitrary))

  shrink (AnyActions actions) = case actions of
    Actions [] -> []
    acts -> [AnyActions (unsafeCoerce act) | act <- shrink acts]
