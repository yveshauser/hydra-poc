{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Unit tests for our "hand-rolled" transactions as they are used in the
-- "direct" chain component.
module Hydra.Chain.Direct.TxSpec where

import Hydra.Prelude hiding (label)
import Test.Hydra.Prelude

import qualified Data.List as List
import Hydra.Cardano.Api (TxIn)
import Hydra.Chain (HeadParameters (..))
import Hydra.Chain.Direct.Fixture (
  genForParty,
  testNetworkId,
 )
import Hydra.Chain.Direct.Tx (
  commitTx,
  initTx,
  observeCommitTx,
  observeInitTx,
 )
import qualified Hydra.Crypto as Hydra
import Hydra.Party (deriveParty)
import Test.Cardano.Ledger.Alonzo.Serialisation.Generators ()
import Test.QuickCheck (
  Property,
  property,
 )
import Test.QuickCheck.Instances ()

spec :: Spec
spec =
  parallel $ do
    prop "can walk through full life-cycle" prop_fullLifeCycle

prop_fullLifeCycle :: NominalDiffTime -> TxIn -> Property
prop_fullLifeCycle contestationPeriod seedInput =
  property . isJust $ do
    let txI = initTx networkId vkeys params seedInput
    oInit <- observeInitTx networkId (List.head parties) txI

    let aliceCommit = arbitrary `generateWith` 42
    txCommitAlice <- commitTx networkId (alice, aliceCardanoVk) (Just aliceCommit) oInit
    txCommitBob <- commitTx networkId (bob, bobCardanoVk) Nothing oInit

    oCommitA <- observeCommitTx networkId oInit txCommitAlice
    oCommitB <- observeCommitTx networkId oInit txCommitBob

    pure [oCommitA, oCommitB]
 where
  networkId = testNetworkId
  params = HeadParameters{contestationPeriod, parties}
  parties = [alice, bob]
  vkeys = [aliceCardanoVk, bobCardanoVk]
  aliceCardanoVk = arbitrary `genForParty` alice
  bobCardanoVk = arbitrary `genForParty` alice
  alice = deriveParty $ Hydra.generateSigningKey "alice"
  bob = deriveParty $ Hydra.generateSigningKey "bob"
