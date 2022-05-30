-- | Mutation-based script validator tests for the init transaction where a
-- 'healthyInitTx' gets mutated by an arbitrary 'InitMutation'.
module Hydra.Chain.Direct.Contract.Init where

import Hydra.Cardano.Api
import Hydra.Prelude

import qualified Cardano.Api.UTxO as UTxO
import Data.List ((\\))
import Hydra.Chain (HeadParameters (..))
import Hydra.Chain.Direct.Contract.Mutation (
  Mutation (..),
  SomeMutation (..),
  addPTWithQuantity,
  changeMintedValueQuantityFrom,
 )
import Hydra.Chain.Direct.Fixture (genForParty, testNetworkId)
import Hydra.Chain.Direct.State (HeadStateKind (..), OnChainHeadState, idleOnChainHeadState)
import Hydra.Chain.Direct.Tx (hydraHeadV1AssetName, initTx)
import Hydra.Ledger.Cardano (genOneUTxOFor, genValue, genVerificationKey)
import Hydra.Party (Party)
import Test.QuickCheck (choose, elements, oneof, suchThat, vectorOf)
import qualified Prelude

--
-- InitTx
--

healthyInitTx :: (Tx, UTxO)
healthyInitTx =
  (tx, healthyLookupUTxO)
 where
  tx =
    initTx
      testNetworkId
      healthyCardanoKeys
      healthyHeadParameters
      healthySeedInput

healthyHeadParameters :: HeadParameters
healthyHeadParameters =
  flip generateWith 42 $
    HeadParameters
      <$> arbitrary
      <*> vectorOf (length healthyParties) arbitrary

healthySeedInput :: TxIn
healthySeedInput =
  fst . Prelude.head $ UTxO.pairs healthyLookupUTxO

healthyParties :: [Party]
healthyParties =
  generateWith (vectorOf 3 arbitrary) 42

healthyCardanoKeys :: [VerificationKey PaymentKey]
healthyCardanoKeys =
  genForParty genVerificationKey <$> healthyParties

healthyLookupUTxO :: UTxO
healthyLookupUTxO =
  generateWith (genOneUTxOFor (Prelude.head healthyCardanoKeys)) 42

genHealthyIdleSt :: Gen (OnChainHeadState 'StIdle)
genHealthyIdleSt = do
  party <- elements healthyParties
  let vk = genVerificationKey `genForParty` party
  pure $ idleOnChainHeadState testNetworkId (healthyCardanoKeys \\ [vk]) vk party

data InitMutation
  = MutateThreadTokenQuantity
  | MutateAddAnotherPT
  | MutateDropInitialOutput
  | MutateDropSeedInput
  | MutateInitialOutputValue
  deriving (Generic, Show, Enum, Bounded)

data ObserveInitMutation
  = MutateSomePT
  deriving (Generic, Show, Enum, Bounded)

genInitMutation :: (Tx, UTxO) -> Gen SomeMutation
genInitMutation (tx, _utxo) =
  oneof
    [ SomeMutation MutateThreadTokenQuantity <$> changeMintedValueQuantityFrom tx 1
    , SomeMutation MutateAddAnotherPT <$> addPTWithQuantity tx 1
    , SomeMutation MutateInitialOutputValue <$> do
        let outs = txOuts' tx
        (ix :: Int, out) <- elements (drop 1 $ zip [0 ..] outs)
        value' <- genValue `suchThat` (/= txOutValue out)
        pure $ ChangeOutput (fromIntegral ix) (modifyTxOutValue (const value') out)
    , SomeMutation MutateDropInitialOutput <$> do
        ix <- choose (1, length (txOuts' tx) - 1)
        pure $ RemoveOutput (fromIntegral ix)
    , SomeMutation MutateDropSeedInput <$> do
        pure $ RemoveInput healthySeedInput
    ]
