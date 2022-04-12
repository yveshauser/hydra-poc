{-# LANGUAGE UndecidableInstances #-}

-- | Aggregates all tracing messages in a single type.
--
-- This module provides a central point where top-level traced messages are
-- grouped. This is useful for traces consumers that will need to do something
-- specific depending on various tracing messages, eg. monitoring and metrics
-- collection.
module Hydra.Logging.Messages where

import Hydra.Prelude

import Hydra.API.Server (APIServerLog)
import Hydra.Chain (HasChainState)
import Hydra.Chain.Direct (DirectChainLog)
import Hydra.Ledger (IsTx, TxIdType, UTxOType)
import Hydra.Node (HydraNodeLog)

data HydraLog tx net
  = DirectChain {directChain :: DirectChainLog}
  | APIServer {api :: APIServerLog}
  | Network {network :: net}
  | Node {node :: HydraNodeLog tx}
  deriving (Generic)

deriving instance (IsTx tx, HasChainState tx, Eq net) => Eq (HydraLog tx net)
deriving instance (IsTx tx, HasChainState tx, Show net) => Show (HydraLog tx net)
deriving instance (IsTx tx, HasChainState tx, ToJSON net) => ToJSON (HydraLog tx net)

instance
  ( Arbitrary net
  , Arbitrary DirectChainLog
  , Arbitrary APIServerLog
  , IsTx tx
  , HasChainState tx
  ) =>
  Arbitrary (HydraLog tx net)
  where
  arbitrary = genericArbitrary
