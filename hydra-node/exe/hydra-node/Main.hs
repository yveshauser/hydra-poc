{-# LANGUAGE DuplicateRecordFields #-}

module Main where

import Hydra.Prelude

import Hydra.API.Server (withAPIServer)
import Hydra.Chain (Chain, ChainCallback)
import Hydra.Chain.Direct (withDirectChain)
import Hydra.Chain.Direct.Util (readKeyPair, readVerificationKey)
import Hydra.HeadLogic (
  Environment (..),
  Event (..),
  HeadState (ReadyState),
  chainStateFromHeadState,
 )
import Hydra.Ledger.Cardano (Tx)
import qualified Hydra.Ledger.Cardano as Ledger
import Hydra.Ledger.Cardano.Configuration (
  newGlobals,
  newLedgerEnv,
  protocolParametersFromJson,
  readJsonFileThrow,
  shelleyGenesisFromJson,
 )
import Hydra.Logging (Tracer, Verbosity (..), withTracer)
import Hydra.Logging.Messages (HydraLog (..))
import Hydra.Logging.Monitoring (withMonitoring)
import Hydra.Network (Host (..))
import Hydra.Network.Heartbeat (withHeartbeat)
import Hydra.Network.Ouroboros (withIOManager, withOuroborosNetwork)
import Hydra.Node (
  EventQueue (..),
  HydraNode (..),
  createEventQueue,
  createHydraHead,
  initEnvironment,
  queryHeadState,
  runHydraNode,
 )
import Hydra.Options (ChainConfig (..), LedgerConfig (..), Options (..), parseHydraOptions)
import Hydra.Party (Party)

main :: IO ()
main = do
  o@Options{verbosity, host, port, peers, apiHost, apiPort, monitoringPort, chainConfig, ledgerConfig} <- identifyNode <$> parseHydraOptions
  env@Environment{party} <- initEnvironment o
  withTracer verbosity $ \tracer' ->
    withMonitoring monitoringPort tracer' $ \tracer -> do
      -- NOTE: Loading a head state from persistence could happen here.
      hh <- createHydraHead ReadyState
      eq <- createEventQueue
      withChain tracer party (chainCallback eq hh) chainConfig $ \oc ->
        withNetwork (contramap Network tracer) host port peers (putEvent eq . NetworkEvent) $ \hn ->
          withAPIServer apiHost apiPort party (contramap APIServer tracer) (putEvent eq . ClientEvent) $ \server -> do
            withCardanoLedger ledgerConfig $ \ledger -> do
              runHydraNode (contramap Node tracer) $
                HydraNode{eq, hh, ledger, hn, oc, server, env}
 where
  withNetwork tracer host port peers =
    let localhost = Host{hostname = show host, port}
     in withHeartbeat localhost $ withOuroborosNetwork tracer localhost peers

  withCardanoLedger ledgerConfig action = do
    globals <-
      newGlobals
        <$> readJsonFileThrow shelleyGenesisFromJson (cardanoLedgerGenesisFile ledgerConfig)

    ledgerEnv <-
      newLedgerEnv
        <$> readJsonFileThrow protocolParametersFromJson (cardanoLedgerProtocolParametersFile ledgerConfig)

    action (Ledger.cardanoLedger globals ledgerEnv)

  chainCallback eq hh cont = do
    headState <- atomically $ queryHeadState hh
    case chainStateFromHeadState headState >>= cont of
      Nothing -> pure ()
      Just onChainTx ->
        putEvent eq $ OnChainEvent{onChainTx}

withChain ::
  Tracer IO (HydraLog Tx net) ->
  Party ->
  ChainCallback Tx IO ->
  ChainConfig ->
  (Chain Tx IO -> IO ()) ->
  IO ()
withChain tracer party callback config action = do
  keyPair@(vk, _) <- readKeyPair cardanoSigningKey
  otherCardanoKeys <- mapM readVerificationKey cardanoVerificationKeys
  withIOManager $ \iocp -> do
    withDirectChain
      (contramap DirectChain tracer)
      networkId
      iocp
      nodeSocket
      keyPair
      party
      (vk : otherCardanoKeys)
      startChainFrom
      callback
      action
 where
  DirectChainConfig{networkId, nodeSocket, cardanoSigningKey, cardanoVerificationKeys, startChainFrom} = config

identifyNode :: Options -> Options
identifyNode opt@Options{verbosity = Verbose "HydraNode", nodeId} = opt{verbosity = Verbose $ "HydraNode-" <> show nodeId}
identifyNode opt = opt
