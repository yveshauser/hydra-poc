{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

-- | Unit tests for our "hand-rolled" transactions as they are used in the
-- "direct" chain component.
module Hydra.Chain.Direct.TxSpec where

import Hydra.Prelude hiding (label)
import Test.Hydra.Prelude

import Hydra.Chain.Direct.Tx

import Cardano.Api (CtxUTxO, makeSignedTransaction)
import Cardano.Binary (serialize)
import Cardano.Ledger.Alonzo (TxOut)
import Cardano.Ledger.Alonzo.Data (Data (Data))
import Cardano.Ledger.Alonzo.Language (Language (PlutusV1))
import Cardano.Ledger.Alonzo.PParams (PParams, PParams' (..))
import Cardano.Ledger.Alonzo.Scripts (ExUnits (..), txscriptfee)
import Cardano.Ledger.Alonzo.Tools (BasicFailure, ScriptFailure, evaluateTransactionExecutionUnits)
import Cardano.Ledger.Alonzo.Tx (ValidatedTx (ValidatedTx, wits), txrdmrs)
import Cardano.Ledger.Alonzo.TxBody (TxOut (TxOut))
import Cardano.Ledger.Alonzo.TxWitness (RdmrPtr, unRedeemers)
import Cardano.Ledger.Crypto (StandardCrypto)
import Cardano.Ledger.Mary.Value (AssetName, PolicyID, Value (Value))
import Cardano.Ledger.Shelley.API (Coin (..), StrictMaybe (..), TxIn (..), UTxO (..))
import Cardano.Ledger.Slot (EpochSize (EpochSize))
import Cardano.Ledger.Val (inject)
import Cardano.Slotting.EpochInfo (fixedEpochInfo)
import Cardano.Slotting.Time (SystemStart (..), mkSlotLength)
import Data.Array (array)
import qualified Data.ByteString.Lazy as LBS
import Data.List (nub, (\\))
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Hydra.Chain (HeadParameters (..), OnChainTx (..))
import Hydra.Chain.Direct (toUnsignedTx)
import Hydra.Chain.Direct.Fixture (maxTxSize, pparams)
import Hydra.Chain.Direct.Util (Era)
import Hydra.Chain.Direct.Wallet (coverFee_)
import qualified Hydra.Contract.MockHead as MockHead
import qualified Hydra.Contract.MockInitial as MockInitial
import Hydra.Data.ContestationPeriod (contestationPeriodFromDiffTime)
import Hydra.Data.Party (partyFromVerKey)
import Hydra.Ledger (balance)
import Hydra.Ledger.Cardano (
  CardanoTx,
  LedgerCrypto,
  Lovelace (Lovelace),
  ScriptDataSupportedInEra (ScriptDataInAlonzoEra),
  TxBody (TxBody),
  TxOutDatum (TxOutDatum),
  Utxo,
  Utxo' (Utxo),
  describeCardanoTx,
  fromLedgerTx,
  fromPlutusData,
  genAdaOnlyUtxo,
  getTxFee,
  lovelaceToTxOutValue,
  lovelaceToValue,
  makeTransactionBody,
  mkScriptAddress,
  mkTxOutDatum,
  modifyValue,
  shelleyBasedEra,
  shrinkUtxo,
  toCtxUTxOTxOut,
  toLedgerUtxo,
  toMaryValue,
  toShelleyTxIn,
  toShelleyTxOut,
 )
import qualified Hydra.Ledger.Cardano as Api
import Hydra.Party (vkey)
import Ledger.Value (currencyMPSHash, unAssetClass)
import Plutus.V1.Ledger.Api (PubKeyHash, toData)
import Test.Cardano.Ledger.Alonzo.PlutusScripts (defaultCostModel)
import Test.Cardano.Ledger.Alonzo.Serialisation.Generators ()
import Test.QuickCheck (
  NonEmptyList (NonEmpty),
  Property,
  checkCoverage,
  counterexample,
  cover,
  elements,
  expectFailure,
  forAll,
  forAllShrinkBlind,
  forAllShrinkShow,
  label,
  property,
  (.&&.),
  (===),
 )
import Test.QuickCheck.Instances ()

spec :: Spec
spec =
  parallel $ do
    describe "initTx" $ do
      prop "transaction size is below limit" $ \txIn cperiod (party :| parties) cardanoKeys ->
        let params = HeadParameters cperiod (party : parties)
            Right tx = makeTransactionBody $ initTx cardanoKeys params txIn
            cbor = serialize $ makeSignedTransaction [] tx
            len = LBS.length cbor
         in len < maxTxSize
              & label (show (len `div` 1024) <> "kB")
              & counterexample ("Tx: " <> show tx)
              & counterexample ("Tx serialized size: " <> show len)

      prop "transaction fee is reasonable" $
        \(seedIn, seedOut) (walletIn, walletOut) cperiod (party :| parties) cardanoKeys ->
          let params = HeadParameters cperiod (party : parties)
              txDraft = initTx cardanoKeys params seedIn
              lookupUtxo = Utxo $ Map.singleton seedIn seedOut
              -- Prepare a wallet utxo which contains 100Ada
              walletUtxo =
                Map.singleton
                  (toShelleyTxIn walletIn)
                  (toShelleyTxOut shelleyBasedEra $ modifyValue (const $ lovelaceToValue $ Lovelace 100_000_000) walletOut)
           in case coverFee_ pparams lookupUtxo walletUtxo txDraft of
                Left err ->
                  False
                    & counterexample ("Wallet error: " <> show err)
                    & counterexample ("Wallet utxo: " <> show walletUtxo)
                Right (_, TxBody content) ->
                  let fee = getTxFee content
                   in fee < Lovelace 3_000_000
                        & label (show fee)
                        & counterexample ("Tx: " <> show content)
                        & counterexample ("Fee: " <> show fee)

      prop "is observed" $ \txIn cperiod (party :| parties) cardanoKeys ->
        let params = HeadParameters cperiod (party : parties)
            Right tx = makeTransactionBody $ initTx cardanoKeys params txIn
            observed = observeInitTx party tx
         in case observed of
              Just (octx, _) -> octx === OnInitTx @CardanoTx cperiod (party : parties)
              _ -> property False
              & counterexample ("Observed: " <> show observed)

      prop "is not observed if not invited" $ \txIn cperiod (NonEmpty parties) cardanoKeys ->
        forAll (elements parties) $ \notInvited ->
          let invited = nub parties \\ [notInvited]
              Right tx = makeTransactionBody $ initTx cardanoKeys (HeadParameters cperiod invited) txIn
           in isNothing (observeInitTx notInvited tx)
                & counterexample ("observing as: " <> show notInvited)
                & counterexample ("invited: " <> show invited)

      prop "updates on-chain state to 'Initial'" $ \txIn cperiod (me :| others) ->
        let params = HeadParameters cperiod parties
            parties = fst <$> me : others
            cardanoKeys = snd <$> me : others
            Right tx = makeTransactionBody $ initTx cardanoKeys params txIn
            res = observeInitTx (fst me) tx
         in case res of
              Just (OnInitTx cp ps, Initial{initials}) ->
                cp === cperiod
                  .&&. ps === parties
                  .&&. length initials === length cardanoKeys
              _ -> property False
              & counterexample ("Observe result: " <> show res)
              & counterexample ("Tx: " <> show tx)

    describe "commitTx" $ do
      prop "transaction size for single commit utxo below limit" $ \party singleUtxo initialIn ->
        let tx = commitTx party (Just singleUtxo) initialIn
            cbor = serialize tx
            len = LBS.length cbor
         in len < maxTxSize
              & label (show (len `div` 1024) <> "kB")
              & counterexample ("Tx: " <> show tx)
              & counterexample ("Tx serialized size: " <> show len)

      prop "is observed" $ \party singleUtxo initialIn ->
        let tx = commitTx party (Just singleUtxo) initialIn
         in observeCommitTx tx
              === Just OnCommitTx{party, committed = Utxo $ Map.fromList [singleUtxo]}
              & counterexample ("Tx: " <> show tx)

    describe "collectComTx" $ do
      prop "transaction size below limit" $ \utxo headIn cperiod parties ->
        let tx = collectComTx utxo (headIn, headDatum)
            headDatum = Data . toData $ MockHead.Initial cperiod parties
            cbor = serialize tx
            len = LBS.length cbor
         in len < maxTxSize
              & label (show (len `div` 1024) <> "kB")
              & counterexample ("Tx: " <> show tx)
              & counterexample ("Tx serialized size: " <> show len)

      prop "is observed" $ \committedUtxo headInput cperiod parties ->
        let headDatum = Data . toData $ MockHead.Initial cperiod parties
            headAddress = scriptAddr $ plutusScript $ MockHead.validatorScript policyId
            headValue = inject (Coin 2_000_000) <> toMaryValue (balance @CardanoTx committedUtxo)
            headOutput = TxOut headAddress headValue SNothing -- will be SJust, but not covered by this test
            lookupUtxo = Map.singleton headInput headOutput
            tx = collectComTx committedUtxo (headInput, headDatum)
            res = observeCollectComTx lookupUtxo tx
         in case res of
              Just (OnCollectComTx, OpenOrClosed{threadOutput = (_, TxOut _ headOutputValue' _, _)}) ->
                headOutputValue' === headValue
              _ -> property False
              & counterexample ("Observe result: " <> show res)
              & counterexample ("Tx: " <> show tx)

    describe "closeTx" $ do
      -- XXX(SN): tests are using a fixed snapshot number because of overlapping instances
      let sn = 1

      prop "transaction size below limit" $ \utxo headIn ->
        let tx = closeTx sn utxo (headIn, headDatum)
            headDatum = Data $ toData MockHead.Open
            cbor = serialize tx
            len = LBS.length cbor
         in len < maxTxSize
              & label (show (len `div` 1024) <> "kB")
              & counterexample ("Tx: " <> show tx)
              & counterexample ("Tx serialized size: " <> show len)

      prop "is observed" $ \utxo headInput ->
        let headDatum = Data $ toData MockHead.Open
            headAddress = scriptAddr $ plutusScript $ MockHead.validatorScript policyId
            headValue = inject (Coin 2_000_000)
            headOutput = TxOut headAddress headValue SNothing -- will be SJust, but not covered by this test
            lookupUtxo = Map.singleton headInput headOutput
            tx = closeTx sn utxo (headInput, headDatum)
            res = observeCloseTx lookupUtxo tx
         in case res of
              Just (OnCloseTx{snapshotNumber}, OpenOrClosed{}) -> snapshotNumber === sn
              _ -> property False
              & counterexample ("Observe result: " <> show res)
              & counterexample ("Tx: " <> show tx)

    describe "fanoutTx" $ do
      let prop_fanoutTxSize :: Utxo -> TxIn StandardCrypto -> Property
          prop_fanoutTxSize utxo headIn =
            let tx = fanoutTx utxo (headIn, headDatum)
                headDatum = Data $ toData MockHead.Closed
                cbor = serialize tx
                len = LBS.length cbor
             in len < maxTxSize
                  & label (show (len `div` 1024) <> "KB")
                  & label (prettyLength utxo <> " entries")
                  & counterexample (toString (describeCardanoTx $ fromLedgerTx tx))
                  & counterexample ("Tx serialized size: " <> show len)
           where
            prettyLength :: Foldable f => f a -> String
            prettyLength (length -> len)
              | len >= 100 = "> 100"
              | len >= 50 = "50-99"
              | len >= 10 = "10-49"
              | otherwise = "00-10"

      prop "size is below limit for small number of UTXO" $
        forAllShrinkShow genAdaOnlyUtxo shrinkUtxo (decodeUtf8 . encodePretty) $ \utxo ->
          forAll arbitrary $
            prop_fanoutTxSize utxo

      -- FIXME: This property currently fails even with a single UTXO if this
      -- UTXO is generated with too many values. We need to deal with it eventually
      -- (fanout splitting) or find a better property to capture what is
      -- actually 'expectable' from the function, given arbitrary UTXO entries.
      prop "size is above limit for UTXO" $
        forAllShrinkBlind arbitrary shrinkUtxo $ \utxo ->
          forAll arbitrary $
            expectFailure . prop_fanoutTxSize utxo

      prop "is observed" $ \utxo headInput ->
        let tx = fanoutTx utxo (headInput, headDatum)
            headDatum = Data $ toData MockHead.Closed
            headAddress = scriptAddr $ plutusScript $ MockHead.validatorScript policyId
            headValue = inject (Coin 2_000_000)
            headOutput = TxOut headAddress headValue SNothing -- will be SJust, but not covered by this test
            lookupUtxo = Map.singleton headInput headOutput
            res = observeFanoutTx lookupUtxo tx
         in res === Just (OnFanoutTx, Final)
              & counterexample ("Tx: " <> show tx)
              & counterexample ("Utxo map: " <> show lookupUtxo)

    describe "abortTx" $ do
      -- NOTE(AB): This property fails if initials are too big
      prop "transaction size below limit" $ \txIn cperiod parties (ReasonablySized initialsPkh) ->
        let headDatum = fromPlutusData . toData $ MockHead.Initial cperiod parties
            initials = Map.map (fromPlutusData . toData . MockInitial.datum) initialsPkh
         in case abortTx (txIn, headDatum) initials of
              Left err -> property False & counterexample ("AbortTx construction failed: " <> show err)
              Right txDraft ->
                case makeTransactionBody txDraft of
                  Left err -> property False & counterexample ("AbortTx construction failed: " <> show err)
                  Right txBody ->
                    let cbor = serialize $ makeSignedTransaction [] txBody
                        len = LBS.length cbor
                     in len < maxTxSize
                          & label (show (len `div` 1024) <> "kB")
                          & counterexample ("Tx: " <> show txBody)
                          & counterexample ("Tx serialized size: " <> show len)

      prop "updates on-chain state to 'Final'" $ \txIn cperiod parties (ReasonablySized initialsPkh) ->
        let txOut = TxOut headAddress headValue SNothing -- will be SJust, but not covered by this test
            headDatum = fromPlutusData . toData $ MockHead.Initial cperiod parties
            headAddress = scriptAddr $ plutusScript $ MockHead.validatorScript policyId
            headValue = inject (Coin 2_000_000)
            initials = Map.map (fromPlutusData . toData . MockInitial.datum) initialsPkh
         in -- XXX(SN): DRY and stair-casing
            case abortTx (txIn, headDatum) initials of
              Left err -> property False & counterexample ("AbortTx construction failed: " <> show err)
              Right txDraft ->
                case makeTransactionBody txDraft of
                  Left err -> property False & counterexample ("AbortTx construction failed: " <> show err)
                  Right txBody ->
                    let utxo = Map.singleton (toShelleyTxIn txIn) txOut
                        res = observeAbortTx utxo $ toUnsignedTx txBody
                     in case res of
                          Just (_, st) -> st === Final
                          _ ->
                            property False
                              & counterexample ("Result: " <> show res)
                              & counterexample ("Tx: " <> show txBody)

      -- TODO(SN): this requires the abortTx to include a redeemer, for a TxIn,
      -- spending a Head-validated output
      prop "validates against 'head' script in haskell (unlimited budget)" $
        \txIn HeadParameters{contestationPeriod, parties} (ReasonablySized initialsPkh) ->
          let headUtxo = (txIn, headOutput)
              headOutput = toCtxUTxOTxOut $ Api.TxOut headAddress headValue (TxOutDatum ScriptDataInAlonzoEra headDatum)
              (policy, _) = first currencyMPSHash (unAssetClass threadToken)
              headAddress = mkScriptAddress networkId $ Api.plutusScript $ MockHead.validatorScript policy
              headValue = lovelaceToTxOutValue 2_000_000
              headDatum =
                fromPlutusData . toData $
                  MockHead.Initial
                    (contestationPeriodFromDiffTime contestationPeriod)
                    (map (partyFromVerKey . vkey) parties)
              initials = Map.map (fromPlutusData . toData . MockInitial.datum) initialsPkh
              initialsUtxo = map (second mkMockInitialTxOut) $ Map.toList initialsPkh
              utxo = Utxo $ Map.fromList (headUtxo : initialsUtxo)
           in checkCoverage $
                case abortTx (txIn, headDatum) initials of
                  Left OverlappingInputs ->
                    property (isJust $ txIn `Map.lookup` initials)
                  Right txDraft ->
                    case makeTransactionBody txDraft of
                      Left err -> property False & counterexample ("AbortTx construction failed: " <> show err)
                      Right txBody ->
                        case validateTxScriptsUnlimited (toLedgerUtxo utxo) (toUnsignedTx txBody) of
                          Left basicFailure ->
                            property False & counterexample ("Basic failure: " <> show basicFailure)
                          Right redeemerReport ->
                            1 + length initials == length (rights $ Map.elems redeemerReport)
                              & counterexample ("Redeemer report: " <> show redeemerReport)
                              & counterexample ("Tx: " <> show txBody)
                              & counterexample ("Input utxo: " <> show utxo)
                              & cover 0.8 True "Success"

-- prop "cover fee correctly handles redeemers" $
--   withMaxSuccess 60 $ \txIn walletUtxo params cardanoKeys ->
--     -- TODO(SN): refactor to deconstruct using cardano-api types
--     let initContent@TxBodyContent{txOuts} = initTx cardanoKeys params txIn
--         Right initBody = makeTransactionBody initContent
--         -- Find head & initial utxos from initTx (using some partial functions & matches)
--         initTxId = getTxId initBody
--         headInput = TxIn (toShelleyTxId initTxId) 0
--         (headOutput : otherOutputs) = txOuts
--         headDatum = fromJust $ lookupDatum initTxWits headOutput
--         headUtxo = (headInput, headOutput)
--         initialOutputs = toList otherOutputs
--         initialDatums = mapMaybe (lookupDatum initTxWits) initialOutputs
--         initialUtxo = zipWith (\ix out -> (TxIn initTxId ix, out)) [1 ..] initialOutputs
--         initials = zipWith (\ix dat -> (TxIn initTxId ix, dat)) [1 ..] initialDatums
--         -- Finally we can create the abortTx and have it processed by the wallet
--         lookupUtxo = Map.fromList (headUtxo : initialUtxo)
--         utxo = UTxO $ walletUtxo <> lookupUtxo
--      in case abortTx (headInput, headDatum) (Map.fromList initials) of
--           Left err ->
--             property False & counterexample ("AbortTx construction failed: " <> show err)
--           Right txAbort ->
--             case coverFee_ pparams lookupUtxo walletUtxo txAbort of
--               Left err ->
--                 True
--                   & label
--                     ( case err of
--                         ErrNoAvailableUtxo -> "No available Utxo"
--                         ErrNotEnoughFunds{} -> "Not enough funds"
--                         ErrUnknownInput{} -> "Unknown input"
--                     )
--               Right (_, txAbortWithFees@ValidatedTx{body = abortTxBody}) ->
--                 let actualExecutionCost = executionCost pparams txAbortWithFees
--                  in actualExecutionCost > Coin 0 && txfee abortTxBody > actualExecutionCost
--                       & label "Ok"
--                       & counterexample ("Execution cost: " <> show actualExecutionCost)
--                       & counterexample ("Fee: " <> show (txfee abortTxBody))
--                       & counterexample ("Tx: " <> show txAbortWithFees)
--                       & counterexample ("Input utxo: " <> show utxo)

executionCost :: PParams Era -> ValidatedTx Era -> Coin
executionCost PParams{_prices} ValidatedTx{wits} =
  txscriptfee _prices executionUnits
 where
  executionUnits = foldMap snd $ unRedeemers $ txrdmrs wits

mkMockInitialTxOut :: PubKeyHash -> Api.TxOut CtxUTxO Api.Era
mkMockInitialTxOut pkh =
  toCtxUTxOTxOut $ Api.TxOut initialAddress initialValue initialDatum
 where
  initialAddress = mkScriptAddress networkId $ Api.plutusScript MockInitial.validatorScript
  initialValue = lovelaceToTxOutValue $ Lovelace 0
  initialDatum = mkTxOutDatum $ MockInitial.datum pkh

-- | Evaluate all plutus scripts and return execution budgets of a given
-- transaction (any included budgets are ignored).
validateTxScriptsUnlimited ::
  -- | Utxo set used to create context for any tx inputs.
  UTxO Era ->
  ValidatedTx Era ->
  Either (BasicFailure LedgerCrypto) (Map RdmrPtr (Either (ScriptFailure LedgerCrypto) ExUnits))
validateTxScriptsUnlimited utxo tx =
  runIdentity $ evaluateTransactionExecutionUnits pparams tx utxo epochInfo systemStart costmodels
 where
  -- REVIEW(SN): taken from 'testGlobals'
  epochInfo = fixedEpochInfo (EpochSize 100) (mkSlotLength 1)
  -- REVIEW(SN): taken from 'testGlobals'
  systemStart = SystemStart $ posixSecondsToUTCTime 0
  -- NOTE(SN): copied from Test.Cardano.Ledger.Alonzo.Tools as not exported
  costmodels = array (PlutusV1, PlutusV1) [(PlutusV1, fromJust defaultCostModel)]

-- | Extract NFT candidates. any single quantity assets not being ADA is a
-- candidate.
txOutNFT :: TxOut Era -> [(PolicyID StandardCrypto, AssetName)]
txOutNFT (TxOut _ value _) =
  mapMaybe findUnitAssets $ Map.toList assets
 where
  (Value _ assets) = value

  findUnitAssets (policy, as) = do
    (name, _q) <- find unitQuantity $ Map.toList as
    pure (policy, name)

  unitQuantity (_name, q) = q == 1
