{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE TypeFamilies        #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

-- | This module defines methods which operate on GtLocalData.

module Pos.Ssc.GodTossing.LocalData.LocalData
       (
         localOnNewSlot
       , sscIsDataUseful
       , sscProcessMessage
         -- * Instances
         -- ** instance SscLocalDataClass SscGodTossing
       ) where

import           Control.Lens                       (at, use, view, (%=), (.=))
import           Control.Lens                       (Getter)
import           Control.Monad.Loops                (andM)
import           Data.Containers                    (ContainerKey,
                                                     SetContainer (notMember))
import           Data.Default                       (Default (def))
import qualified Data.HashMap.Strict                as HM
import qualified Data.HashSet                       as HS
import           Serokell.Util.Verify               (isVerSuccess)
import           Universum

import           Pos.Crypto                         (PublicKey, Share)
import           Pos.Ssc.Class.LocalData            (LocalQuery, LocalUpdate, MonadSscLD,
                                                     SscLocalDataClass (..),
                                                     sscRunLocalQuery, sscRunLocalUpdate)
import           Pos.Ssc.GodTossing.Functions       (checkOpeningMatchesCommitment,
                                                     checkShares, inLastKSlotsId,
                                                     isCommitmentIdx, isOpeningIdx,
                                                     isSharesIdx, verifySignedCommitment)
import           Pos.Ssc.GodTossing.LocalData.Types (GtLocalData, gtGlobalCertificates,
                                                     gtGlobalCommitments,
                                                     gtGlobalOpenings, gtGlobalShares,
                                                     gtLastProcessedSlot,
                                                     gtLocalCertificates,
                                                     gtLocalCommitments, gtLocalOpenings,
                                                     gtLocalShares)
import           Pos.Ssc.GodTossing.Types.Base      (Commitment, CommitmentSignature,
                                                     Opening, VssCertificate)
import           Pos.Ssc.GodTossing.Types.Instance  ()
import           Pos.Ssc.GodTossing.Types.Message   (DataMsg (..), MsgTag (..))
import           Pos.Ssc.GodTossing.Types.Type      (SscGodTossing)
import           Pos.Ssc.GodTossing.Types.Types     (GtGlobalState (..), GtPayload (..))
import           Pos.Types                          (SlotId (..))
import           Pos.Util                           (diffDoubleMap, getKeys,
                                                     readerToState)

type LDQuery a = LocalQuery SscGodTossing a
type LDUpdate a = LocalUpdate SscGodTossing a

instance SscLocalDataClass SscGodTossing where
    sscEmptyLocalData = def
    sscGetLocalPayloadQ = getLocalPayload
    sscApplyGlobalStateU = applyGlobal

----------------------------------------------------------------------------
-- Process New Slot
----------------------------------------------------------------------------
clearGlobalState :: LDUpdate ()
clearGlobalState = do
    gtGlobalCommitments  .= mempty
    gtGlobalOpenings     .= mempty
    gtGlobalShares       .= mempty

-- | Clean-up some data when new slot starts.
localOnNewSlot
    :: MonadSscLD SscGodTossing m
    => SlotId -> m ()
localOnNewSlot = sscRunLocalUpdate . localOnNewSlotU

localOnNewSlotU :: SlotId -> LDUpdate ()
localOnNewSlotU si@SlotId {siSlot = slotIdx} = do
    when (slotIdx == 0) clearGlobalState
    unless (isCommitmentIdx slotIdx) $ gtLocalCommitments .= mempty
    unless (isOpeningIdx slotIdx) $ gtLocalOpenings .= mempty
    unless (isSharesIdx slotIdx) $ gtLocalShares .= mempty
    gtLastProcessedSlot .= si

----------------------------------------------------------------------------
-- Check knowledge of data
----------------------------------------------------------------------------

-- | Check whether SSC data with given tag and public key can be added
-- to local data.
sscIsDataUseful
    :: MonadSscLD SscGodTossing m
    => MsgTag -> PublicKey -> m Bool
sscIsDataUseful tag = sscRunLocalQuery . sscIsDataUsefulQ tag

sscIsDataUsefulQ :: MsgTag -> PublicKey -> LDQuery Bool
sscIsDataUsefulQ CommitmentMsg =
    sscIsDataUsefulImpl gtLocalCommitments gtGlobalCommitments
sscIsDataUsefulQ OpeningMsg =
    sscIsDataUsefulSetImpl gtLocalOpenings gtGlobalOpenings
sscIsDataUsefulQ SharesMsg =
    sscIsDataUsefulSetImpl gtLocalShares gtGlobalShares
sscIsDataUsefulQ VssCertificateMsg =
    sscIsDataUsefulImpl gtLocalCertificates gtGlobalCertificates

type MapGetter a = Getter GtLocalData (HashMap PublicKey a)
type SetGetter set = Getter GtLocalData set

sscIsDataUsefulImpl :: MapGetter a -> MapGetter a -> PublicKey -> LDQuery Bool
sscIsDataUsefulImpl localG globalG pk =
    (&&) <$>
        (notMember pk <$> view globalG) <*>
        (notMember pk <$> view localG)

sscIsDataUsefulSetImpl :: (SetContainer set, ContainerKey set ~ PublicKey)
                       => MapGetter a -> SetGetter set -> PublicKey -> LDQuery Bool
sscIsDataUsefulSetImpl localG globalG pk =
    (&&) <$>
        (notMember pk <$> view localG) <*>
        (notMember pk <$> view globalG)

----------------------------------------------------------------------------
-- Ssc Process Message
----------------------------------------------------------------------------

-- | Process message and save it if needed. Result is whether message
-- has been actually added.
sscProcessMessage ::
       MonadSscLD SscGodTossing m
    => DataMsg -> m Bool
sscProcessMessage = sscRunLocalUpdate  . sscProcessMessageU

sscProcessMessageU :: DataMsg -> LDUpdate Bool
sscProcessMessageU (DMCommitment pk comm)     = processCommitment pk comm
sscProcessMessageU (DMOpening pk open)        = processOpening pk open
sscProcessMessageU (DMShares pk shares)       = processShares pk shares
sscProcessMessageU (DMVssCertificate pk cert) = processVssCertificate pk cert

processCommitment
    :: PublicKey -> (Commitment, CommitmentSignature) -> LDUpdate Bool
processCommitment pk c = do
    epochIdx <- siEpoch <$> use gtLastProcessedSlot
    ok <- readerToState $ andM $ checks epochIdx
    ok <$ when ok (gtLocalCommitments %= HM.insert pk c)
  where
    checks epochIndex =
        [ not . HM.member pk <$> view gtGlobalCommitments
        , not . HM.member pk <$> view gtLocalCommitments
        , HM.member pk <$> view gtGlobalCertificates
        , pure . isVerSuccess $ verifySignedCommitment pk epochIndex c
        ]

processOpening :: PublicKey -> Opening -> LDUpdate Bool
processOpening pk o = do
    ok <- readerToState $ andM checks
    ok <$ when ok (gtLocalOpenings %= HM.insert pk o)
  where
    checkAbsence = sscIsDataUsefulSetImpl gtLocalOpenings gtGlobalOpenings
    checks = [checkAbsence pk, matchOpening pk o]

-- Match opening to commitment from globalCommitments
matchOpening :: PublicKey -> Opening -> LDQuery Bool
matchOpening pk opening =
    flip checkOpeningMatchesCommitment (pk, opening) <$> view gtGlobalCommitments

processShares :: PublicKey -> HashMap PublicKey Share -> LDUpdate Bool
processShares pk s
    | null s = pure False
    | otherwise = do
        -- TODO: we accept shares that we already have (but don't add them to
        -- local shares) because someone who sent us those shares might not be
        -- aware of the fact that they are already in the blockchain. On the
        -- other hand, now nodes can send us huge spammy messages and we can't
        -- ban them for that. On the third hand, is this a concern?
        globalSharesPKForPK <- HM.lookupDefault mempty pk <$> use gtGlobalShares
        localSharesForPk <- HM.lookupDefault mempty pk <$> use gtLocalShares
        let s' = s `HM.difference` (HS.toMap globalSharesPKForPK)
        let newLocalShares = localSharesForPk `HM.union` s'
        -- Note: size is O(n), but union is also O(n + m), so
        -- it doesn't matter.
        let checks =
              [ pure (HM.size newLocalShares /= HM.size localSharesForPk)
              , readerToState $ checkSharesLastVer pk s
              ]
        ok <- andM checks
        ok <$ when ok (gtLocalShares . at pk .= Just newLocalShares)

checkSharesLastVer :: PublicKey -> HashMap PublicKey Share -> LDQuery Bool
checkSharesLastVer pk shares =
    (\comms openings certs -> checkShares comms openings certs pk shares) <$>
    view gtGlobalCommitments <*>
    view gtGlobalOpenings <*>
    view gtGlobalCertificates

processVssCertificate :: PublicKey -> VssCertificate -> LDUpdate Bool
processVssCertificate pk c = do
    ok <- not . HM.member pk <$> use gtGlobalCertificates
    ok <$ when ok (gtLocalCertificates %= HM.insert pk c)

----------------------------------------------------------------------------
-- Apply Global State
----------------------------------------------------------------------------
applyGlobal :: GtGlobalState -> LDUpdate ()
applyGlobal globalData = do
    let
        globalCommitments = _gsCommitments globalData
        globalOpenings = _gsOpenings globalData
        globalShares = _gsShares globalData
        globalCert = _gsVssCertificates globalData
    gtLocalCommitments  %= (`HM.difference` globalCommitments)
    gtLocalOpenings  %= (`HM.difference` globalOpenings)
    gtLocalShares  %= (`diffDoubleMap` globalShares)
    gtLocalCertificates  %= (`HM.difference` globalCert)

    slotId <- use gtLastProcessedSlot
    if inLastKSlotsId slotId then clearGlobalState
    else do
        gtGlobalCommitments .= globalCommitments `HM.difference` globalOpenings
        gtGlobalOpenings .= getKeys globalOpenings
        gtGlobalShares .= HM.map getKeys globalShares
        gtGlobalCertificates .= globalCert

----------------------------------------------------------------------------
-- Get Local Payload
----------------------------------------------------------------------------
getLocalPayload :: SlotId -> LDQuery GtPayload
getLocalPayload SlotId{..} =
    (if isCommitmentIdx siSlot then
        CommitmentsPayload <$> view gtLocalCommitments
    else if isOpeningIdx siSlot then
        OpeningsPayload <$> view gtLocalOpenings
    else if isSharesIdx siSlot then
        SharesPayload <$> view gtLocalShares
    else
        pure CertificatesPayload)
    <*> view gtLocalCertificates
