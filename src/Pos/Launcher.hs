{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

-- | Launcher of node.

module Pos.Launcher
       ( NodeParams (..)
       , runNode
       , runNodeReal
       ) where

import           Universum

import           Pos.Communication (serve)
import           Pos.WorkMode      (NodeParams (..), WorkMode, runRealMode)

runNode :: WorkMode m => m ()
runNode = do
    serve

runNodeReal :: NodeParams -> IO ()
runNodeReal p = runRealMode p runNode

-- runNodes :: WorkMode m => [Node m] -> m ()
-- runNodes nodes = setLoggerName "xx" $ do
--     let n = length nodes
--     keys <- Map.fromList . zip [NodeId 0 .. NodeId (n - 1)] <$>
--                 replicateM n keyGen
--     -- The system shall start working in a bit of time. Not exactly right now
--     -- because due to the way inSlot implemented, it'd be nice to wait a bit
--     -- – if we start right now then all nodes will miss the first slot of the
--     -- first epoch.
--     now <- virtualTime
--     liftIO $ writeIORef systemStart (now + slotDuration `div` 2)
--     inSlot False $ \epoch slot -> do
--         when (slot == 0) $
--             logInfo $ sformat ("========== EPOCH "%int%" ==========") epoch
--         logInfo $ sformat ("---------- slot "%int%" ----------") slot
--     nodeCallbacks <- liftIO $ newIORef mempty
--     let send n_from n_to message = do
--             f <- (Map.! n_to) <$> liftIO (readIORef nodeCallbacks)
--             f n_from message
--     for_ (zip [0..] nodes) $ \(i, nodeFun) -> do
--         let nid = NodeId i
--         f <- nodeFun nid (keys Map.! nid) n (fmap fst keys) (send nid)
--         liftIO $ modifyIORef' nodeCallbacks (Map.insert nid f)
--     sleepForever

-- runNodesReal :: [Node RealMode] -> IO ()
-- runNodesReal = runRealMode "tmp" ctx . runNodes
--   where
--     ctx = NodeContext 0
