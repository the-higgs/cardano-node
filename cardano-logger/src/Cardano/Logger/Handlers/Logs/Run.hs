{-# LANGUAGE LambdaCase #-}

module Cardano.Logger.Handlers.Logs.Run
  ( runLogsHandler
  ) where

import           Control.Concurrent (threadDelay)
import           Control.Concurrent.STM (STM, atomically)
import           Control.Concurrent.STM.TBQueue (TBQueue, tryReadTBQueue)
import           Control.Monad (forM_, forever)
import qualified Data.HashMap.Strict as HM
import           Data.IORef (readIORef)
import           Data.List (intercalate)

import           Cardano.Logger.Configuration
import           Cardano.Logger.Types (AcceptedItems, NodeId)

runLogsHandler
  :: LoggerConfig
  -> AcceptedItems
  -> IO ()
runLogsHandler config acceptedItems = forever $ do
  threadDelay 2000000
  items <- HM.toList <$> readIORef acceptedItems
  forM_ items $ \(nodeId, (_niStore, loQueue, _)) ->
    atomically (getAllLogObjects loQueue) >>= writeLogObjects config nodeId

getAllLogObjects :: TBQueue lo -> STM [lo]
getAllLogObjects loQueue =
  tryReadTBQueue loQueue >>= \case
    Just lo' -> (:) lo' <$> getAllLogObjects loQueue
    Nothing  -> return []

writeLogObjects
  :: Show lo
  => LoggerConfig
  -> NodeId
  -> [lo]
  -> IO ()
writeLogObjects _ _ [] = return ()
writeLogObjects _config nodeId logObjects =
  appendFile fileName . intercalate "\n" . map show $ logObjects
 where
  fileName = "/tmp/cardano-logger-test-" <> show nodeId <> ".log"
