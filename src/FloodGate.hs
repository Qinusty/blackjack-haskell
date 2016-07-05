module FloodGate where

import Control.Monad.STM
import Control.Concurrent
import Control.Concurrent.STM.TChan
import Utils
type FloodGate = [IO ()]


oneSecond = 1000000

runThread :: TChan (IO ()) -> IO ()
runThread chan = forever $ do
    action <-  atomically $ readTChan chan
    action
    threadDelay oneSecond
    
