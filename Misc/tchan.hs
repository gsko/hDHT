import Control.Monad.STM
import Control.Concurrent
import Control.Concurrent.STM.TChan

delaySec :: Int -> IO ()
delaySec = fmap threadDelay (*oneSecond)
    where oneSecond = 1000000

writerThread :: TChan Int -> IO ()
writerThread chan = do
        write 1
        delaySec 1
        write 2
        delaySec 1 
        write 3
        delaySec 1 
    where write = (atomically . writeTChan chan)

readerThread :: TChan Int -> IO ()
readerThread chan = do
        newInt <- atomically $ readTChan chan
        putStrLn $ "read new value: " ++ show newInt
        readerThread chan

main = do
        chan <- atomically $ newTChan
        forkIO $ readerThread chan
        forkIO $ writerThread chan
        delaySec 5
