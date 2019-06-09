import Control.Concurrent

type Lock = MVar ()

beep :: Lock -> IO ()
beep lock = do
    yield

    withMVar lock $ \_ -> do
        putStrLn "beep"
    
    beep lock

boop :: Lock -> IO ()
boop lock = do
    yield

    withMVar lock $ \_ -> do 
        putStr "boop"

    boop lock

main :: IO ()
main = do
    lock <- newMVar ()

    beepId <- forkIO $ beep lock
    boopId <- forkIO $ boop lock

    threadDelay 1000
    putMVar lock ()

    return ()