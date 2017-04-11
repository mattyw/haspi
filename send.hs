import Network.Socket
import Network.Multicast
import Control.Concurrent

import Data.IORef

main = withSocketsDo $ do
    (sock, addr) <- multicastSender "224.0.0.1" 9999
    i <- newIORef 0
    let loop = do
        msg <- readIORef i
        sendTo sock (show msg) addr
        putStrLn "sent"
        modifyIORef i (+1)
        threadDelay 10000000 --10s
        loop in loop
