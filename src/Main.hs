import qualified Network.Socket as Sock
import Network.Multicast
import Control.Concurrent

recv = Sock.withSocketsDo $ do
    sock <- multicastReceiver "224.0.0.1" 9999
    let loop = do
        (msg, _, addr) <- Sock.recvFrom sock 1024
        print (msg, addr)
        loop in loop


send = Sock.withSocketsDo $ do
    (sock, addr) <- multicastSender "224.0.0.1" 9999
    let loop = do
        Sock.sendTo sock "ping" addr
        putStrLn "sent"
        threadDelay 10000000 --10s
        loop in loop

main = do
    forkIO recv
    send
