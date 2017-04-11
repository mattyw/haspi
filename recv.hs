import Network.Socket
import Network.Multicast
import Control.Concurrent

main = withSocketsDo $ do
    sock <- multicastReceiver "224.0.0.1" 9999
    let loop = do
        (msg, _, addr) <- recvFrom sock 1024
        print (msg, addr)
        loop in loop

