module Main where

import qualified Network.Socket as Sock
import Network.Multicast
import Control.Concurrent
import Control.Monad
import Happstack.Server (ServerPart, Response, nullConf, simpleHTTP, ok, nullDir, notFound)

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

-- TODO how do I get data from recv into output here.
homePage :: ServerPart String
homePage = ok $ do "haspi is running"

myServer = simpleHTTP nullConf $ msum
    [ homePage ]

main = do
    forkIO myServer
    forkIO recv
    send
