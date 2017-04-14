module Main where

import qualified Network.Socket as Sock
import Network.Multicast
import Control.Concurrent
import Data.IORef
import Control.Monad
import Control.Concurrent.Chan
import Happstack.Server (ServerPart, Response, nullConf, simpleHTTP, ok, nullDir, notFound)

recv :: Chan String -> IO ()
recv ch = Sock.withSocketsDo $ do
    sock <- multicastReceiver "224.0.0.1" 9999
    let loop = do
        (msg, _, addr) <- Sock.recvFrom sock 1024
        print (msg, addr)
        writeChan ch $ show addr
        loop in loop

send = Sock.withSocketsDo $ do
    (sock, addr) <- multicastSender "224.0.0.1" 9999
    let loop = do
        Sock.sendTo sock "ping" addr
        putStrLn "sent"
        threadDelay 10000000 --10s
        loop in loop

homePage :: ServerPart String
homePage = ok $ do "haspi is running"

myServer :: IORef [String] -> IO ()
myServer ref = simpleHTTP nullConf $ msum
    [ homePage ]

worker :: Chan String -> IORef [String] -> IO ()
worker ch ref = forever (readChan ch >>= \value -> modifyIORef ref (value:))

newRef = newIORef ([] :: [String])

main = do
    nodes <- newRef
    ch <- newChan
    forkIO $ worker ch nodes
    forkIO $ myServer nodes
    forkIO $ recv ch
    send

-- TODO The IO Ref needs to be a set
-- TODO The IO Ref needs to be read and displayed in the web page output
-- TODO Functions in a another module to handle state
-- TODO Maybe each node should respond with the nodes it knows about as well as its own ip
