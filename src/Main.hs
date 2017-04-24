module Main where

import qualified Network.Socket as Sock
import Data.List (intercalate)
import Network.Multicast
import Control.Concurrent
import Data.IORef
import Control.Monad
import Control.Monad.Trans
import Control.Concurrent.Chan
import Happstack.Server (ServerPart, Response, nullConf, simpleHTTP, ok, nullDir, notFound, toResponse)

data Node = Node {
    addr :: String
    , neighbours :: [String]
}

recv :: Chan String -> IO ()
recv ch = Sock.withSocketsDo $ do
    sock <- multicastReceiver "224.0.0.1" 9999
    let loop = do
        (msg, _, addr) <- Sock.recvFrom sock 1024
        print (msg, addr)
        writeChan ch $ show addr
        loop in loop

send :: IORef [String] -> IO ()
send ref = Sock.withSocketsDo $ do
    (sock, addr) <- multicastSender "224.0.0.1" 9999
    let loop = do
        nodes <- readIORef ref
        Sock.sendTo sock "ping" addr
        putStrLn "sent"
        threadDelay 10000000 --10s
        loop in loop

homePage :: IORef [String] -> ServerPart Response
homePage ref = do
    nodes <- liftIO $ readIORef ref
    ok . toResponse $ "known nodes: " ++ intercalate "," nodes

myServer :: IORef [String] -> IO ()
myServer ref = simpleHTTP nullConf $ msum
    [ homePage ref ]

worker :: Chan String -> IORef [String] -> IO ()
worker ch ref = forever (readChan ch >>= \value -> modifyIORef ref (value:))

newRef = newIORef ([] :: [String])

main = do
    nodes <- newRef
    ch <- newChan
    forkIO $ worker ch nodes
    forkIO $ myServer nodes
    forkIO $ recv ch
    send nodes

-- TODO The IO Ref needs to be a set
-- TODO Functions in a another module to handle state
-- TODO Maybe each node should respond with the nodes it knows about as well as its own ip
