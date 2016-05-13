import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad
import Network
import System.IO
import Text.Printf

fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

port :: Int
port = 44444

processRequest :: Handle -> IO ()
processRequest handle = do
    hSetBuffering handle LineBuffering
    hPutStrLn handle ("Connection established. Waiting for input...\n")
    loop
    where
       loop = do
           hPutStrLn handle ("?>")
           line <- hGetLine handle
           hPutStrLn handle ("Reply: " ++ (reverse line))
           loop
           
main = withSocketsDo $ do
    socket <- listenOn (PortNumber $ fromIntegral port)
    printf "Listening on port %d\n" port
    forever $ do
        (handle, host, port) <- accept socket
        printf "Accepted connection from %s: %s\n" host (show port)
        forkFinally (processRequest handle) (\_ -> hClose handle)