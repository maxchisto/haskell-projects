import Control.Monad
import Control.Concurrent

data Logger = Logger (MVar LogCommand)

data LogCommand = Message String | Stop (MVar ())

initLogger :: IO Logger
initLogger = do
    m <- newEmptyMVar
    let loggerHandle = Logger m
    forkIO $ loggerLoop loggerHandle
    
    return loggerHandle

loggerLoop :: Logger -> IO ()
loggerLoop (Logger m) = loop 
    where 
      loop = do
        cmd <- takeMVar m
        case cmd of
            (Message msg) -> do
                putStrLn msg
                loop
            (Stop s) -> do
                putStrLn "Stopping logger"
                putMVar s ()
                
logString :: Logger -> String -> IO ()
logString (Logger m) msg = putMVar m (Message msg)

stopLogger :: Logger -> IO ()
stopLogger (Logger m) = do
    s <- newEmptyMVar
    putMVar m (Stop s)
    takeMVar s



main = do
    l <- initLogger
    logString l "wat?"
    logString l "1234567890"
    stopLogger l
    logString l "this won't be logged"