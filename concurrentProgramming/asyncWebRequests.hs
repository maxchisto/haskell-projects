import Control.Monad
import Control.Concurrent
import Control.Concurrent.Async
import Network.HTTP

sites = ["http://www.google.com",
         "http://www.yahoo.com",
         "http://duckduckgo.com",
         "http://www.bing.com"]
         
pageContents :: String -> IO (String)
pageContents url = do
    requestResult <- simpleHTTP (getRequest url)
    getResponseBody requestResult
    

printLength :: String -> IO ()
printLength = (putStrLn . show . length)

main = do
    -- non-blocking single request
    requestFuture <- async (pageContents "http://www.google.com")
    putStr "Waiting for the single request... "
    contents <- wait requestFuture
    printLength contents
    
    -- A set of async requests 
    requestFutures <- mapM (async . pageContents) sites
    contents <- sequence (map wait requestFutures)
    mapM_ printLength contents