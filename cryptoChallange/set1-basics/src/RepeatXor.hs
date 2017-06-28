{-# LANGUAGE OverloadedStrings #-}

module RepeatXor
    (
      demo
    ) where

import Data.Bits (xor)
import Data.ByteString as BS
import Data.ByteString.Char8 as C
import Data.ByteString.Base64 as B64
import Data.ByteString.Base16 as B16
import Data.List.Split (chunksOf)
import Data.List as L
import Data.Word
import Lib

type Window = Int


demo :: IO ()
demo = do
    Prelude.putStrLn "Demo!"
    payload <- getBinaryPayload "resources/6.txt"
    mapM_ (\x -> Prelude.putStrLn $ show $ (x, normedDistance payload x)) [2..40]
    -- Prelude.putStrLn $ show $ transposeBS $ processPayload payload
    let payloadB16 = B16.encode payload
    -- mapM_ (\x -> printDecoded x payloadB16) [20..40]
    printDecoded 1 "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"
    return ()

printBin :: ByteString -> IO ()
printBin bytes = do
    Prelude.putStrLn $ show $ B16.encode $ bytes
    return ()

getBinaryPayload :: FilePath -> IO ByteString
getBinaryPayload path = do
    text <- BS.readFile path
    let payloadB64 = C.filter (\x -> x /= '\n') text
    Prelude.putStrLn $ show $ payloadB64
    return $ decodeLenient payloadB64


normedDistance :: ByteString -> Window -> Float
normedDistance bString n = fromIntegral totalDistance / fromIntegral n
    where bytes = BS.unpack bString
          a = C.take n bString
          b = C.take n (C.drop n bString)
          c = C.take n (C.drop (2*n) bString)
          d = C.take n (C.drop (3*n) bString)
          totalDistance = binHammingDistance a b + binHammingDistance b c + binHammingDistance a c + binHammingDistance a d + binHammingDistance b d

-- Split by key size and transpose
preparePayload :: ByteString -> Int -> [ByteString]
preparePayload xs n = L.map BS.pack (L.transpose chunks)
    where chars = BS.unpack xs
          chunks = chunksOf n chars

decipher :: ByteString -> ByteString
decipher = fst . L.head . textSuspects

processPayload :: Int -> ByteString -> [ByteString]
processPayload window payload = L.map BS.pack (L.transpose unpackedCols)
    where bsList = preparePayload payload window
          decipheredCols = L.map decipher bsList
          unpackedCols = L.map BS.unpack decipheredCols


transposeBS :: [ByteString] -> [ByteString]
transposeBS = (L.map BS.pack) . L.transpose . (L.map BS.unpack)

printDecoded :: Int -> ByteString -> IO ()
printDecoded n bs = Prelude.putStrLn $ show $ BS.concat $ processPayload n bs