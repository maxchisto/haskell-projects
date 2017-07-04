{-# LANGUAGE OverloadedStrings #-}

module RepeatXor
    (
      breakCode,
      chunkByKeysize,
      transposeChunks
    ) where


import Data.Bits (xor)
import Data.ByteString as BS
import Data.ByteString.Char8 as C
import Data.ByteString.Base64 as B64
import Data.ByteString.Base16 as B16
import Data.List.Split (chunksOf)
import Data.List as L
import Data.String as S
import Data.Word
import Lib

type Keysize = Int

breakCode :: IO ()
breakCode = do
    Prelude.putStrLn "Xor2!"
    payload <- getBinaryPayload "resources/6.txt"
    mapM_ (Prelude.putStrLn . show) (findHiddenText payload)

    return ()

getBinaryPayload :: FilePath -> IO ByteString
getBinaryPayload path = do
    text <- BS.readFile path
    let payloadB64 = C.filter (\x -> x /= '\n') text
    Prelude.putStrLn $ show $ payloadB64
    return $ decodeLenient payloadB64


normedDistance :: ByteString -> Keysize -> Float
normedDistance bString n = fromIntegral totalDistance / fromIntegral n
    where bytes = BS.unpack bString
          a = C.take n bString
          b = C.take n (C.drop n bString)
          c = C.take n (C.drop (2*n) bString)
          d = C.take n (C.drop (3*n) bString)
          totalDistance = binHammingDistance a b + binHammingDistance b c + binHammingDistance a c + binHammingDistance a d + binHammingDistance b d

-- Calculate score using normalized hamming distance
bytesWithScore :: ByteString -> Keysize -> (Keysize, Float)
bytesWithScore bs k = (k, score)
    where score = normedDistance bs k

sortingFunc :: (Keysize, Float) -> (Keysize, Float) -> Ordering
sortingFunc x y | (snd x) > (snd y) = GT
                        | otherwise = LT

-- Produces a list of keysizes based on scoring function
likelyKeysizes :: ByteString -> [Keysize]
likelyKeysizes bs = L.map fst sortedTuples
    where tuples = [bytesWithScore bs x | x <- [2..40]]
          sortedTuples = L.sortBy sortingFunc tuples

-- Splits payload in chunks of the size of keysize
chunkByKeysize :: ByteString -> Keysize -> [ByteString]
chunkByKeysize bs k = L.map BS.pack chunks
    where words = BS.unpack bs
          chunks = chunksOf k words

transposeChunks :: [ByteString] -> [ByteString]
transposeChunks = (L.map BS.pack) . L.transpose . (L.map BS.unpack)

decodeSingleXor :: ByteString -> ByteString
decodeSingleXor bs = fst $ L.head (textSuspects bs)

-- Takes transposed bytestring and returns possible combos
combinationsOfChunks :: [ByteString] -> [ByteString]
combinationsOfChunks chunks = L.map decodeSingleXor chunks

textWithScore :: ByteString -> (ByteString, Float)
textWithScore bs = (bs, rateText bs)

decodeXor :: ByteString -> [(ByteString, Float)]
decodeXor bs = do
    keysize <- L.take 5 $ likelyKeysizes bs
    let textChunks = transposeChunks $ combinationsOfChunks $ transposeChunks $ chunkByKeysize bs keysize
    return $ textWithScore $ BS.concat textChunks

sortRatedText :: [(ByteString, Float)] -> [(ByteString, Float)]
sortRatedText = L.sortBy sortingFunc
    where sortingFunc x y | (snd x) < (snd y) = GT
                          | otherwise = LT

findHiddenText :: ByteString -> [String]
findHiddenText = S.lines . C.unpack . fst . L.head . sortRatedText . decodeXor