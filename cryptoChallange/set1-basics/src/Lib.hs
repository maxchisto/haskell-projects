{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( someFunc,
      xorBS,
      binHammingDistance,
      textSuspects
    ) where

import Data.Bits (xor, popCount)
import Data.ByteString as BS
import Data.ByteString.Base16 as B16
import Data.ByteString.Base64 as B64
import Data.ByteString.Char8 as C
import Data.Char (toLower)
import Data.List as L
import Data.Word


someFunc :: IO ()
someFunc = do
    BS.putStrLn $ hex2bin "666f6f"  -- should return "foo"
    BS.putStrLn $ hex2base64 "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"
    BS.putStrLn $ xorBS "1c0111001f010100061a024b53535009181c" "686974207468652062756c6c277320657965"
    -- mapM_ BS.putStrLn allMsgs
    -- Prelude.putStrLn $ show $ rateText "W{{\DEL}zs4YW3g4x}\DELq4u4d{azp4{r4vuw{z"
    printTextSuspects "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"
    -- text <- BS.readFile "resources/4.txt"
    -- let lines = C.split '\n' text
    -- mapM_ printTextSuspects lines
    -- let line = "Burning 'em, if you ain't quick and nimble\nI go crazy when I hear a cymbal"
    -- let key = "ICE"
    -- Prelude.putStrLn $ show $ B16.encode $ xorEncode line key
    -- Prelude.putStrLn $ show $ xorDecode "0b3637272a2b2e63622c2e69692a23693a2a3c6324202d623d63343c2a26226324272765272a282b2f20430a652e2c652a3124333a653e2b2027630c692b20283165286326302e27282f" key

    -- Prelude.putStrLn $ show $ binHammingDistance "this is a test" "wokka wokka!!!"
    return ()


hex2base64 :: ByteString -> ByteString
hex2base64 = bin2base64 . hex2bin

hex2bin :: ByteString -> ByteString
hex2bin s = fst $ B16.decode s

bin2base64 :: ByteString -> ByteString
bin2base64 s = B64.encode s

xorBS :: ByteString -> ByteString -> ByteString
xorBS x y = BS.pack $ BS.zipWith xor (hex2bin x) (hex2bin y)

decipher :: ByteString -> Word8 -> ByteString
decipher xs y = BS.map (xor y) (hex2bin xs)

allCombos :: ByteString -> [ByteString]
allCombos hexString = Prelude.map (decipher hexString) codes
    where codes = Prelude.map toEnum [0..255]


printTextSuspects :: ByteString -> IO ()
printTextSuspects s = mapM_ (Prelude.putStrLn . show) $ textSuspects s

textSuspects :: ByteString -> [(ByteString, Float)]
textSuspects hexString = L.take 5 $ L.sortBy sortingFunc (Prelude.map toRatedText $ allCombos hexString)
    where toRatedText = \x -> (x,rateText x)
          sortingFunc x y | (snd x) < (snd y) = GT
                          | otherwise = LT


rateText :: ByteString -> Float
rateText = freqScore . toCharList
  where toCharList = C.unpack . (C.map toLower) . removeSpaces

tripletScore :: [Char] -> Int
tripletScore (l1:l2:l3:xs) = score (l1,l2,l3) + tripletScore (l2:l3:xs)
tripletScore (_:_:[]) = 0
tripletScore (_:[]) = 0
tripletScore [] = 0

removeSpaces :: ByteString -> ByteString
removeSpaces xs = C.filter (/= ' ') xs


score :: (Char, Char, Char) -> Int
score ('t','h','e') = 16
score ('a','n','d') = 15
score ('t','h','a') = 14
score ('e','n','t') = 13
score ('i','n','g') = 12
score ('i','o','n') = 11
score ('t','i','o') = 10
score ('f','o','r') = 9
score ('n','d','e') = 8
score ('h','a','s') = 7
score ('n','c','e') = 6
score ('e','d','t') = 5
score ('t','i','s') = 4
score ('o','f','t') = 3
score ('s','t','h') = 2
score ('m','e','n') = 1
score (l1,l2,l3) = 0


charToFreq :: Char -> Float
charToFreq 'e' = 12.02
charToFreq 't' = 9.1
charToFreq 'a' = 8.12
charToFreq 'o' = 7.68
charToFreq 'i' = 7.31
charToFreq 'n' = 6.95
charToFreq 's' = 6.28
charToFreq 'r' = 6.02
charToFreq 'h' = 5.92
charToFreq 'd' = 4.32
charToFreq 'l' = 3.98
charToFreq 'u' = 2.88
charToFreq 'c' = 2.71
charToFreq 'm' = 2.61
charToFreq 'f' = 2.30
charToFreq 'y' = 2.11
charToFreq 'w' = 2.09
charToFreq 'g' = 2.03
charToFreq 'p' = 1.82
charToFreq _ = 0.0


freqScore :: [Char] -> Float
freqScore xs = freqSum / Prelude.fromIntegral (Prelude.length xs)
  where freqSum = Prelude.sum $ Prelude.map charToFreq xs


repeatXor :: [Word8] -> [Word8] -> [Word8]
repeatXor [] keys = []
repeatXor (x:xs) (key:keys) = (x `xor` key) : (repeatXor xs (keys ++ [key]))

-- Result comes out binary. Use B16.encode to show it
xorEncode :: ByteString -> ByteString -> ByteString
xorEncode text key = BS.pack $ repeatXor (BS.unpack text) (BS.unpack key)

xorDecode :: ByteString -> ByteString -> ByteString
xorDecode hex key = xorEncode (hex2bin hex) key

-- BS.zipWith returns [Word8]
binHammingDistance :: ByteString -> ByteString -> Int
binHammingDistance xs ys = Prelude.foldr (\x acc -> acc + (popCount x)) 0 xoredBytes
    where xoredBytes = BS.zipWith xor xs ys