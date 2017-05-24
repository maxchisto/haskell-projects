{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( someFunc
    ) where

import Data.Bits (xor)
import Data.ByteString as BS
import Data.ByteString.Base16 as B16
import Data.ByteString.Base64 as B64
import Data.ByteString.Char8 as C
import Data.Char (toLower)
import Data.Word


someFunc :: IO ()
someFunc = do
    BS.putStrLn $ hex2bin "666f6f"  -- should return "foo"
    BS.putStrLn $ hex2base64 "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"
    BS.putStrLn $ xorBS "1c0111001f010100061a024b53535009181c" "686974207468652062756c6c277320657965"
    -- mapM_ BS.putStrLn allMsgs
    -- Prelude.putStrLn $ show $ rateText "W{{\DEL}zs4YW3g4x}\DELq4u4d{azp4{r4vuw{z"
    mapM_ (Prelude.putStrLn . show) $ Prelude.filter (\(_,y) -> y > 0) (Prelude.map (\x -> (x,rateText x)) allMsgs)
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

allMsgs :: [ByteString]
allMsgs = Prelude.map (decipher "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736") codes
    where codes = Prelude.map toEnum [0..255]

rateText :: ByteString -> Int
rateText = tripletScore . C.unpack . (C.map toLower) . removeSpaces

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