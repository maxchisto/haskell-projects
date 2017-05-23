{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( someFunc
    ) where

import Data.Bits (xor)
import Data.ByteString as BS
import Data.ByteString.Base16 as B16
import Data.ByteString.Base64 as B64


someFunc :: IO ()
someFunc = do
    BS.putStrLn $ hex2bin "666f6f"  -- should return "foo"
    BS.putStrLn $ hex2base64 "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"
    BS.putStrLn $ xorBS "1c0111001f010100061a024b53535009181c" "686974207468652062756c6c277320657965"
    -- mapM_ BS.putStrLn (decipher )
    return ()


hex2base64 :: ByteString -> ByteString
hex2base64 = bin2base64 . hex2bin

hex2bin :: ByteString -> ByteString
hex2bin s = fst $ B16.decode s

bin2base64 :: ByteString -> ByteString
bin2base64 s = B64.encode s

xorBS :: ByteString -> ByteString -> ByteString
xorBS x y = BS.pack $ BS.zipWith xor (hex2bin x) (hex2bin y)

