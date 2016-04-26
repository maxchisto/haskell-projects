-- Converts a simple algebraic expression into an array of expression tokens


import Data.Char as DC hiding (isNumber)

data Token = Number Int | Plus | Minus | LeftBrace | RightBrace 
    deriving (Eq, Show)

tokenize :: String -> Maybe [Token]
tokenize s = sequence tokens
    where expWords = words s
          -- tokens :: [Maybe Token] 
          tokens = map asToken expWords


asToken :: String -> Maybe Token
asToken x | isNumber x      = Just (Number (read x :: Int))
          | isPlus x        = Just Plus
          | isMinus x       = Just Minus
          | isLeftBrace x   = Just LeftBrace
          | isRightBrace x  = Just RightBrace
          | otherwise       = Nothing 
 
isNumber :: String -> Bool
isNumber "" = True
isNumber (x:xs) = (DC.isDigit x) && isNumber xs

isPlus :: String -> Bool
isPlus "+" = True
isPlus _ = False

isMinus :: String -> Bool
isMinus "-" = True
isMinus _ = False

isLeftBrace :: String -> Bool
isLeftBrace "(" = True
isLeftBrace _ = False

isRightBrace :: String -> Bool
isRightBrace ")" = True
isRightBrace _ = False