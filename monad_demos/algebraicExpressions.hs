-- Converts a simple algebraic expression into an array of expression tokens


import Data.Char as DC hiding (isNumber)

data Token = Number Int | Plus | Minus | LeftBrace | RightBrace 
    deriving (Eq, Show)

tokenize :: String -> Maybe [Token]
tokenize s = foldr foldFunc (Just []) $ map asToken $ words s

foldFunc :: Maybe Token -> Maybe [Token] -> Maybe [Token]
foldFunc maybeToken maybeTokens = do x <- maybeToken
                                     y <- maybeTokens
                                     return (x:y) 
                                     -- token >>= (\x -> 
                                     --   tokens >>= (\y -> 
                                     --     return (x:y)    
                                     --   )
                                     -- )

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