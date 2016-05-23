{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where
import Log

extractTimeStamp :: String -> Int
extractTimeStamp s = read s::Int

extractErrorCode :: String -> Int
extractErrorCode = extractTimeStamp

parseWords :: [String] -> LogMessage
parseWords ("I":(w:wds)) = LogMessage Info (extractTimeStamp w) (unwords wds)
parseWords ("W":(w:wds)) = LogMessage Warning (extractTimeStamp w) (unwords  wds)
parseWords ("E":(e_code:(w:wds))) = LogMessage (Error (extractErrorCode e_code)) (extractTimeStamp w) (unwords  wds)
parseWords wds = Unknown (unwords wds) 

parseLine :: String -> LogMessage
parseLine s = parseWords (words s)

parse :: String -> [LogMessage]
parse s = map parseLine (lines s)

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) mt = mt
insert lm@(LogMessage _ _ _) Leaf = Node Leaf lm Leaf
insert lm@(LogMessage _ ts _) (Node left_mt lm2@(LogMessage _ nodeVal _) right_mt)
    | ts < nodeVal  = Node (insert lm left_mt) lm2 right_mt
    | otherwise     = Node left_mt lm2 (insert lm right_mt)

build :: [LogMessage] -> MessageTree
build [] = Leaf
build (x:xs) = insert x (build xs)

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node Leaf lm Leaf) = [lm]
inOrder (Node leftMt lm rightMt) = (inOrder leftMt) ++ [lm] ++ (inOrder rightMt)

extractText :: [LogMessage] -> [String]
extractText [] = []
extractText ((LogMessage (Error x) _ s):xs)
    | x >= 50 =  s:(extractText xs)
    | otherwise = extractText xs
extractText ((LogMessage Info _ s):xs) = extractText xs
extractText ((LogMessage Warning _ s):xs) = extractText xs

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong x = extractText (inOrder (build x))