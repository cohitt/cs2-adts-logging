module LogAnalysis where

import Log

parseMessage :: String -> LogMessage
parseMessage s | head(s)=='I' = LogMessage Info (read ((words s) !! 1)) (unwords (drop 2 (words s)))
parseMessage s | head(s)=='W' = LogMessage Warning (read ((words s) !! 1)) (unwords (drop  2(words s)))
parseMessage s | head(s)=='E' = LogMessage (Error (read ((words s) !! 1))) (read ((words s) !! 2)) (unwords (drop 3 (words s)))
parseMessage s = Unknown s

parseMessageType :: String -> Maybe MessageType
parseMessageType s | head(s)=='I' = Just Info
parseMessageType s | head(s)=='W' = Just Warning
parseMessageType s | head(s)=='E' = Just (Error (read ((words s) !! 1)))
parseMessageType s = Nothing

parseTimeStamp :: String -> Maybe TimeStamp
parseTimeStamp s | head(s)=='I' || head(s)=='W' = Just (read ((words s) !! 1))
parseTimeStamp s | head(s)=='E' = Just (read ((words s) !! 2))
parseTimeStamp s = Nothing

parse :: String -> [LogMessage]
parse s = map (parseMessage) (lines s)

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown l) m = m
insert l Leaf = Node Leaf l Leaf
insert (LogMessage t ts s) (Node mt1 (LogMessage tn tsn sn) mt2) | ts>tsn = Node mt1 (LogMessage tn tsn sn) (insert (LogMessage t ts s) mt2)
insert (LogMessage t ts s) (Node mt1 (LogMessage tn tsn sn) mt2) = Node (insert (LogMessage t ts s) mt1) (LogMessage tn tsn sn) mt2

build :: [LogMessage] -> MessageTree
build l = foldr (insert) Leaf l

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node mt1 lm mt2) = inOrder mt1 ++ [lm] ++ inOrder mt2

helper :: LogMessage -> String
helper (LogMessage t ts s) = show(t) ++ show(ts) ++ s 

checkType :: LogMessage -> Bool
checkType (LogMessage (Error sv) _ _) | sv>50 = True
checkType _ = False

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong ls = map (helper) (filter (checkType) (inOrder (build ls)))

--Filter so only messages that are errors with severity of 50 or greater are retained