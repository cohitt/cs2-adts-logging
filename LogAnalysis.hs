module Main where

import Log

main :: IO ()
main = undefined

parseMessage :: String -> LogMessage
parseMessage s | head(s)=='I' = LogMessage Info (read ((words s) !! 1)) (unwords (drop 1 (words s)))
parseMessage s | head(s)=='W' = LogMessage Warning (read ((words s) !! 1)) (unwords (drop 1 (words s)))
parseMessage s | head(s)=='E' = LogMessage (Error (read ((words s) !! 1))) (read ((words s) !! 2)) (unwords (drop 2 (words s)))
parseMessage s = Unknown s

parse :: String -> [LogMessage]
parse s = map (parseMessage) (lines s)

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown l) m = m
insert l Leaf = Node Leaf l Leaf
insert (LogMessage t ts s) (Node mt1 lm mt2) = Node (Node mt1 lm mt2) (LogMessage t ts s) Leaf

build :: [LogMessage] -> MessageTree
build l = foldr (insert) Leaf l



