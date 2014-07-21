{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log
import Data.Char (isDigit)

parseMessage :: String -> LogMessage
parseMessage message = let (f:s:t:rest) = (words message)
                       in logMsg f s t rest where
                           logMsg :: String -> String -> String -> [String] -> LogMessage
                           logMsg _ num1 _ _
                               | any (not . isDigit) num1 = Unknown message
                           logMsg "E" errStr timeStr rest =
                               let errNo = read errStr :: Int
                               in if errNo > 0 && errNo <= 100
                                  then LogMessage (Error errNo)
                                           (read timeStr :: Int)
                                           (unwords rest)
                                  else Unknown message
                           logMsg "I" timeStr h t = LogMessage Info
                                                    (read timeStr :: Int)
                                                    (unwords (h : t))
                           logMsg "W" timeStr h t = LogMessage Warning
                                                    (read timeStr :: Int)
                                                    (unwords (h : t))
                           logMsg _ _ _ _ = Unknown message

parse :: String -> [LogMessage]
parse stuff = map parseMessage $ lines stuff
              
isLeaf :: MessageTree -> Bool
isLeaf Leaf = True
isLeaf _ = False

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert theMessage Leaf = Node Leaf theMessage Leaf
insert theMessage@(LogMessage _ stamp _) (Node leftTree val@(LogMessage _ stamp2 _) rightTree) =
    if stamp < stamp2
    then if isLeaf leftTree
         then Node (Node Leaf theMessage Leaf) val rightTree
         else Node (insert theMessage leftTree) val rightTree
    else if isLeaf rightTree
         then Node leftTree val (Node Leaf theMessage Leaf)
         else Node leftTree val (insert theMessage rightTree)

build :: [LogMessage] -> MessageTree
build messageList = foldr insert Leaf messageList

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node leftTree message rightTree) = inOrder leftTree ++ message : inOrder rightTree
                                       
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong logList = map getString $ filter errorOver50 $ inOrder $ build logList
    where errorOver50 :: LogMessage -> Bool
          errorOver50 (LogMessage (Error sev) _ _) | sev >= 50 = True
          errorOver50 _ = False
          getString :: LogMessage -> String
          getString (LogMessage _ _ s) = s
