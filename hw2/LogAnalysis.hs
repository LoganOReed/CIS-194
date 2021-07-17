{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

-- Takes a validated string and returns a LogMessage type
listToLogMessage :: [String] -> LogMessage
    
listToLogMessage ("I":xs) =
    LogMessage Info time str
    where
        time = read (head xs)::Int
        str  = unwords $ drop 1 xs

listToLogMessage ("W":xs) =
    LogMessage Warning time str
    where
        time = read (head xs)::Int
        str  = unwords $ drop 1 xs

listToLogMessage ("E":n:xs) = 
    LogMessage (Error severity) time str
    where 
        severity = read n :: Int
        time     = read (head xs)::Int
        str      = unwords $ drop 1 xs

listToLogMessage str = 
    Unknown $ unwords str


-- Parses a string into the LogMessage Data Structure
parseMessage :: String -> LogMessage

parseMessage str =
    listToLogMessage $ words str

-- Parses an entire file
parse :: String -> [LogMessage]

parse str =
    parseMessage taken : parse (unlines untaken)
    where
        lin = lines str
        taken = head lin
        untaken = drop 1 lin

-- BST PART OF THE ASSIGNMENT

-- Adds a LogMessage into a binary search tree
insert :: LogMessage -> MessageTree -> MessageTree

insert (Unknown _) tree = 
    tree

insert msg@(LogMessage _ _ _) Leaf = 
    Node Leaf msg Leaf

insert msg@(LogMessage _ time _) (Node left (LogMessage _ nTime _) right)
    | time < nTime = 
        insert msg left
    | otherwise    =
        insert msg right

insert _ tree = tree

-- takes a list of messages and constructs a bst
build :: [LogMessage] -> MessageTree

build [] = Leaf
build (x:xs) = insert x (build xs)

-- takes a sorted message tree and returns the bst in order
inOrder :: MessageTree -> [LogMessage]

inOrder Leaf  = []

inOrder (Node left msg right) = 
    inOrder left ++ [msg] ++ inOrder right