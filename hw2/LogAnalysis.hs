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