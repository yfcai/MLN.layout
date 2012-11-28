module Main where

import LogParser

-- Provisional tests
-- =================

logFile = "/Users/yfcai/sc/MLN.layout/experiment/t.log"

testParams = do
 x <- readFile logFile
 let y = extractParams x
 putStr (concatMap show y)

testBoxes = do
 x <- readFile logFile
 let y = extractBoxes x
 print y

main = do
 x <- readFile logFile
 let y = extractBoxes x
 print (length y)
