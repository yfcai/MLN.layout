{-# OPTIONS_GHC -i/Users/yfcai/sc/MLN.layout/lib #-}

-- Gathering font information from TeX log

module Gather where

import LogParser
import Data.Char(chr)
import Data.List((\\))

import qualified Data.Map.Strict as Map

-- 1. Emitting code to gather fontChar dimensions
-- ==============================================

header = "\\showboxbreadth=99999\n\\showboxdepth=99999\n\\makeatletter\n"

-- Create a box wrapping the character denoted by its ASCII code
-- in a box and show it under the supplied font so as to gather
-- of the dimensions of the character box.
charBox :: (FontID, String) -> String
charBox (f, c) =
 "\\setbox0=\\vbox{\\hbox{\
 \\\fontencoding{" ++ fontEncoding f ++ "}\
 \\\fontfamily{" ++ fontFamily f ++ "}\
 \\\fontseries{" ++ fontSeries f ++ "}\
 \\\fontshape{" ++ fontShape f ++ "}\
 \\\fontsize{" ++ size ++ "}{" ++ size ++ "}\
 \\\selectfont\n"
 ++ c ++
 "}}\\ht0=0pt \\dp0=0pt \\wd0=0pt \\showbox0\n"
 where size = show (fontSize f)

-- Converts n to chr(n) a la TeX.
-- Chars between 0 and 31 are represented by the char it would
-- become when its first bit is flipped prefixed by ^^.
--
-- Please be careful with active characters of TeX.
printChar :: Int -> String
printChar n | n <    40 = ['^', '^', chr (n + 64)]
            | otherwise = chr n : []

badChars :: [Int] -- things that cause TeX trouble
badChars = [0..31] ++ [ -- Problematic: unable to type ligatures!
 32, 34, 35, 36, 37, 38,
 63, 92, 94, 95, 96,
 123, 124, 125, 126, 127]


encodings = ["OT1"]
families = ["cmtt", "cmr"]
series = ["m", "bx"]
shapes = ["n", "it"]
sizes = [5, 8, 10, 12]
asciis = additionals ++ map printChar ([0..127] \\ badChars)

additionals = [
 "fi", "ffi"
 ]

allChars = [(FontID e f s h i, a) |
 e <- encodings,
 f <- families,
 s <- series,
 h <- shapes,
 i <- sizes,
 a <- asciis]

texcode = header ++ concatMap charBox allChars

-- 2. Read the log to understand the dimensions
-- ============================================

fontMap :: Map (String, FontID) BoxDimen
fontMap = error "TODO: Implement me!"
