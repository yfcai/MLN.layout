{-# OPTIONS_GHC -i/Users/yfcai/sc/MLN.layout/lib #-}

-- Gathering font information from TeX log

module Gather where

import LogParser
import Data.Char(chr)

header = "\\showboxbreadth=99999\n\\showboxdepth=99999\n\\makeatletter\n"

-- Create a box wrapping the character denoted by its ASCII code
-- in a box and show it under the supplied font so as to gather
-- of the dimensions of the character box.
charBox :: (FontID, Int) -> String
charBox (f, c) =
 "\\setbox0=\\vbox{\\hbox{\
 \\\fontencoding{" ++ fontEncoding f ++ "}\
 \\\fontfamily{" ++ fontFamily f ++ "}\
 \\\fontseries{" ++ fontSeries f ++ "}\
 \\\fontshape{" ++ fontShape f ++ "}\
 \\\fontsize{" ++ size ++ "}{" ++ size ++ "}\
 \\\selectfont\n"
 ++ printChar c ++
 "}}\\ht0=0pt \\dp0=0pt \\wd0=0pt \\showbox0\n"
 where size = show (fontSize f)

-- Converts n to chr(n) a la TeX.
-- Chars between 0 and 31 are represented by the char it would
-- become when its first bit is flipped prefixed by ^^.
--
-- Please be careful with active characters of TeX.
printChar :: Int -> String
printChar n | n <    40 = ['^', '^', chr (n + 64)]
            | otherwise = case n of
                    92  -> error "Backslash!"
                    94  -> "\\textasciicircum"
                    95  -> "\\textunderscore"
                    123 -> "\\{"
                    124 -> "\\}"
                    126 -> "\\textasciitilde"
                    127 -> "^^?"
                    _   -> chr n : []

encodings = ["OT1"]
families = ["cmtt", "cmr"]
series = ["m", "bx"]
shapes = ["n", "it"]
sizes = [5, 8, 10, 12]
asciis = [0..91] ++ [93..127]

allChars = [(FontID e f s h i, a) |
 e <- encodings,
 f <- families,
 s <- series,
 h <- shapes,
 i <- sizes,
 a <- asciis]

texcode = header ++ concatMap charBox allChars
