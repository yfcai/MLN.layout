module LogParser where

import Text.Regex.TDFA
import Data.List(sortBy)
import Data.Maybe(isNothing)

logFile = "/Users/yfcai/sc/MLN.layout/experiment/t.log"

-- ParamHint spewID category [(paramName, paramValue)]
data ParamHint = ParamHint Int String [(String, String)]

instance Show ParamHint where
 show (ParamHint sid cat params) = let
  showPs ((key, val) : ps) = key ++ "\t" ++ val ++ "\n" ++ showPs ps
  showPs [] = "\n"
  in "[" ++ show sid ++ "] " ++ cat ++ "\n" ++ showPs params

-- Match-all operator for regexes
(=~*) :: String -> String -> [[String]]
(=~*) = (=~)

-- extractParams :: String -> [ParamHint]
extractParams text = let
 paramPars = "^\\[([0-9]+)\\] (.+)\n(([^ ]+) (.+)\n)*"
 format :: [String] -> ParamHint
 format (_ : i : c : ps) = ParamHint (read i) c (formatPs ps)
 formatPs (_ : key : val : ps) = (key, val) : formatPs ps
 formatPs [] = []
 in map format (text =~* paramPars)

testParams = do
 x <- readFile logFile
 let y = extractParams x
 putStr (concatMap show y)



data BoxDimen = BoxDimen {
 boxHeight :: Double, boxDepth :: Double, boxWidth :: Double,
 boxShift  :: Double
} deriving Show

data GlueDimen = GlueDimen {
 glueNatural :: Double, glueStretch :: Double, glueShrink :: Double
} deriving Show

              --xbox dimens   content
data PageItem = Hbox BoxDimen [PageItem]
              | Vbox BoxDimen [PageItem]
              --FontChar fontID char
              | FontChar String Char
              --Glue name[optional] dimension
              | Glue (Maybe String) GlueDimen
              --We leave abnormal kern types for when math mode got handled
              --Kern width
              | Kern Double
              --Cast penalty to a floating point numer from onset
              --Optimisation will be on floats anyway
              --Penalty penalty
              | Penalty Double
                deriving Show
-- TODO: discretionary replacing

--                    box-content    to-parse    succeed-or-fail
type PageItemParser = [PageItem]  -> String   -> Maybe PageItem

-- We write name of parser in their declaration for documentation
data PageItemParserDecl = PageItemParserDecl {
 pageItemName   :: String,
 pageItemFreq   :: Double,
 pageItemParser :: PageItemParser
}

regFloat  = "[0-9]+\\.[0-9]+"
regCFloat = "(" ++ regFloat ++ ")"

readOptional :: (Read a) => a -> String -> a
readOptional defaultValue s = if null s then defaultValue else read s

-- Define parsers in a list,
-- so you have no opportunity to forget to use new ones.
-- The list will later be sorted according to frequency.
-- Do not rely on position to parse things bzw. define
-- no parser that relies on other parsers' failure to
-- succeed.
--
-- To ponder: Is it possible to abstract commonalities of the parsers?
pageItemParserDecls :: [PageItemParserDecl]
pageItemParserDecls = [

 -- 0: HBox and Vbox
 --
 -- IGNORES 22.11.12
 -- Unset boxes, glue sets, special fields
 PageItemParserDecl "[HV]box" 5 (\boxContent boxDecl -> let
  boxPars =
   "^([hv])box\\(" ++ regCFloat ++ "\\+" ++ regCFloat ++ "\\)x"
   ++ regCFloat ++ "(, glue set[^,]*)?(, shifted " ++ regCFloat ++ ")?"
  in
  case boxDecl =~* boxPars of
   [] -> Nothing
   [[_, hv, height, depth, width, glueSet, shiftStr, shift]] -> let
    cons | hv == "h" = Hbox
         | hv == "v" = Vbox
    dimension = BoxDimen {
     boxHeight = read height,
     boxDepth  = read depth,
     boxWidth  = read width,
     boxShift  = readOptional 0.0 shift
    }
    in Just (cons dimension boxContent)
 ),

 -- 1: FontChar
 -- Every character in source document gives rise to a FontChar item
 -- Raise Inexhaustive-pattern-error when children are present.
 --
 -- IGNORES 22.11.12
 -- CLOBBERED (corrupted pointer), * (char absent in font)
 --
 -- TODO FIXME: Decl format depends on \discretionary!
{-
 PageItemParserDecl "FontChar" 10 (\[] decl ->
  case decl =~* "^(O?T1/[^ ]+) ([^ ]+)( \\( WHAT DO WE PUT HERE?!\\))?" of
   [] -> Nothing
   -- [[_, fontID, 
 ),
-}

 -- Infinity
 -- Last parser handles I-dunno-what-to-do situations.
 -- Change it to show e. g. what log items you're not handling
 -- in order to extend or to correct
 PageItemParserDecl "I-dunno-what-to-do" (-1.0/0.0) (\children thisDecl ->
  Nothing -- For now we do nothing. 21.11.12
  -- error "Unhandled item: " ++ thisDecl
 )]

-- Sort the parsers roughly according to
-- frequency so as to minimise parse time.
sortedPageItemParserDecls :: [PageItemParserDecl]
sortedPageItemParserDecls = sortBy
 -- Note how `compare` have its arguments given in reversed order.
 -- We want to sort in descending order.
 (\decl1 decl2 -> compare (pageItemFreq decl2) (pageItemFreq decl1))
 pageItemParserDecls

pageItemParsers :: [PageItemParser]
pageItemParsers = map pageItemParser sortedPageItemParserDecls

-- Negation of Maybe monad (>>)
(!>>) :: Maybe a -> Maybe a -> Maybe a
(!>>) Nothing    maybe_b = maybe_b
(!>>) a@(Just _) maybe_b = a

-- Try applying pageItemParsers until the first one succeeds
--
--                    children      item-decl
tryPageItemParsers :: [PageItem] -> String    -> Maybe PageItem
tryPageItemParsers children itemDecl = foldl (!>>) Nothing
 (map (\parser -> parser children itemDecl) pageItemParsers)

-- Compute the nested-level and the page-item-declaration
-- from log items such as
--
--     .....\glue(\rightskip) 0.0
--
-- and at the same time insert special strings separating
-- arguments of \discretionary.
-- TODO: implement the above!
-- splitItemLines :: [String] -> [(Int, String)]
splitItemLines lines = let
 --      lvl    toRead    processed-lines                   lvl  decl
 loop :: Int -> String -> [Maybe (Int,  String)] -> [Maybe (Int, String)]
 loop n ('.'  : toRead) done = loop (n + 1) toRead done
 loop n ('\\' : toRead) done = Just (n, toRead) : done
 loop n ('|'  : toRead) (Nothing : done) = Nothing : loop (n+1) toRead done
 loop n ('|'  : toRead) done             = Nothing : loop (n+1) toRead done
 loop n list done = error list
 in
 foldr (loop 0) lines

-- Transforms a list of item-declarations with levels into a list
-- of recognised items. Boxes have their children packed inside.
-- Stuff yet to be parsed are returned.
--
--  level-of-nesting    item-lines          items       other-item-lines
parseItemList :: Int -> [(Int, String)] -> ([PageItem], [(Int, String)])
parseItemList _ [] = ([], [])
parseItemList nestedLvl allLines@((itemLvl, itemDecl) : otherLines) =
 if itemLvl == nestedLvl
 then let
  (children , afterChildren  ) = parseItemList (nestedLvl + 1) otherLines
  (siblings , fromNextLevel  ) = parseItemList nestedLvl fromNextSibling
  (prependTo, fromNextSibling) = case tryPageItemParsers children itemDecl
   of Nothing       -> (id          , otherLines   )
      Just thisItem -> ((thisItem :), afterChildren)
  in  (prependTo siblings, fromNextLevel)
 else ([]                , allLines     ) -- itemLvl != nestedLvl

-- TODO: this still has the wrong type. 21.11.12
extractBoxes :: String -> [PageItem]
extractBoxes text = let
 dumpPars  = "Completed box being shipped out.*\n.*\n(.+\n)*"
 textBox   = text =~ dumpPars :: String
 itemLines = splitItemLines (joinIntoItems (drop 2 (lines textBox)))
 in
 case parseItemList 1 itemLines of
  (items, []) -> items
  (items, ls) -> error (unlines (map show (take 10 ls)))
 -- Error is triggered when parseItemList fails to eat everything,
 -- due either to a bug or to being invoked on an incorrect level.


testBoxes = do
 x <- readFile logFile
 let s = unlines (take 177 (lines x))
     y = extractBoxes s
 putStrLn (unlines (drop 169 (lines s)))
 putStr (unlines (map show y))


-- TeX wraps lines of log when it becomes longer than a certain number of
-- chars. We undo it, knowing that the first line of every item starts
-- with either a dot or a backslash.
joinIntoItems :: [String] -> [String]
joinIntoItems (x : []) = [x]
joinIntoItems (x : xs) = let
 nextItem : otherItems = joinIntoItems xs
 joined   = (x ++ nextItem) : otherItems
 splitted = x : nextItem : otherItems
 in if elem (head nextItem) ".\\" then splitted else joined
joinIntoItems [] = []


