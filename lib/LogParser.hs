module LogParser where

import Text.Regex.TDFA
import Data.List(sortBy)

logFile = "/Users/yfcai/sc/MLN.layout/experiment/t.log"

-- ParamHint spewID category [(paramName, paramValue)]
data ParamHint = ParamHint Int String [(String, String)]

instance Show ParamHint where
 show (ParamHint sid cat params) = let
  showPs ((key, val) : ps) = key ++ "\t" ++ val ++ "\n" ++ showPs ps
  showPs [] = "\n"
  in "[" ++ show sid ++ "] " ++ cat ++ "\n" ++ showPs params

-- extractParams :: String -> [ParamHint]
extractParams text = let
 paramPars = "^\\[([0-9]+)\\] (.+)\n(([^ ]+) (.+)\n)*"
 matchRslt = (text =~ paramPars) :: [[String]]
 format :: [String] -> ParamHint
 format (_ : i : c : ps) = ParamHint (read i) c (formatPs ps)
 formatPs (_ : key : val : ps) = (key, val) : formatPs ps
 formatPs [] = []
 in map format matchRslt

testParams = do
 x <- readFile logFile
 let y = extractParams x
 putStr (concatMap show y)



data BoxDimen = BoxDimen {
 boxHeight :: Double, boxDepth :: Double, boxWidth :: Double
}

data GlueDimen = GlueDimen {
 glueNatural :: Double, glueStretch :: Double, glueShrink :: Double
}

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

--                    box-content    to-parse    succeed-or-fail
type PageItemParser = [PageItem]  -> String   -> Maybe PageItem

-- We write name of parser in their declaration for documentation
data PageItemParserDecl = PageItemParserDecl {
 pageItemName   :: String,
 pageItemFreq   :: Double,
 pageItemParser :: PageItemParser
}

-- Define parsers in a list,
-- so you have no opportunity to forget to use new ones.
-- The list will later be sorted according to frequency.
-- Do not rely on position to parse things bzw. define
-- no parser that relies on other parsers' failure to
-- succeed.
pageItemParserDecls :: [PageItemParserDecl]
pageItemParserDecls = [
 -- 0
 PageItemParserDecl "[HV]box" 5 (\boxContent boxDecl -> let
  boxPars = -- Ignore unset boxes, shifts and special fields (21.11.12)
   "^([hv])box\\(([0-9]*\\.[0-9]*)\\+([0-9]*\\.[0-9]*)\\)x([0-9]*\\.[0-9]*)"
  fields = tail (boxDecl =~ boxPars) :: [[String]]
  in
  case fields of
   [] -> Nothing
   [[hv, height, depth, width]] -> let
    cons | hv == "h" = Hbox
         | hv == "v" = Vbox
    dimension = BoxDimen {
     boxHeight = read height,
     boxDepth  = read depth,
     boxWidth  = read width
    }
    in Just (cons dimension boxContent)
 ),
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
pageItemParsers :: [PageItemParser]
pageItemParsers = map pageItemParser (sortBy
 (\decl1 decl2 -> compare (pageItemFreq decl1) (pageItemFreq decl2)
 ) pageItemParserDecls)

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
-- from a log item such as
--
--     .....\glue(\rightskip) 0.0
--
splitItemDecl :: String -> (Int, String)
splitItemDecl = loop 0 where
 --      current-level    to-read    level declaration
 loop :: Int           -> String -> (Int,  String)
 loop n ('.'  : toRead) = loop (n + 1) toRead
 loop n ('\\' : toRead) = (n, toRead)

-- Transforms a list of item-declarations (strings) into a list
-- of recognised items. Boxes have their children packed inside.
-- Stuff yet to be parsed are returned.
--
--  level-of-nesting    item-list     items       rest-of-item-list
parseItemList :: Int -> [String]  -> ([PageItem], [String])
parseItemList _ [] = ([], [])
parseItemList nestedLevel (itemDecl : otherItems) = let
 (itemLevel, itemDecl ) = splitItemDecl itemDecl
 (children , afterThem) = parseItemList (nestedLevel + 1) otherItems
 in error "yo!"

-- TODO: Ponder this.
-- Boxes have children.
-- Other page items have not.
-- Wherein lies the responsibility to decide
-- the list on which to continue parsing,
-- whether it should be `otherItems` or `afterThem`?

-- TODO: this still has the wrong type. 21.11.12
-- extractBoxes :: String -> [PageItem]
extractBoxes text = let
 dumpPars = "Completed box being shipped out.*\n.*\n(.+\n)*"
 textBox  = text =~ dumpPars :: String
 lineList = drop 2 (lines textBox)
 itemList = joinIntoItems lineList
 in
 itemList

testBoxes = do
 x <- readFile logFile
 let y = extractBoxes x
 putStr (unlines (map (take 20) y))


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


