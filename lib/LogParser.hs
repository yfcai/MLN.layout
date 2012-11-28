module LogParser where

import Text.Regex.TDFA
import Data.List(sortBy)
import Data.Maybe(isNothing)

-- ParamHint spewID category [(paramName, paramValue)]
data ParamHint = ParamHint Int String [(String, String)]

instance Show ParamHint where
 show (ParamHint sid cat params) = let
  showPs ((key, val) : ps) = key ++ "\t" ++ val ++ "\n" ++ showPs ps
  showPs [] = "\n"
  in "[" ++ show sid ++ "] " ++ cat ++ "\n" ++ showPs params

-- Match-all operator for regexes
infixl 1 =~*
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


data BoxDimen = BoxDimen {
 boxHeight :: Double, boxDepth :: Double, boxWidth :: Double,
 boxShift  :: Double
} deriving Show

data GlueDimen = GlueDimen {
 glueNatural :: Double, glueStretch :: Surreal, glueShrink :: Surreal
} deriving Show

-- Reals and infinities
data Surreal
 = Real Double
 --       order magnitude
 | Infinity Int Double
 deriving Show

-- Font identifier made comprehensible
-- http://www.forkosh.com/pstex/latexcommands.htm
data FontID = FontID {
 fontEncoding :: String,
 fontFamily   :: String,
 fontSeries   :: String,
 fontShape    :: String,
 fontSize     :: Int
} deriving Show

data PageItem
 -- box dimens   content
 = Hbox BoxDimen [PageItem]
 | Vbox BoxDimen [PageItem]

 --         fontID Char   Optional-description
 | FontChar FontID String String

 --     name   dimension
 | Glue String GlueDimen

 -- We leave abnormal kern types for when math mode got handled
 --     width
 | Kern Double

 -- Cast penalty to a floating point numer from onset
 -- Optimisation will be on floats anyway
 --        penalty
 | Penalty Double

 -- Discretionary word-breaking point
 -- \discretionary{Anfang}{Ende}{Ungetrennt}
 -- Anfang and Ende are children of this node.
 -- Ungetrennt contains tokens to be replaced should a line break
 -- happen at this point. They are not children, but succeeding
 -- siblings of the Disc node. The `replacing` field
 -- contains the number of succeeding siblings belonging to
 -- Ungetrennt. The border between Anfang and Ende is marked
 -- with a DiscSep item. Note that if Ende is empty then no
 -- DiscSep is introduced, and `replacing` is not mentioned
 -- if Ungetrennt is empty.
 | Disc Int [PageItem]

 -- \discretionary{Anfang}{Ende}{Ungetrennt}
 -- Separating {Angang} and {Ende}
 | DiscSep
   deriving Show

data PageItemLine
 --   nesting-level item-declaration
 = PageItemLine Int String
 | DiscSepLine
   deriving Show

-- Maps each PageItemLine with a constructor other than PageItemLine
-- to a specific PageItem. Please keep patterns inexhaustive to detect
-- bugs.
unlinePageItem :: PageItemLine -> PageItem
unlinePageItem DiscSepLine = DiscSep

--                    box-content    to-parse    succeed-or-fail
type PageItemParser = [PageItem]  -> String   -> Maybe PageItem

-- We write name of parser in their declaration for documentation
data PageItemParserDecl = PageItemParserDecl {
 pageItemName   :: String,
 pageItemFreq   :: Double,
 pageItemParser :: PageItemParser
}

-- Regular expressions for positive/negative floats and its capture
regFloat  = "-?[0-9]+\\.[0-9]+"
regCFloat = "(" ++ regFloat ++ ")"

readOptional :: (Read a) => a -> String -> a
readOptional defaultValue s = if null s then defaultValue else read s

-- Converts a float and a filll into a surreal number
-- `fil`  has order-of-infinity = 1
-- `fill` has ..                = 2 etc.
readSurreal :: String -> String -> Surreal
readSurreal real []  = Real (read real)
readSurreal inf  fil = Infinity (length fil - 2) (read inf)

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
 PageItemParserDecl "[HV]box" 5 (\boxContent boxDecl ->
  case boxDecl =~*
   "^([hv])box\\(" ++ regCFloat ++ "\\+" ++ regCFloat ++ "\\)x"
   ++ regCFloat ++ "(, glue set[^,]*)?(, shifted " ++ regCFloat ++ ")?"
  of
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
 PageItemParserDecl "FontChar" 10 (\_ decl ->
  case decl =~*
   "^([A-Z][A-Z0-9]*)/([^/]+)/([^/]+)/([^/]+)/([0-9]+) \
   \([^ ]+)( \\(([^)]+)\\))?"
  of
   [] -> Nothing
   [[_, encoding, family, series, shape, size, char, _, description]] ->
    Just (FontChar (FontID {
     fontEncoding = encoding,
     fontFamily   = family,
     fontSeries   = series,
     fontShape    = shape,
     fontSize     = read size
    }) char description)
 ),

 -- 2: Glue
 PageItemParserDecl "Glue" 8 (\_ decl ->
  case decl =~*
   "^glue(\\(([^)]+)\\))? " ++ regCFloat ++
   "( plus "  ++ regCFloat ++ "(fil+)?)?\
   \( minus " ++ regCFloat ++ "(fil+)?)?"
  of
   [] -> Nothing
   [[_, _, name, natural, _, plus, pfil, _, minus, mfil]] ->
    Just (Glue name GlueDimen {
     glueNatural = read natural,
     glueStretch = if null plus  then Real 0 else readSurreal plus pfil,
     glueShrink  = if null minus then Real 0 else readSurreal minus mfil
    })
 ),

 -- 3: Kern
 --
 -- IGNORES 28.11.12
 -- \kern -23.45 (for accent)
 PageItemParserDecl "Kern" 6 (\_ decl ->
  case decl =~* -- Beware: Space after \kern is optional!
   "^kern( ?" ++ regFloat ++ ")"
  of
   [] -> Nothing
   [[_, size]] -> Just (Kern (read size))
 ),

 -- 4: Penalty
 PageItemParserDecl "Penalty" 4 (\_ decl ->
  case decl =~*
   "^penalty (-?[0-9]+)"
  of
   [] -> Nothing
   [[_, penalty]] -> Just (Penalty (read penalty))
 ),

 -- 5: Disc -- discretionary word-breaking point
 PageItemParserDecl "Disc" 7 (\children decl ->
  case decl =~*
   "^discretionary( replacing ([0-9]+))?"
  of
   [] -> Nothing
   [[_, _, replacing]] -> Just (Disc (readOptional 0 replacing) children)
 ),

 -- 6: DiscSep -- should be unlined

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
splitItemLines :: [String] -> [PageItemLine]
splitItemLines lines = let
 --      lvl    toRead    processed-lines
 loop :: Int -> String -> [PageItemLine] -> [PageItemLine]
 loop n ('.'  : toRead) done = loop (n + 1) toRead done
 loop n ('\\' : toRead) done = PageItemLine n toRead : done
 loop n ('|'  : toRead)
        (DiscSepLine : done) = DiscSepLine : loop (n + 1) toRead done
 loop n ('|'  : toRead) done = DiscSepLine : loop (n + 1) toRead done
 -- An error here signals an unexpected line format
 loop n list done = error list
 in
 foldr (loop 0) [] lines

-- Transforms a list of item-declarations with levels into a list
-- of recognised items. Boxes have their children packed inside.
-- Stuff yet to be parsed are returned.
--
--  level-of-nesting    item-lines         items       other-item-lines
parseItemList :: Int -> [PageItemLine] -> ([PageItem], [PageItemLine])
parseItemList _ [] = ([], [])
parseItemList nestedLvl allLines@(thisLine : otherLines) = case thisLine of
 -- Start parsing if the line contains textual information
 PageItemLine itemLvl itemDecl ->
  if itemLvl == nestedLvl
  then let
   (children , afterChildren  ) = parseItemList (nestedLvl + 1) otherLines
   (siblings , fromNextLevel  ) = parseItemList nestedLvl fromNextSibling
   (prependTo, fromNextSibling) = case tryPageItemParsers children itemDecl
    of Nothing       -> (id          , otherLines   )
       Just thisItem -> ((thisItem :), afterChildren)
   in  (prependTo siblings, fromNextLevel)
  else ([]                , allLines     ) -- itemLvl != nestedLvl
 -- If the line has no text (e. g. is a separator), then map it directly
 otherwise -> let
  (items, unprocessedLines) = parseItemList nestedLvl otherLines
  in (unlinePageItem thisLine : items, unprocessedLines)

extractBoxes :: String -> [PageItem]
extractBoxes text = let
 dumpPars  = "Completed box being shipped out.*\n.*\n(.+\n)*"
 textBox   = text =~ dumpPars :: String
 itemLines = splitItemLines (joinIntoItems (drop 2 (lines textBox)))
 in
 case parseItemList 1 itemLines of
  (items, []) -> items
  (items, ls) -> error (unlines (map show (take 20 ls)))
 -- Error is triggered when parseItemList fails to eat everything,
 -- due either to a bug or to being invoked on an incorrect level.

-- TeX wraps lines of log when it becomes longer than a certain number of
-- chars. We undo it, knowing that the first line of every item starts
-- with either a dot (backslash can be present in wrapped-over lines, too).
-- This places the burden of making all document items not of top level,
-- via e. g. placing them in a \vbox.
joinIntoItems :: [String] -> [String]
joinIntoItems (x : []) = [x]
joinIntoItems (x : xs) = let
 nextItem : otherItems = joinIntoItems xs
 joined   = (x ++ nextItem) : otherItems
 splitted = x : nextItem : otherItems
 in if (head nextItem) == '.' then splitted else joined
joinIntoItems [] = []
