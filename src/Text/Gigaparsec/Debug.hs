{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE ExistentialQuantification, RecordWildCards, NamedFieldPuns #-}
module Text.Gigaparsec.Debug (debug, debugWith, debugConfig, DebugConfig(..), WatchedReg(..), Break(..)) where

import Text.Gigaparsec.Internal (Parsec)
import Text.Gigaparsec.Internal.RT (Reg, readReg)
import Text.Gigaparsec.Internal qualified as Internal
import Text.Gigaparsec.Internal.RT qualified as Internal

import Control.Monad (when, forM)
import System.IO (hGetEcho, hSetEcho, stdin)
import Data.List (intercalate, isPrefixOf)
import Data.List.NonEmpty (NonEmpty((:|)), (<|))
import Data.List.NonEmpty qualified as NonEmpty (toList)
import System.Console.Pretty (color, supportsPretty, Color(Green, White, Red, Blue))

type DebugConfig :: *
data DebugConfig = DebugConfig {
    ascii :: !Bool,
    breakPoint :: !Break,
    watchedRegs :: ![WatchedReg]
  }

debugConfig :: DebugConfig
debugConfig = DebugConfig { ascii = False, breakPoint = Never, watchedRegs = [] }

type WatchedReg :: *
data WatchedReg = forall r a. Show a => WatchedReg String (Reg r a)

type Break :: *
data Break = OnEntry | OnExit | Always | Never

debug :: String -> Parsec a -> Parsec a
debug = debugWith debugConfig

debugWith :: DebugConfig -> String -> Parsec a -> Parsec a
debugWith config@DebugConfig{ascii} name (Internal.Parsec p) = Internal.Parsec $ \st good bad -> do
  -- TODO: could make it so the postamble can print input information from the entry?
  ascii' <- (\colourful -> ascii || not colourful) <$> Internal.ioToRT supportsPretty
  let config' = config { ascii = ascii' }
  doDebug name Enter st ""  config'
  let good' x st' = do
        let st'' = st' { Internal.debugLevel = Internal.debugLevel st' - 1}
        doDebug name Exit st'' (green ascii' " Good") config'
        good x st''
  let bad' err st' = do
        let st'' = st' { Internal.debugLevel = Internal.debugLevel st' - 1}
        doDebug name Exit st'' (red ascii' " Bad") config'
        bad err st''
  p (st { Internal.debugLevel = Internal.debugLevel st + 1}) good' bad'

type Direction :: *
data Direction = Enter | Exit

breakOnEntry :: Break -> Bool
breakOnEntry OnEntry = True
breakOnEntry Always  = True
breakOnEntry _       = False

breakOnExit :: Break -> Bool
breakOnExit OnExit = True
breakOnExit Always = True
breakOnExit _      = False

shouldBreak :: Direction -> Break -> Bool
shouldBreak Enter = breakOnEntry
shouldBreak Exit = breakOnExit

doDebug :: String -> Direction -> Internal.State -> String -> DebugConfig -> Internal.RT ()
doDebug name dir st end DebugConfig{..} = do
  printInfo name dir st end ascii watchedRegs
  when (shouldBreak dir breakPoint) waitForUser

printInfo :: String -> Direction -> Internal.State -> String -> Bool -> [WatchedReg] -> Internal.RT ()
printInfo name dir st@Internal.State{input, line, col} end ascii regs = do
  let cs = replace "\n" (newline ascii)
         . replace " " (space ascii)
         . replace "\r" (carriageReturn ascii)
         . replace "\t" (tab ascii)
         $ take (5 + 1) input
  let cs' = if length cs < (5 + 1) then cs ++ endOfInput ascii else cs
  let prelude = portal dir name ++ " " ++ show (line, col) ++ ": "
  let caret = replicate (length prelude) ' ' ++ blue ascii "^"
  regSummary <-
    if null regs then return []
    else (++ [""]) . ("watched registers:" :) <$>
      forM regs (\(WatchedReg rname reg) ->
        (\x -> "    " ++ rname ++ " = " ++ show x) <$> readReg reg)
  Internal.ioToRT $ putStr $ indentAndUnlines st ((prelude ++ cs' ++ end) : caret : regSummary)

waitForUser :: Internal.RT ()
waitForUser = Internal.ioToRT $ do
  echo <- hGetEcho stdin
  hSetEcho stdin False
  putStrLn "..."
  _ <- getChar
  hSetEcho stdin echo

render :: Direction -> String
render Enter = ">"
render Exit = "<"

portal :: Direction -> String -> String
portal dir name = render dir ++ name ++ render dir

indent :: Internal.State -> String
indent st = replicate (Internal.debugLevel st * 2) ' '

indentAndUnlines :: Internal.State -> [String] -> String
indentAndUnlines st = unlines . map (indent st ++)

green, red, white, blue :: Bool -> String -> String
green = colour Green
red = colour Red
white = colour White
blue = colour Blue

colour :: Color -> Bool -> String -> String
colour _ True s = s
colour c False s = color c s

newline, space, carriageReturn, tab, endOfInput :: Bool -> String
newline ascii = green ascii "↙"
space ascii = white ascii "·"
carriageReturn ascii = green ascii "←"
tab ascii = white ascii "→"
endOfInput ascii = red ascii "•"

replace :: String -> String -> String -> String
replace needle replacement haystack =
  intercalate replacement (NonEmpty.toList (splitOn needle haystack))

splitOn :: String -> String -> NonEmpty String
splitOn pat = go
  where go src
          | isPrefixOf pat src = "" <| go (drop n src)
          | c:cs <- src        = let (w :| ws) = go cs in (c : w) :| ws
          | otherwise          = "" :| []
        n = length pat
