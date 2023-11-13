{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE ExistentialQuantification, RecordWildCards, NamedFieldPuns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
module Text.Gigaparsec.Debug (debug, debugConfig, DebugConfig(..), WatchedReg(..), Break(..)) where

import Text.Gigaparsec (Parsec)
import Text.Gigaparsec.Registers (Reg)
import Text.Gigaparsec.Internal qualified as Internal
import Text.Gigaparsec.Internal.RT qualified as Internal

import Control.Monad (when)
import System.IO (hGetEcho, hSetEcho, stdin)
import Data.List (intercalate, isPrefixOf)
import Data.List.NonEmpty (NonEmpty((:|)), (<|))
import Data.List.NonEmpty qualified as NonEmpty (toList)
import System.Console.Pretty (color, Color(Green, White, Red, Blue))

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

debug :: String -> DebugConfig -> Parsec a -> Parsec a
debug name config@DebugConfig{ascii} (Internal.Parsec p) = Internal.Parsec $ \st good bad -> do
  -- TODO: could make it so the postamble can print input information from the entry?
  doDebug name Enter st "" config
  let good' x st' = do
        let st'' = st' { Internal.debugLevel = Internal.debugLevel st' - 1}
        doDebug name Exit st'' (green ascii " Good") config
        good x st''
  let bad' err st' = do
        let st'' = st' { Internal.debugLevel = Internal.debugLevel st' - 1}
        doDebug name Exit st'' (red ascii " Bad") config
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
printInfo name dir st@Internal.State{input, line, col} end ascii _regs = Internal.ioToRT $ do
  let cs = replace "\n" (newline ascii)
         . replace " " (space ascii)
         . replace "\r" (carriageReturn ascii)
         . replace "\t" (tab ascii)
         $ take (5 + 1) input
  let cs' = if length cs < (5 + 1) then cs ++ endOfInput ascii else cs
  let prelude = portal dir name ++ " " ++ show (line, col) ++ ": "
  let caret = replicate (length prelude) ' ' ++ blue ascii "^"
  let regSummary = [] -- TODO: something with the register summary
  putStr $ indentAndUnlines st ((prelude ++ cs' ++ end) : caret : regSummary)

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

green :: Bool -> String -> String
green True s = s
green False s = color Green s

red :: Bool -> String -> String
red True s = s
red False s = color Red s

white :: Bool -> String -> String
white True s = s
white False s = color White s

blue :: Bool -> String -> String
blue True s = s
blue False s = color Blue s

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
