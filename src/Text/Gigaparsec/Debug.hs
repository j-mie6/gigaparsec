{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE ExistentialQuantification, NamedFieldPuns #-}
{-|
Module      : Text.Gigaparsec.Debug
Description : This module contains the very useful debugging combinator, as well as breakpoints.
License     : BSD-3-Clause
Maintainer  : Jamie Willis, Gigaparsec Maintainers
Stability   : stable

This module contains the very useful debugging combinators 'debug' and 'debugWith', as well as
breakpoints that can be used to pause parsing execution.

@since 0.2.1.0
-}
module Text.Gigaparsec.Debug (debug, debugWith, debugConfig, DebugConfig(..), WatchedReg(..), Break(..)) where

import Text.Gigaparsec.Internal (Parsec)
import Text.Gigaparsec.Internal.RT (Reg, readReg)
import Text.Gigaparsec.Internal qualified as Internal
import Text.Gigaparsec.Internal.RT qualified as Internal

import Control.Monad (when, forM)
import System.IO (hGetEcho, hSetEcho, hPutStr, stdin, stdout, Handle)
import Data.List (intercalate, isPrefixOf)
import Data.List.NonEmpty (NonEmpty((:|)), (<|))
import Data.List.NonEmpty qualified as NonEmpty (toList)
import System.Console.Pretty (color, supportsPretty, Color(Green, White, Red, Blue))

{-|
Configuration that allows for customising the behaviour of a 'debugWith'
combinator.
-}
type DebugConfig :: *
data DebugConfig = DebugConfig {
    ascii :: !Bool, -- ^ should the output of the combinator be in plain uncoloured ascii?
    breakPoint :: !Break, -- ^ should parsing execution be paused when entering or leaving this combinator?
    watchedRegs :: ![WatchedReg], -- ^ what registers should have their values tracked during debugging?
    handle :: !Handle -- ^ where should the output of the combinator be sent?
  }

{-|
The plain configuration used by the 'debug' combinator itself. It will have coloured
terminal output (if available), never pause the parsing execution, not track any registers,
and print its output to 'stdout'.
-}
debugConfig :: DebugConfig
debugConfig = DebugConfig { ascii = False, breakPoint = Never, watchedRegs = [], handle = stdout }

{-|
This type allows for a specified register to be watched by a debug combinator. The
contents of the register must be 'Show'able, and it should be given a name to identify
it within the print-out. Registers containing different types can be simultaneously
tracked, which is why this datatype is existential.
-}
type WatchedReg :: *
data WatchedReg = forall r a. Show a => WatchedReg String    -- ^ the name of the register
                                                   (Reg r a) -- ^ the register itself

{-|
This is used by 'DebugConfig' to specify whether the parsing should be paused
when passing through a 'debugWith' combinator.
-}
type Break :: *
data Break = OnEntry -- ^ pause the parsing just after entering a debug combinator
           | OnExit  -- ^ pause the parsing just after leaving a debug combinator
           | Always  -- ^ pause the parsing both just after entry and exit of a debug combinator
           | Never   -- ^ do not pause execution when passing through (__default__)

{-|
This combinator allows this parser to be debugged by providing a trace through the execution.

When this combinator is entered, it will print the name assigned to the parser to the console,
as well as the current input context for a few characters that follow.
This parser is then executed. If it succeeded, this combinator again reports the
name along with \"@Good@\" and the input context. If it failed, it reports the name
along with \"@Bad@\" and the input context.
-}
debug :: String -> Parsec a -> Parsec a
debug = debugWith debugConfig

{-|
This combinator allows this parser to be debugged by providing a trace through the execution.
An additional 'DebugConfig' is provided to customise behaviour.

When this combinator is entered, it will print the name assigned to the parser to the
configured handle, as well as the current input context for a few characters that follow.
This parser is then executed. If it succeeded, this combinator again reports the
name along with \"@Good@\" and the input context. If it failed, it reports the name
along with \"@Bad@\" and the input context.

When breakpoints are enabled within the config, the execution of the combinator will pause
on either entry, exit, or both. The parse is resumed by entering any character on standard input.
-}
debugWith :: DebugConfig -> String -> Parsec a -> Parsec a
debugWith config@DebugConfig{ascii} name (Internal.Parsec p) = Internal.Parsec $ \st good bad -> do
  -- TODO: could make it so the postamble can print input information from the entry?
  ascii' <- (\colourful -> ascii || not colourful) <$> Internal.unsafeIOToRT supportsPretty
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

---------------------------------------------
---- INTERNALS

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
  printInfo handle name dir st end ascii watchedRegs
  when (shouldBreak dir breakPoint) waitForUser

printInfo :: Handle -> String -> Direction -> Internal.State -> String -> Bool -> [WatchedReg] -> Internal.RT ()
printInfo handle name dir st@Internal.State{input, line, col} end ascii regs = do
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
  Internal.unsafeIOToRT $
    hPutStr handle $ indentAndUnlines st ((prelude ++ cs' ++ end) : caret : regSummary)

waitForUser :: Internal.RT ()
waitForUser = Internal.unsafeIOToRT $ do
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
