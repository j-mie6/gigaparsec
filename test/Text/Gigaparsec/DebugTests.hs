{-# LANGUAGE OverloadedStrings #-}
module Text.Gigaparsec.DebugTests where

import Test.Tasty
import Test.Tasty.HUnit

import Text.Gigaparsec
import Text.Gigaparsec.Char
import Text.Gigaparsec.Debug

import Text.Gigaparsec.Internal qualified as Internal
import Text.Gigaparsec.Internal.Errors qualified as Internal
import Text.Gigaparsec.Internal.RT qualified as Internal

import System.IO
import Data.Knob
import Data.Knob qualified as Knob (getContents)

import Data.ByteString.Char8 (unpack)
import Data.List (isInfixOf, isPrefixOf)

{-
This is quite interesting, because testing the debug combinator is a bit
odd. We need to redirect the standard IO channels to point to our own
implementations, and then analyse the input to look for key indicators
its working without testing the exact formats (which is not useful).
-}

tests :: TestTree
tests = testGroup "Debug" [
    debugTests
  ]

debugTests :: TestTree
debugTests = testGroup "debug should"
  [ testCase "have correct depth when nested" do
      output <- mockDebug "a\r\n \t" $ \config ->
        debugWith config "outer" (debugWith config "inner" item)
      -- a line containing inner should have more leading spaces than
      -- one containing outer
      let ls = lines output
      let outers = filter (isInfixOf "outer") ls
      let inners = filter (isInfixOf "inners") ls
      assertBool "all outer lines should have no leading spaces" $
        not (any (isPrefixOf " ") outers)
      assertBool "all inner lines should have leading spaces" $
        all (isPrefixOf " ") inners
  , testCase "don't have line breaks in the input string" do
      output <- mockDebug "\n\n\n\n\n\n" $ \config ->
        debugWith config "letter" letter
      let ls = lines output
      length ls @?= 4
  -- TODO: watched registers never boil
  ]

ioParse :: Parsec a -> String -> IO ()
ioParse (Internal.Parsec p) inp = Internal.rtToIO $ p (Internal.emptyState inp) good bad
  where good :: a -> Internal.State -> Internal.RT ()
        good _ _  = return ()
        bad :: Internal.Error -> Internal.State -> Internal.RT ()
        bad _ _ = return ()

mockDebug :: String -> (DebugConfig -> Parsec a) -> IO String
mockDebug input f =
  do mock <- newKnob ""
     hMock <- newFileHandle mock "mock" WriteMode
     let config = debugConfig { handle = hMock }
     ioParse (f config) input
     hClose hMock
     unpack <$> Knob.getContents mock
