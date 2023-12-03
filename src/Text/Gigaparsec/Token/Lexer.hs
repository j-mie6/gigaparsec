{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}
module Text.Gigaparsec.Token.Lexer (
    Lexer, mkLexer,
    lexeme, nonlexeme, fully, space,
    skipComments, whiteSpace, alter, initSpace,
    apply
  ) where

import Text.Gigaparsec (Parsec, eof, void, empty, (<|>), atomic, unit)
import Text.Gigaparsec.Char (satisfy, string, item, endOfLine)
import Text.Gigaparsec.Combinator (skipMany, manyTill)
import Text.Gigaparsec.Token.Descriptions qualified as Desc
import Control.Exception (Exception, throw)
import Text.Gigaparsec.Registers (put, get, localWith, rollback)
import System.IO.Unsafe (unsafePerformIO)
import Data.IORef (newIORef)
import Text.Gigaparsec.Internal.RT (fromIORef)
import Control.Monad (join, guard)
import Text.Gigaparsec.Internal.Require (require)
import Data.List (isPrefixOf)

type Lexer :: *
data Lexer = Lexer { lexeme :: !Lexeme
                   , nonlexeme :: !Lexeme
                   , fully :: !(forall a. Parsec a -> Parsec a)
                   , space :: !Space
                   }

mkLexer :: Desc.LexicalDesc -> Lexer
mkLexer Desc.LexicalDesc{..} = Lexer {..}
  where lexeme = Lexeme { apply = id
                        }
        nonlexeme = NonLexeme {}
        fully' p =  whiteSpace space *> p <* eof
        fully p
          | Desc.whitespaceIsContextDependent spaceDesc = initSpace space *> fully' p
          | otherwise                                   = fully' p
        space = mkSpace spaceDesc

type Lexeme :: *
data Lexeme = Lexeme
                { apply :: !(forall a. Parsec a -> Parsec a) -- this is tricky...
                }
            | NonLexeme
                {
                }

type Space :: *
data Space = Space { whiteSpace :: !(Parsec ())
                   , skipComments :: !(Parsec ())
                   , alter :: forall a. Desc.CharPredicate -> Parsec a -> Parsec a
                   , initSpace :: Parsec ()
                   }

mkSpace :: Desc.SpaceDesc -> Space
mkSpace desc@Desc.SpaceDesc{..} = Space {..}
  where -- don't think we can trust doing initialisation here, it'll happen in some random order
        {-# NOINLINE wsImpl #-}
        !wsImpl = fromIORef (unsafePerformIO (newIORef (error "uninitialised space")))
        comment = commentParser desc -- do not make this lazy
        implOf
          | supportsComments desc = maybe skipComments (skipMany . (<|> comment) . void . satisfy)
          | otherwise             = maybe empty (skipMany . satisfy)
        !configuredWhitespace = implOf space
        !whiteSpace
          | whitespaceIsContextDependent = join (get wsImpl)
          | otherwise                    = configuredWhitespace
        !skipComments = skipMany comment
        alter p
          | whitespaceIsContextDependent = rollback wsImpl . localWith wsImpl (implOf p)
          | otherwise                    = throw (UnsupportedOperation badAlter)
        initSpace
          | whitespaceIsContextDependent = put wsImpl configuredWhitespace
          | otherwise                    = throw (UnsupportedOperation badInit)
        badInit = "whitespace cannot be initialised unless `spaceDesc.whitespaceIsContextDependent` is True"
        badAlter = "whitespace cannot be altered unless `spaceDesc.whitespaceIsContextDependent` is True"

{-
We have the following invariances to be checked up front:
  * at least one kind of comment must be enabled
  * the starts of line and multiline must not overlap

-- TODO: needs error messages put in
-- TODO: optimise manyTill?
-- TODO: remove guard, configure properly
-}
commentParser :: Desc.SpaceDesc -> Parsec ()
commentParser Desc.SpaceDesc{..} =
  require (multiEnabled || singleEnabled) "skipComments" noComments $
    require (not (multiEnabled && isPrefixOf commentStart commentLine)) "skipComments" noOverlap $
      void (multiLine <|> singleLine)
  where
    -- can't make these string until guard is gone
    openComment = atomic (string commentStart)
    closeComment = atomic (string commentEnd)
    multiLine = guard multiEnabled *> openComment *> wellNested 1
    wellNested :: Int -> Parsec ()
    wellNested 0 = unit
    wellNested n = closeComment *> wellNested (n - 1)
               <|> guard nestedComments *> openComment *> wellNested (n + 1)
               <|> item *> wellNested n -- TODO: can this loop be tightened? first characters?
    singleLine = guard singleEnabled
              *> atomic (string commentLine)
              *> void (manyTill item endOfLineComment)

    endOfLineComment = if commentLineAllowsEOF then void endOfLine <|> eof else void endOfLine

    multiEnabled = not (null commentStart || null commentEnd)
    singleEnabled = not (null commentLine)
    noComments = "one of single- or multi-line comments must be enabled"
    noOverlap = "single-line comments must not overlap with multi-line comments"

supportsComments :: Desc.SpaceDesc -> Bool
supportsComments Desc.SpaceDesc{..} = not (null commentLine && null commentStart)

type UnsupportedOperation :: *
newtype UnsupportedOperation = UnsupportedOperation String deriving stock Eq
instance Show UnsupportedOperation where
  show (UnsupportedOperation msg) = "unsupported operation: " ++ msg
instance Exception UnsupportedOperation
