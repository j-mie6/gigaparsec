{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}
module Text.Gigaparsec.Token.Lexer (Lexer, mkLexer, lexeme, nonlexeme) where

import Text.Gigaparsec (Parsec, eof, void, empty, (<|>))
import Text.Gigaparsec.Char (satisfy)
import Text.Gigaparsec.Combinator (skipMany)
import Text.Gigaparsec.Token.Descriptions
import Control.Exception (Exception, throw)
import Text.Gigaparsec.Registers (put, get, localWith, rollback)
import System.IO.Unsafe (unsafePerformIO)
import Data.IORef (newIORef)
import Text.Gigaparsec.Internal.RT (fromIORef)
import Control.Monad (join)

type Lexer :: *
data Lexer = Lexer { lexeme :: !Lexeme
                   , nonlexeme :: !Lexeme
                   , fully :: !(forall a. Parsec a -> Parsec a)
                   , space :: !Space
                   }

mkLexer :: LexicalDesc -> Lexer
mkLexer LexicalDesc{..} = Lexer {..}
  where lexeme = Lexeme { apply = id
                        }
        nonlexeme = NonLexeme {}
        fully' p =  whiteSpace space *> p <* eof
        fully p
          | whitespaceIsContextDependent spaceDesc = initSpace space *> fully' p
          | otherwise                              = fully' p
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
                   , alter :: forall a. CharPredicate -> Parsec a -> Parsec a
                   , initSpace :: Parsec ()
                   }

mkSpace :: SpaceDesc -> Space
mkSpace desc@SpaceDesc{..} = Space {..}
  where -- don't think we can trust doing initialisation here, it'll happen in some random order
        {-# NOINLINE wsImpl #-}
        !wsImpl = fromIORef (unsafePerformIO (newIORef (error "uninitialised space")))
        comment = empty @Parsec @() -- FIXME:
        implOf
          | supportsComments desc = maybe skipComments (skipMany . (<|> comment) . void . satisfy)
          | otherwise             = maybe empty (skipMany . satisfy)
        configuredWhitespace = implOf space
        whiteSpace
          | whitespaceIsContextDependent = join (get wsImpl)
          | otherwise                    = configuredWhitespace
        skipComments = skipMany comment
        alter p
          | whitespaceIsContextDependent = rollback wsImpl . localWith wsImpl (implOf p)
          | otherwise                    = throw (UnsupportedOperation badAlter)
        initSpace
          | whitespaceIsContextDependent = put wsImpl configuredWhitespace
          | otherwise                    = throw (UnsupportedOperation badInit)
        badInit = "whitespace cannot be initialised unless `spaceDesc.whitespaceIsContextDependent` is True"
        badAlter = "whitespace cannot be altered unless `spaceDesc.whitespaceIsContextDependent` is True"

supportsComments :: SpaceDesc -> Bool
supportsComments SpaceDesc{..} = not (null commentLine && null commentStart)

type UnsupportedOperation :: *
newtype UnsupportedOperation = UnsupportedOperation String deriving stock Eq
instance Show UnsupportedOperation where
  show (UnsupportedOperation msg) = "unsupported operation: " ++ msg
instance Exception UnsupportedOperation
