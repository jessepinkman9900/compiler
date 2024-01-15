{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
module Lexer (
    Token(..),
    lexer
) where

import Control.Applicative (Alternative(..), liftA2)
import Control.Monad.Except (ExceptT, MonadError (throwError), runExceptT)
import Control.Monad.State (State, gets, modify', runState)
import Data.Char (isAlpha, isDigit, isLower, isSpace, isUpper)
import Data.List (foldl', foldl1')
import Data.Maybe (listToMaybe, maybeToList)
import Ourlude
import GHC.IO.Handle (NewlineMode(inputNL))

data LexerError
    = Unexpected Char
    | UnexpectedEOF
    | UnmatchedLayout
    deriving()

unexpected :: String -> LexerError
unexpected [] = UnexpectedEOF
unexpected (c:_) = Unexpected c

data Token = NoTokensYet deriving (Eq, Show)

newtype Lexer a = Lexer {
    runLexer :: String -> Either LexerError (a, String)
}

satisfies :: (Char -> Bool) -> Lexer Char
satisfies p = 
    Lexer <| \case
        c : cs | p c -> Right (c, cs)
        rest -> Left (unexpected rest)

char :: Char -> Lexer Char
char target = satisfies (== target)


instance Functor Lexer where
    fmap f (Lexer l) = Lexer (l >>> fmap (first f))

instance Applicative Lexer where
    pure a = Lexer (\input -> Right (a, input))
    Lexer lF <*> Lexer lA =
        Lexer <| \input -> do
            (f, rest) <- lF input
            (a, s) <- lA rest
            return (f a, s)






lexer :: String -> Either LexerError [Token]
lexer _ = Left Err

