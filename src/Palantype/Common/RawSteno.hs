{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE MultiWayIf #-}

module Palantype.Common.RawSteno where

import           Control.Applicative            ( Applicative((<*), pure, (<*>))
                                                )
import           Control.Category               ( (<<<) )
import           Control.Monad                  ( Monad((>>))
                                                , MonadPlus(mzero)
                                                , guard
                                                , unless
                                                , when
                                                )
import           Data.Data                      ( Proxy(Proxy)

                                                , typeRep
                                                )
import           Data.Either                    ( Either(..) )
import           Data.Eq                        ( Eq((==)) )
import           Data.Foldable                  ( Foldable(foldl') )
import           Data.Function                  ( ($) )
import           Data.Functor                   ( ($>)
                                                , (<$>)
                                                , void
                                                )
import           Data.Maybe                     ( Maybe(..) )
import           Data.Monoid                    ( Monoid(mconcat), (<>) )
import           Data.Ord                       ( Ord((<), (>), (>=)) )
import Data.Text ( Text, isInfixOf )
import qualified Data.Text                     as Text
import           Palantype.EN                   ( pEN )
import           Text.Parsec                    ( (<?>)
                                                , (<|>)
                                                , ParseError
                                                , Parsec
                                                , anyChar
                                                , char
                                                , eof
                                                , getState
                                                , lookAhead
                                                , many1
                                                , noneOf
                                                , oneOf
                                                , runParser
                                                , sepBy1
                                                , setState
                                                , space
                                                , spaces
                                                , try
                                                )
import           Text.Show                      ( Show(show) )
import           TextShow                       ( TextShow(showt)

                                                )
import Control.Monad.Fail (MonadFail(fail))
import Data.List (head)
import Data.Bool ((&&), otherwise)
import Palantype.Common.Class (Palantype (toKeys, toFinger), RawSteno (RawSteno, unRawSteno))
import Palantype.Common.Internal (Chord (Chord), PatternPos (..), Finger (..))
import Palantype.Common.KeyIndex (fromIndex)

fromChord :: forall k . Palantype k => Chord k -> RawSteno
fromChord = RawSteno <<< showt

{-|
Put "/"s between RawSteno in a list
-}
unparts :: [RawSteno] -> RawSteno
unparts rs = RawSteno $ Text.intercalate "/" $ unRawSteno <$> rs

evalStenoPattern :: forall key. Palantype key => RawSteno -> Either Text PatternPos
evalStenoPattern (RawSteno str) =
    case runParser ((,) <$> (sentence @key) <*> getState) (Nothing, Nothing) "raw steno code" str of
        Left  err -> Left $ Text.pack $ show err
        Right (_, (Just last, _))  ->
          let first = getFinger $ Text.head str
          in  Right $ if
                | "/" `isInfixOf` str -> Multiple
                | first < LeftThumb && last < RightIndex -> Onset
                | first >= LeftThumb && last < RightIndex -> Nucleus
                | first >= LeftThumb && last >= RightIndex -> Coda
                | otherwise -> Multiple
        Right (_, (Nothing, _)) -> Left $ "inconsisent result from evalParser for " <> str
  where
    getFinger '-' = RightIndex
    getFinger c = toFinger $ head $ toKeys @key c


-- | parse raw steno code where words are separated by space(s) and
--   chords within one word are separated by '/'
parseSteno :: Palantype key => RawSteno -> Either Text [Chord key]
parseSteno (RawSteno str) =
    case runParser sentence (Nothing, Nothing) "raw steno code" str of
        Left  err -> Left $ Text.pack $ show err
        Right ls  -> Right $ mconcat ls

-- | fails silently in case of parser error and returns
-- | an empty list
parseStenoLenient :: Palantype key => RawSteno -> [Chord key]
parseStenoLenient (RawSteno str) =
    case runParser sentence (Nothing, Nothing) "" str of
        Left  _ -> []
        Right ls  -> mconcat ls

parseWord :: Palantype key => RawSteno -> Either ParseError [Chord key]
parseWord (RawSteno str) = runParser word (Nothing, Nothing) "" str

-- | parse raw steno code, expects a single chords, i.e. no spaces, no '/'
-- | fails silently and returns and returns an empty chord
parseChordLenient :: Palantype key => RawSteno -> Chord key
parseChordLenient (RawSteno str) =
    case runParser chord (Nothing, Nothing) "" str of
        Left  _ -> Chord []
        Right c -> c

parseStenoKey :: Palantype key => RawSteno -> Either Text key
parseStenoKey (RawSteno str) =
    case runParser keyWithHyphen (Nothing, Nothing) "" str of
        Left  err -> Left $ Text.pack $ show err
        Right k   -> Right k

sentence
    :: Palantype key => Parsec Text (Maybe Finger, Maybe key) [[Chord key]]
sentence = spaces >> sepBy1 word (many1 space) <* eof

word :: Palantype key => Parsec Text (Maybe Finger, Maybe key) [Chord key]
word = sepBy1 chord (char '/')

chord :: Palantype key => Parsec Text (Maybe Finger, Maybe key) (Chord key)
chord = do
    setState (Nothing, Nothing)
    ks <- keys
    eof <|> void (lookAhead $ oneOf " /")
    pure $ Chord ks

keys :: Palantype key => Parsec Text (Maybe Finger, Maybe key) [key]
keys = do
    many1 keyWithHyphen

keyWithHyphen :: Palantype key => Parsec Text (Maybe Finger, Maybe key) key
keyWithHyphen = do
    try keyLeftHand <|> keyOrHyphenKey <?> "left hand or key"

keyLeftHand
    :: forall key . Palantype key => Parsec Text (Maybe Finger, Maybe key) key
keyLeftHand = do
    (mFinger, lk) <- getState
    c             <- noneOf " /"
    void $ char '-'
    eof <|> void (lookAhead $ char '/')

    let reach
            :: key -> Parsec Text (Maybe Finger, Maybe key) key
        reach k = do
            let f = toFinger k
            setState (Just f, Just k)
            guard $ if typeRep (Proxy :: Proxy key) == typeRep pEN
                then lk < Just k
                else mFinger < Just f
            guard $ f < RightThumb
            pure k

    foldl' (\p k -> p <|> reach k) (fail "key left hand no reach") (toKeys c)

keyOrHyphenKey :: Palantype key => Parsec Text (Maybe Finger, Maybe key) key
keyOrHyphenKey = do
    (finger, _) <- getState
    c           <- lookAhead $ noneOf " /"
    when (c == '-') $ do
        when (finger < Just RightThumb)
            $ setState (Just LeftThumb, Just $ fromIndex 16)
        void anyChar
    key

key
    :: forall key
     . (Palantype key)
    => Parsec Text (Maybe Finger, Maybe key) key
key = do
    (mFinger, mLastK) <- getState
    c                 <- lookAhead $ noneOf "/ "

    let reach
            :: key -> Parsec Text (Maybe Finger, Maybe key) key
        reach k = do
-- a hacky way to allow multiple keys pressed by one finger
-- for the original palantype system
            if typeRep (Proxy :: Proxy key) == typeRep pEN
                then unless (Just k > mLastK) mzero
                else unless (Just (toFinger k) > mFinger) mzero
            anyChar $> k

    k <- foldl' (\parser k -> parser <|> reach k) mzero $ toKeys c
    setState (Just $ toFinger k, Just k)
    pure k
