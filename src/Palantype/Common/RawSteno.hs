{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TemplateHaskell #-}

module Palantype.Common.RawSteno where

import           Control.Applicative            ( Applicative((<*), pure, (<*>))
                                                )
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
import           Data.Foldable                  ( Foldable(foldl', length, maximum, minimum) )
import           Data.Function                  ( ($) )
import           Data.Functor                   ( ($>)
                                                , (<$>)
                                                , void, Functor (fmap)
                                                )
import           Data.Maybe                     ( Maybe(..) )
import           Data.Monoid                    ( Monoid(mconcat), (<>) )
import           Data.Ord                       ( Ord((<), (>), (>=), (<=)) )
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
import Control.Monad.Fail (MonadFail(fail))
import Data.Bool ((&&), otherwise, (||), Bool (False), bool)
import Palantype.Common.Class (Palantype (toKeys, toFinger, keyCode))
import Palantype.Common.RawSteno.Type (RawSteno (RawSteno, unRawSteno))
import Palantype.Common.Internal (Chord (Chord), PatternPos (..), Finger (..))
import Palantype.Common.KeyIndex (fromIndex)
import qualified Data.List.NonEmpty as NonEmpty
import Palantype.Common.TH (fromJust)
import Data.List ( partition, head )
import Data.List.NonEmpty (nonEmpty)
import Control.Category ((<<<))

fromChord :: forall k . Palantype k => Chord k -> RawSteno
fromChord (Chord []) = fromText ""
fromChord (Chord lsKey) =
    let (left, right) = partition (\k -> toFinger k <= LeftThumb) lsKey
    in  fromText $ case nonEmpty left of

            -- left is empty
            Nothing -> (if ambiguous $ head right then "-" else "")
                <> Text.pack (keyCode <$> right)

            -- left is not empty
            Just leftNE -> case nonEmpty right of
                Nothing -> Text.pack (keyCode <$> left)
                    <> (if ambiguous $ NonEmpty.last leftNE then "-" else "")

                -- right is not empty
                Just rightNE -> addHyphen leftNE rightNE
  where
    ambiguous k = length ($fromJust $ toKeys @k $ keyCode k) > 1

    addHyphen left right =
        let l = NonEmpty.last left
            r = NonEmpty.head right
            bHyphenate =
                case (mAltFinger maximum l, mAltFinger minimum r) of
                    (Nothing, Nothing  ) -> False
                    (Just maxL, Nothing) -> maxL < toFinger r
                    (Nothing, Just minR) -> minR > toFinger l
                    (Just maxL, Just minR) -> maxL < toFinger r || minR > toFinger l
            mHyphen = bool "" "-" bHyphenate
        in  Text.pack $ (keyCode <$> NonEmpty.toList left)
                     <> mHyphen
                     <> (keyCode <$> NonEmpty.toList right)

    mAltFinger f k =
        let as = fmap toFinger $ $fromJust $ toKeys @k $ keyCode k
        in  if length as > 1 then Just $ f as else Nothing

fromText :: Text -> RawSteno
fromText = RawSteno

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
    getFinger c = toFinger $ NonEmpty.head $ $fromJust $ toKeys @key c


-- | parse raw steno code where words are separated by space(s) and
--   chords within one word are separated by '/'
parseSteno :: Palantype key => RawSteno -> Either Text [Chord key]
parseSteno (RawSteno str) =
    case runParser sentence (Nothing, Nothing) "raw steno code" str of
        Left  err -> Left $ Text.pack $ show err
        Right ls  -> Right $ mconcat ls

-- | fails silently in case of parser error and returns
-- | an empty list
parseStenoMaybe :: Palantype key => RawSteno -> Maybe [Chord key]
parseStenoMaybe (RawSteno str) =
    case runParser sentence (Nothing, Nothing) "" str of
        Left  _   -> Nothing
        Right ls  -> Just $ mconcat ls

parseWord :: Palantype key => RawSteno -> Either ParseError [Chord key]
parseWord (RawSteno str) = runParser word (Nothing, Nothing) "" str

-- | parse raw steno code, expects a single chords, i.e. no spaces, no '/'
-- | fails silently and returns and returns an empty chord
parseChordMaybe :: Palantype key => RawSteno -> Maybe (Chord key)
parseChordMaybe (RawSteno str) =
    case runParser chord (Nothing, Nothing) "" str of
        Left  _ -> Nothing
        Right c -> Just c

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

    foldl' (\p k -> p <|> reach k) (fail "key left hand no reach") $
        $fromJust $ toKeys c

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

    ks <- case toKeys c of
        Nothing -> fail $ "Could not parse char as key: " <> pure c
        Just ls -> pure ls

    k <- foldl' (\parser k -> parser <|> reach k) mzero ks
    setState (Just $ toFinger k, Just k)
    pure k
