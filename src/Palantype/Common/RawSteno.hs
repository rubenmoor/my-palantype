{-# LANGUAGE TupleSections #-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Palantype.Common.RawSteno where

import           Control.Applicative (Alternative (empty))
import           Control.Monad       (MonadPlus (mzero), guard, when)
import           Data.Functor        (void, ($>))
import           Data.Text           (Text)
import qualified Data.Text           as Text
import           Palantype.Common    (Chord (Chord), Finger (RightThumb),
                                      Palantype (toFinger, toKeys))
import           Text.Parsec         (Parsec, anyChar, char, eof, getState,
                                      lookAhead, many1, runParser, sepBy1,
                                      setState, space, spaces, try, (<|>))
import           TextShow            (TextShow (showt, showb), fromText)
import Data.String (IsString (..))
import Data.Aeson.Types (FromJSON)
import Data.Aeson (ToJSON, FromJSONKey, ToJSONKey)
import Text.JSON5 (JSON5)

newtype RawSteno = RawSteno { unRawSteno :: Text }
  deriving (Eq, Ord, JSON5, FromJSON, ToJSON, FromJSONKey, ToJSONKey)

instance TextShow RawSteno where
  showb = fromText . unRawSteno

instance IsString RawSteno where
  fromString = RawSteno . fromString

-- | parse raw steno code where words are separated by space(s) and
-- | chords within one word are separated by '/'
parseSteno
  :: Palantype key
  => RawSteno -> Either Text [Chord key]
parseSteno (RawSteno str) =
  case runParser sentence Nothing "raw steno code" str of
    Left  err -> Left  $ Text.pack $ show err
    Right ls  -> Right $ concat ls

-- | fails silently in case of parser error and returns
-- | an empty list
parseStenoLenient
  :: Palantype key
  => RawSteno
  -> [Chord key]
parseStenoLenient (RawSteno str) =
  case runParser sentence Nothing "raw steno code" str of
    Left  err -> []
    Right ls  -> concat ls

parseWord (RawSteno str) =
  case runParser word Nothing "raw steno code" str of
    Left err -> Left $ Text.pack $ show err
    Right cs -> Right cs

-- | parse raw steno code, expects a single chords, i.e. no spaces, no '/'
-- | fails silently and returns and returns an empty chord
parseChordLenient
  :: Palantype key
  => RawSteno -> Chord key
parseChordLenient (RawSteno str) =
  case runParser chord Nothing "raw steno code" str of
    Left  _ -> Chord []
    Right c -> c

parseStenoKey
  :: Palantype key
  => RawSteno -> Either Text key
parseStenoKey (RawSteno str) =
  case runParser keyWithHyphen Nothing "raw steno code" str of
    Left  err -> Left  $ Text.pack $ show err
    Right k   -> Right k

sentence
  :: Palantype key
  => Parsec Text (Maybe Finger) [[Chord key]]
sentence = spaces >> sepBy1 word (many1 space) <* eof

word
  :: Palantype key
  => Parsec Text (Maybe Finger) [Chord key]
word = sepBy1 chord (char '/')

chord
  :: Palantype key
  => Parsec Text (Maybe Finger) (Chord key)
chord = do
  setState Nothing
  ks <- keys
  eof
  pure $ Chord ks

keys
  :: Palantype key
  => Parsec Text (Maybe Finger) [key]
keys = many1 keyWithHyphen

keyWithHyphen
  :: Palantype key
  => Parsec Text (Maybe Finger) key
keyWithHyphen = try (keyLeftHand <* eof) <|> keyOrHyphenKey

keyLeftHand
  :: Palantype key
  => Parsec Text (Maybe Finger) key
keyLeftHand = do
  mFinger <- getState
  c <- anyChar
  h <- char '-'

  let reach
        :: Palantype key
        => key -> Parsec Text (Maybe Finger) key
      reach k = do
        let f = toFinger k
        guard $ Just f < mFinger
        guard $ f < RightThumb
        pure k

  foldl (\p k -> p <|> reach k) mzero (toKeys c)

keyOrHyphenKey
  :: Palantype key
  => Parsec Text (Maybe Finger) key
keyOrHyphenKey = do
  finger <- getState
  c <- lookAhead anyChar
  when (c == '-') $ do
    when (finger < Just RightThumb) $ setState $ Just RightThumb
    void anyChar
  key

key
  :: Palantype key
  => Parsec Text (Maybe Finger) key
key = do
  mFinger <- getState
  c <- lookAhead anyChar

  let reach
        :: Palantype key
        => key -> Parsec Text (Maybe Finger) key
      reach k = do
        guard $ Just (toFinger k) > mFinger
        anyChar $> k

  k <- foldl (\parser k -> parser <|> reach k) mzero (toKeys c)
  setState $ Just $ toFinger k
  pure k
