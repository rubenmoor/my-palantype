{-# LANGUAGE TupleSections #-}

module Palantype.Common.RawSteno where

import           Control.Applicative (Alternative (empty))
import           Control.Monad       (MonadPlus (mzero), guard, when)
import           Data.Functor        (void, ($>))
import           Data.Text           (Text)
import qualified Data.Text           as Text
import           Palantype.Common    (Chord (Chord), Finger (RightThumb),
                                      Palantype (toFinger, toKeys))
import           Palantype.DE.Keys   (Key)
import           Text.Parsec         (Parsec, anyChar, char, eof, getState,
                                      lookAhead, many1, runParser, sepBy1,
                                      setState, space, spaces, try, (<|>))
import           TextShow            (TextShow (showt))

parseRawSteno :: Text -> Either Text [Chord Key]
parseRawSteno str =
  case runParser sentence Nothing "frontend steno code" str of
    Left  err -> Left  $ Text.pack $ show err
    Right ls  -> Right $ concat ls

sentence :: Parsec Text (Maybe Finger) [[Chord Key]]
sentence = spaces >> sepBy1 word (many1 space) <* eof

word :: Parsec Text (Maybe Finger) [Chord Key]
word = sepBy1 chord (char '/')

chord :: Parsec Text (Maybe Finger) (Chord Key)
chord = do
  setState Nothing
  ks <- keys
  eof
  pure $ Chord ks

keys :: Parsec Text (Maybe Finger) [Key]
keys = many1 $ try (keyLeftHand <* eof) <|> keyOrHyphen

keyLeftHand :: Parsec Text (Maybe Finger) Key
keyLeftHand = do
  mFinger <- getState
  c <- anyChar
  h <- char '-'

  let reach :: Key -> Parsec Text (Maybe Finger) Key
      reach k = do
        let f = toFinger k
        guard $ Just f < mFinger
        guard $ f < RightThumb
        pure k

  foldl (\p k -> p <|> reach k) mzero (toKeys c)

keyOrHyphen :: Parsec Text (Maybe Finger) Key
keyOrHyphen = do
  finger <- getState
  c <- lookAhead anyChar
  when (c == '-') $ do
    when (finger < Just RightThumb) $ setState $ Just RightThumb
    void anyChar
  key

key :: Parsec Text (Maybe Finger) Key
key = do
  mFinger <- getState
  c <- lookAhead anyChar

  let reach :: Key -> Parsec Text (Maybe Finger) Key
      reach k = do
        guard $ Just (toFinger k) > mFinger
        anyChar $> k

  foldl (\parser k -> parser <|> reach k) mzero (toKeys c)
