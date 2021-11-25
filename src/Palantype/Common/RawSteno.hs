{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Palantype.Common.RawSteno where

import           Control.Applicative            ( Alternative(empty) )
import           Control.Monad                  ( MonadPlus(mzero)
                                                , guard
                                                , unless
                                                , when
                                                )
import           Data.Aeson                     ( FromJSONKey
                                                , ToJSON
                                                , ToJSONKey
                                                )
import           Data.Aeson.Types               ( FromJSON )
import           Data.Functor                   ( ($>)
                                                , void
                                                )
import           Data.Hashable                  ( Hashable )
import           Data.String                    ( IsString(..) )
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           Palantype.Common               ( Chord(Chord)
                                                , Finger(RightThumb)
                                                , Palantype(toFinger, toKeys)
                                                )
import           Text.JSON5                     ( JSON5 )
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
                                                , parserTrace
                                                , runParser
                                                , sepBy1
                                                , setState
                                                , space
                                                , spaces
                                                , try
                                                )
import           TextShow                       ( TextShow(showb, showt)
                                                , fromText
                                                )

newtype RawSteno = RawSteno { unRawSteno :: Text }
  deriving (Eq, Ord, JSON5, FromJSON, ToJSON, FromJSONKey, ToJSONKey, Hashable)

instance TextShow RawSteno where
    showb = fromText . unRawSteno

instance IsString RawSteno where
    fromString = RawSteno . fromString

instance Show RawSteno where
    show = Text.unpack . showt

-- | parse raw steno code where words are separated by space(s) and
-- | chords within one word are separated by '/'
parseSteno :: Palantype key => RawSteno -> Either Text [Chord key]
parseSteno (RawSteno str) =
    case runParser sentence Nothing "raw steno code" str of
        Left  err -> Left $ Text.pack $ show err
        Right ls  -> Right $ concat ls

-- | fails silently in case of parser error and returns
-- | an empty list
parseStenoLenient :: Palantype key => RawSteno -> [Chord key]
parseStenoLenient (RawSteno str) = case runParser sentence Nothing "" str of
    Left  err -> []
    Right ls  -> concat ls

parseWord :: Palantype key => RawSteno -> Either ParseError [Chord key]
parseWord (RawSteno str) = runParser word Nothing "" str

-- | parse raw steno code, expects a single chords, i.e. no spaces, no '/'
-- | fails silently and returns and returns an empty chord
parseChordLenient :: Palantype key => RawSteno -> Chord key
parseChordLenient (RawSteno str) = case runParser chord Nothing "" str of
    Left  _ -> Chord []
    Right c -> c

parseStenoKey :: Palantype key => RawSteno -> Either Text key
parseStenoKey (RawSteno str) = case runParser keyWithHyphen Nothing "" str of
    Left  err -> Left $ Text.pack $ show err
    Right k   -> Right k

sentence :: Palantype key => Parsec Text (Maybe Finger) [[Chord key]]
sentence = spaces >> sepBy1 word (many1 space) <* eof

word :: Palantype key => Parsec Text (Maybe Finger) [Chord key]
word = sepBy1 chord (char '/')

chord :: Palantype key => Parsec Text (Maybe Finger) (Chord key)
chord = do
    setState Nothing
    ks <- keys
    eof <|> void (lookAhead $ char '/')
    pure $ Chord ks

keys :: Palantype key => Parsec Text (Maybe Finger) [key]
keys = do
    many1 keyWithHyphen

keyWithHyphen :: Palantype key => Parsec Text (Maybe Finger) key
keyWithHyphen = do
    try keyLeftHand <|> keyOrHyphenKey <?> "left hand or key"

keyLeftHand :: Palantype key => Parsec Text (Maybe Finger) key
keyLeftHand = do
    mFinger <- getState
    c       <- noneOf "/"
    h       <- char '-'
    eof <|> void (lookAhead $ char '/')

    let reach :: Palantype key => key -> Parsec Text (Maybe Finger) key
        reach k = do
            let f = toFinger k
            setState $ Just f
            guard $ mFinger < Just f
            guard $ f < RightThumb
            pure k

    foldl (\p k -> p <|> reach k) (fail "key left hand no reach") (toKeys c)

keyOrHyphenKey :: Palantype key => Parsec Text (Maybe Finger) key
keyOrHyphenKey = do
    finger <- getState
    c      <- lookAhead $ noneOf "/"
    when (c == '-') $ do
        when (finger < Just RightThumb) $ setState $ Just RightThumb
        void anyChar
    key

key :: Palantype key => Parsec Text (Maybe Finger) key
key = do
    mFinger <- getState
    c       <- lookAhead $ noneOf "/"

    let reach :: Palantype key => key -> Parsec Text (Maybe Finger) key
        reach k = do
            unless (Just (toFinger k) > mFinger)
                $  fail
                $  "key "
                <> Text.unpack (showt k)
                <> " no reach"
            anyChar $> k

    k <- foldl (\parser k -> parser <|> reach k)
               (fail "keys no reach")
               (toKeys c)
    setState $ Just $ toFinger k
    pure k
