{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
import           Data.Data                      ( Proxy(Proxy)
                                                , Typeable
                                                , typeRep
                                                )
import           Data.Functor                   ( ($>)
                                                , void
                                                )
import           Data.Hashable                  ( Hashable )
import           Data.Proxied                   ( dataTypeOfProxied )
import           Data.String                    ( IsString(..) )
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           Palantype.Common               ( Chord(Chord)
                                                , Finger(RightThumb)
                                                , Palantype
                                                    ( fromIndex
                                                    , toFinger
                                                    , toKeys
                                                    )
                                                )
import           Palantype.EN                   ( pEN )
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
                                                , oneOf
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

fromChord :: forall k. Palantype k => Chord k -> RawSteno
fromChord = RawSteno . showt

unparts :: [RawSteno] -> RawSteno
unparts rs = RawSteno $ Text.intercalate "/" $ unRawSteno <$> rs

instance IsString RawSteno where
    fromString = RawSteno . fromString

instance Show RawSteno where
    show = Text.unpack . showt

-- | parse raw steno code where words are separated by space(s) and
--   chords within one word are separated by '/'
parseSteno :: Palantype key => RawSteno -> Either Text [Chord key]
parseSteno (RawSteno str) =
    case runParser sentence (Nothing, Nothing) "raw steno code" str of
        Left  err -> Left $ Text.pack $ show err
        Right ls  -> Right $ concat ls

-- | fails silently in case of parser error and returns
-- | an empty list
parseStenoLenient :: Palantype key => RawSteno -> [Chord key]
parseStenoLenient (RawSteno str) = case runParser sentence (Nothing, Nothing) "" str of
    Left  err -> []
    Right ls  -> concat ls

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
    h             <- char '-'
    eof <|> void (lookAhead $ char '/')

    let reach
            :: Palantype key => key -> Parsec Text (Maybe Finger, Maybe key) key
        reach k = do
            let f = toFinger k
            setState (Just f, Just k)
            guard $ if typeRep (Proxy :: Proxy key) == typeRep pEN
                then lk < Just k
                else mFinger < Just f
            guard $ f < RightThumb
            pure k

    foldl (\p k -> p <|> reach k) (fail "key left hand no reach") (toKeys c)

keyOrHyphenKey :: Palantype key => Parsec Text (Maybe Finger, Maybe key) key
keyOrHyphenKey = do
    (finger, _) <- getState
    c           <- lookAhead $ noneOf " /"
    when (c == '-') $ do
        when (finger < Just RightThumb)
            $ setState (Just RightThumb, Just $ fromIndex 16)
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
            :: Palantype key => key -> Parsec Text (Maybe Finger, Maybe key) key
        reach k = do
-- a hacky way to allow multiple keys pressed by one finger
-- for the original palantype system
            if typeRep (Proxy :: Proxy key) == typeRep pEN
                then unless (Just k > mLastK) mzero
                else unless (Just (toFinger k) > mFinger) mzero
            anyChar $> k

    k <- foldl (\parser k -> parser <|> reach k) mzero $ toKeys c
    setState (Just $ toFinger k, Just k)
    pure k
