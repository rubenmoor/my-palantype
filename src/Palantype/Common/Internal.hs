{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Palantype.Common.Internal where

import Control.Category (
    (<<<),
 )
import Data.Aeson (
    FromJSON,
    FromJSONKey,
    ToJSON,
    ToJSONKey,
 )
import Data.Eq (Eq)
import Data.Foldable (Foldable)
import Data.Int (Int)
import Data.Ord (Ord)
import qualified Data.Text as Text
import GHC.Generics (Generic)
import Data.Text (Text)
import Text.Read (Read)
import Text.Show (Show (show))
import TextShow (
    TextShow (
        showb,
        showbPrec,
        showt
    ),
    fromString,
    fromText,
 )
import TextShow.Generic (genericShowbPrec)

data Lang = EN | DE
    deriving stock (Eq, Generic, Ord, Read)

instance FromJSON Lang
instance ToJSON Lang
instance FromJSONKey Lang
instance ToJSONKey Lang

instance TextShow Lang where
    showb = \case
        EN -> fromText "Palantype"
        DE -> fromText "Palantype DE"

instance Show Lang where
    show = Text.unpack <<< showt

type Greediness = Int

{- |
pattern position
-}
data PatternPos
    = -- | start in the onset and end in the nucleus
      Onset
    | -- | start and end in the nucleus
      Nucleus
    | -- | start in the nucleus or later
      Coda
    | -- | contain onset and coda, may contain several chords
      Multiple
    | -- | single letter that can be onset or coda
      OnsetAndCoda
    deriving stock (Eq, Generic, Ord, Show)

instance FromJSON PatternPos
instance ToJSON PatternPos

instance TextShow PatternPos where
    showb = fromString <<< show

showPretty :: PatternPos -> Text
showPretty = \case
    Onset -> "Onset"
    Nucleus -> "Nucleus"
    Coda -> "Coda"
    Multiple -> "Multiple"
    OnsetAndCoda -> "Onset/Coda"

data Finger
    = LeftPinky
    | LeftRing
    | LeftMiddle
    | LeftIndex
    | LeftThumb
    | RightThumb
    | RightIndex
    | RightMiddle
    | RightRing
    | RightPinky
    deriving stock (Generic, Eq, Ord, Show)

instance TextShow Finger where
    showbPrec = genericShowbPrec

-- a series of chords, to type a word of arbitrary length
newtype Series k = Series {unSeries :: [Chord k]}
    deriving stock (Eq, Ord, Foldable)

newtype Chord k = Chord {unChord :: [k]}
    deriving stock (Eq, Ord, Foldable)
