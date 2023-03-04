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

import           Control.Category               ( (<<<) )
import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                )
import           Data.Foldable                  ( Foldable )
import           Data.Int                       ( Int )
import           Data.Ord                       ( Ord )
import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )
import           Text.Show                      ( Show(show) )
import           TextShow                       ( TextShow
                                                    ( showb
                                                    , showbPrec
                                                    )
                                                , fromString
                                                )
import           TextShow.Generic               ( genericShowbPrec )
import           Data.Eq                        ( Eq )

data ExceptionInterpretation
  = ExcRuleAddition
  | ExcSubstitution
  deriving stock (Show)

instance TextShow ExceptionInterpretation where
  showb = fromString <<< show

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
    | -- | a whole world exception from `exceptions.json`
      PPException
    deriving stock (Eq, Generic, Ord, Show)

instance FromJSON PatternPos
instance ToJSON PatternPos

instance TextShow PatternPos where
    showb = fromString <<< show

showPretty :: PatternPos -> Text
showPretty = \case
    Onset        -> "Onset"
    Nucleus      -> "Nucleus"
    Coda         -> "Coda"
    Multiple     -> "Multiple"
    OnsetAndCoda -> "Onset/Coda"
    PPException  -> "Exceptions"

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
    deriving stock (Foldable)

newtype Chord k = Chord {unChord :: [k]}
    deriving stock (Foldable, Generic)
