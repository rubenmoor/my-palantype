{-# LANGUAGE NoImplicitPrelude #-}

module Palantype.Common where

import           Data.Char     (Char)
import           Data.Eq       (Eq)
import           Data.Foldable (Foldable (foldr))
import           Data.Function (($))
import           Data.List     ((++), intersperse)
import           Data.Map      (Map)
import qualified Data.Map      as Map
import           Data.Ord      (Ord)
import TextShow (TextShow (showb), singleton)
import Data.Monoid (Monoid(mconcat))
import Control.Category ((.))
import Data.Functor (Functor(fmap))

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
  deriving (Eq, Ord)

class Palantype key where
  toFinger :: key -> Finger

  -- map every key to a character representing raw steno code
  keyCode :: key -> Char
  toKeys  :: Char -> [key]

-- a series of chords, to type a word of arbitrary length
newtype Series k = Series { unSeries :: [Chord k] }
  deriving (Eq, Ord)

instance TextShow k => TextShow (Series k) where
  showb = mconcat . intersperse (singleton '/') . fmap showb . unSeries

newtype Chord k = Chord { unChord :: [k] }
  deriving (Eq, Ord)

instance TextShow k => TextShow (Chord k) where
  showb = mconcat . fmap showb . unChord
