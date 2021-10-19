{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# LANGUAGE KindSignatures #-}
module Palantype.Common where

import           Data.Char     (Char, GeneralCategory (OtherLetter))
import           Data.Eq       (Eq ((==)))
import           Data.Foldable (Foldable (foldr))
import           Data.Function (($))
import           Data.List     ((++), intersperse)
import           Data.Map      (Map)
import qualified Data.Map      as Map
import           Data.Ord      (Ord)
import TextShow (TextShow (showb), singleton)
import Data.Monoid (Monoid(mconcat))
import Control.Category ((.))
import Data.Functor (Functor(fmap), (<$>))
import Data.Int (Int)
import Data.Data (Data (toConstr), indexConstr, maxConstrIndex, dataTypeOf, fromConstr, constrIndex)
import Data.Foldable (Foldable(foldl))
import Data.Maybe (fromMaybe)
import GHC.Base (undefined)
import Control.Exception (assert)
import Data.Proxied (dataTypeOfProxied)
import Data.Data (Proxy(Proxy))

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

-- | defines a steno key layout
-- |
-- | The default implementation of `toFinger` assigns every finger three keys:
-- | a top row key, a home row key, and a bottom row key; in that order.
-- | The thumbs are an exception, because as it stands now I added a forth key
-- | for each.
-- |
-- | There should be exactly 32 keys (32 constructors), otherwise the default
-- | implementation for `toFinger` will fail and the layout will look messy
class Data key => Palantype key where
  -- | map every key to a character representing raw steno code
  -- | this is the only required function
  keyCode :: key -> Char

  toFinger :: key -> Finger
  toFinger k =
    assert (maxConstrIndex (dataTypeOf k) == 32) $
      case keyIndex k of
        1  -> LeftPinky
        2  -> LeftPinky
        3  -> LeftPinky
        4  -> LeftRing
        5  -> LeftRing
        6  -> LeftRing
        7  -> LeftMiddle
        8  -> LeftMiddle
        9  -> LeftMiddle
        10 -> LeftIndex
        11 -> LeftIndex
        12 -> LeftIndex
        13 -> LeftThumb -- extra key
        14 -> LeftThumb
        15 -> LeftThumb
        16 -> LeftThumb
        17 -> RightThumb -- extra key
        18 -> RightThumb
        19 -> RightThumb
        20 -> RightThumb
        21 -> RightIndex
        22 -> RightIndex
        23 -> RightIndex
        24 -> RightMiddle
        25 -> RightMiddle
        26 -> RightMiddle
        27 -> RightRing
        28 -> RightRing
        29 -> RightRing
        30 -> RightPinky
        31 -> RightPinky
        32 -> RightPinky

  -- | override for efficiency
  toKeys :: Char -> [key]
  toKeys c =
    let t = dataTypeOfProxied (Proxy :: Proxy key)
        ks = fromConstr . indexConstr t <$> [1..(maxConstrIndex t)]
        m = foldl (\m k -> Map.insertWith (++) (keyCode k) [k] m) Map.empty ks
    in  fromMaybe [] $ Map.lookup c m

  keyIndex :: key -> Int
  keyIndex = constrIndex . toConstr

  fromIndex :: Int -> key
  fromIndex i =
    let t = dataTypeOfProxied (Proxy :: Proxy key)
    in  fromConstr $ indexConstr t i

-- a series of chords, to type a word of arbitrary length
newtype Series k = Series { unSeries :: [Chord k] }
  deriving (Eq, Ord)

instance TextShow k => TextShow (Series k) where
  showb = mconcat . intersperse (singleton '/') . fmap showb . unSeries

newtype Chord k = Chord { unChord :: [k] }
  deriving (Eq, Ord)

instance TextShow k => TextShow (Chord k) where
  showb = mconcat . fmap showb . unChord
