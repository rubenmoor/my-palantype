{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFoldable #-}

{-# LANGUAGE DerivingStrategies #-}
module Palantype.Common where

import           Control.Category               ( (.)
                                                , (<<<)
                                                )
import           Control.Exception              ( assert )
import           Data.Aeson                     ( FromJSON
                                                , FromJSONKey
                                                , ToJSON
                                                , ToJSONKey
                                                )
import           Data.Char                      ( Char )
import           Data.Data                      ( Data(toConstr)
                                                , Proxy(Proxy)
                                                , constrIndex
                                                , dataTypeOf
                                                , fromConstr
                                                , indexConstr
                                                , maxConstrIndex
                                                )
import           Data.Eq                        ( Eq((==)) )
import           Data.Foldable                  ( Foldable(foldl) )
import           Data.Function                  ( ($)
                                                , flip
                                                )
import           Data.Functor                   ( (<$>)
                                                , Functor(fmap)
                                                )
import           Data.Hashable                  ( Hashable )
import           Data.Int                       ( Int )
import           Data.List                      ( (++)
                                                , intersperse
                                                , sort
                                                )
import qualified Data.Map                      as Map
import           Data.Maybe                     ( fromMaybe )
import           Data.Monoid                    ( Monoid(mconcat) )
import           Data.Ord                       ( Ord((<=)) )
import           Data.Proxied                   ( dataTypeOfProxied )
import           Data.Semigroup                 ( Semigroup((<>)) )
import qualified Data.Text                     as Text
import           Data.Text                      ( Text )
import           GHC.Err                        ( error )
import           GHC.Generics                   ( Generic )
import           GHC.Num                        ( Num )
import           Text.Read                      ( Read )
import           Text.Show                      ( Show(show) )
import           TextShow                       ( TextShow
                                                    ( showb
                                                    , showbPrec
                                                    , showt
                                                    )
                                                , fromText
                                                , singleton
                                                )
import           TextShow.Generic               ( genericShowbPrec )

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

-- | defines a steno key layout
-- |
-- | The default implementation of `toFinger` assigns every finger three keys:
-- | a top row key, a home row key, and a bottom row key; in that order.
-- | The thumbs are an exception, because as it stands now I added a forth key
-- | for each.
-- |
-- | There should be exactly 32 keys (32 constructors), otherwise the default
-- | implementation for `toFinger` will fail and the layout will look messy
class (Data key, Eq key, Ord key, TextShow key) => Palantype key where
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
        _  -> error "toFinger: impossible"

  -- | override for efficiency
  toKeys :: Char -> [key]
  toKeys c =
    let t = dataTypeOfProxied (Proxy :: Proxy key)
        ks = fromConstr . indexConstr t <$> [1..(maxConstrIndex t)]
        m = foldl (\m' k -> Map.insertWith (flip (++)) (keyCode k) [k] m') Map.empty ks
    in  fromMaybe [] $ Map.lookup c m

  keyIndex :: key -> KeyIndex
  keyIndex = KeyIndex . constrIndex . toConstr

  fromIndex :: KeyIndex -> key
  fromIndex i =
    let t = dataTypeOfProxied (Proxy :: Proxy key)
    in  fromConstr $ indexConstr t $ unKeyIndex i

newtype KeyIndex = KeyIndex { unKeyIndex :: Int }
  deriving stock (Eq, Ord)
  deriving newtype (Hashable, FromJSON, ToJSON, FromJSONKey, ToJSONKey, Num)

-- a series of chords, to type a word of arbitrary length
newtype Series k = Series { unSeries :: [Chord k] }
  deriving stock (Eq, Ord, Foldable)

instance TextShow k => TextShow (Series k) where
    showb = mconcat . intersperse (singleton '/') . fmap showb . unSeries

newtype Chord k = Chord { unChord :: [k] }
  deriving stock (Eq, Ord, Foldable)

mkChord :: forall k . (Palantype k) => [k] -> Chord k
mkChord keys = Chord $ sort keys

instance TextShow k => TextShow (Chord k) where
    showb = mconcat . fmap showb . unChord

instance (TextShow k) => Show (Chord k) where
    show = Text.unpack . showt

toKeyIndices :: Palantype k => Chord k -> [KeyIndex]
toKeyIndices = fmap keyIndex <<< unChord

-- | show the key, with a hyphen attached
--   x- for the keys of the left hand,
--   -x for the keys of the right hand
showH :: Palantype key => key -> Text
showH k =
    let f = if constrIndex (toConstr k) <= 16 then (<> "-") else ("-" <>)
    in  f $ Text.singleton $ keyCode k

class (Data a, FromJSON a, Ord a) => PatternGroup a where
  toDescription :: a -> Text
