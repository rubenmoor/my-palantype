{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Palantype.Common.Class
  ( Palantype (..)
  , mkChord
  ) where

import           Control.Category               ( (.)

                                                )
import           Control.Exception              ( assert )
import           Data.Aeson                     ( FromJSON

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

                                                )
import           Data.List                      ( (++)

                                                , sort
                                                )
import qualified Data.Map                      as Map
import           Data.Ord                       ( Ord((<=)) )
import           Data.Proxied                   ( dataTypeOfProxied )
import           Data.Semigroup                 ( Semigroup((<>)) )
import qualified Data.Text                     as Text
import           Data.Text                      ( Text )
import           GHC.Err                        ( error )
import           TextShow                       (TextShow)
import Data.ByteString (ByteString)
import Control.DeepSeq (NFData)
import Palantype.Common.Internal (Finger (..), PatternPos, Greediness, Chord (Chord))
import Palantype.Common.KeyIndex ( keyIndex )
import Data.Bool (Bool)
import Data.Map (Map)
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import Data.Maybe (Maybe)
import Palantype.Common.RawSteno.Type (RawSteno)

-- | defines a steno key layout
-- |
-- | The default implementation of `toFinger` assigns every finger three keys:
-- | a top row key, a home row key, and a bottom row key; in that order.
-- | The thumbs are an exception, because as it stands now I added a forth key
-- | for each.
-- |
-- | There should be exactly 32 keys (32 constructors), otherwise the default
-- | implementation for `toFinger` will fail and the layout will look messy
class ( Data key
      , Eq key
      , Ord key
      , TextShow key
      , Data (PatternGroup key)
      , FromJSON (PatternGroup key)
      , NFData (PatternGroup key)
      , Ord (PatternGroup key)
      , TextShow (PatternGroup key)
      , ToJSON (PatternGroup key)
      , ToJSONKey (PatternGroup key)
      ) => Palantype key where

  type PatternGroup key = pg | pg -> key

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

  -- | get all the Palantype Keys that are represented by the given char
  --   valid chars result in lists of one or two keys
  --   invalid chars (that do not represent any key) result in an empty list
  toKeys :: Char -> Maybe (NonEmpty key)
  toKeys c =
    let t = dataTypeOfProxied (Proxy :: Proxy key)
        ks = fromConstr . indexConstr t <$> [1 .. maxConstrIndex t]
        m = foldl (\m' k -> Map.insertWith (flip (++)) (keyCode k) [k] m') Map.empty ks
    in  nonEmpty $ Map.findWithDefault [] c m

  toDescription :: PatternGroup key -> Text
  patSimpleMulti :: PatternGroup key
  patCapitalize :: PatternGroup key
  patAcronym :: PatternGroup key

  -- | the primitives as defined in "primitives.json"
  lsPrimitives
    :: [(ByteString, [(Greediness, RawSteno, PatternGroup key, Bool, PatternPos)])]

  -- | full word exceptions
  -- exceptions that span several chords go here
  mapExceptions
    :: Map Text [(RawSteno, PatternGroup key)]

mkChord :: forall k. (Palantype k) => [k] -> Chord k
mkChord keys = Chord $ sort keys
