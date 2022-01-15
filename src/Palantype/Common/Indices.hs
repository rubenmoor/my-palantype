{-|
Description : use language-independent key indices to define commands

Cf. https://github.com/openstenoproject/plover/wiki/Dictionary-Format
-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Palantype.Common.Indices
    ( KIChord
    , toRaw
    , fromChord
    , toKeys
    ) where

import           Control.Category               ( (<<<) )
import           Data.Eq                        ( Eq )
import           Data.Functor                   ( Functor(fmap)
                                                )
import           Data.Hashable                  ( Hashable )
import qualified Data.Text                     as Text
import           Palantype.Common               ( Chord
                                                )
import           TextShow                       ( TextShow(showb, showt)
                                                , fromText
                                                )
import qualified Palantype.DE.Keys as DE
import Data.Function (($))
import Data.Semigroup (Semigroup((<>)))
import Palantype.Common.Class (KeyIndex, Palantype (keyCode, fromIndex), RawSteno (..), toKeyIndices)

{-|
a "key-index chord", an index based steno chord representation
-}
newtype KIChord = KIChord { unKIChord :: [KeyIndex] }
  deriving stock Eq
  deriving newtype Hashable

instance TextShow KIChord where
    showb c = fromText (showt $ toRaw @DE.Key c) <> fromText "(DE)"

toRaw :: forall key . Palantype key => KIChord -> RawSteno
toRaw =
    RawSteno <<< Text.pack <<< fmap (keyCode <<< fromIndex @key) <<< unKIChord

fromChord :: Palantype k => Chord k -> KIChord
fromChord = KIChord <<< toKeyIndices

toKeys :: forall key . Palantype key => KIChord -> [key]
toKeys = fmap fromIndex <<< unKIChord
