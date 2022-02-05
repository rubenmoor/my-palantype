{-|
Description : use language-independent key indices to define commands

Cf. https://github.com/openstenoproject/plover/wiki/Dictionary-Format
-}

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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
import           Palantype.Common.Internal      ( Chord (..)
                                                )
import Palantype.Common.Class (Palantype (keyCode), RawSteno (..))
import Palantype.Common.KeyIndex (KeyIndex, fromIndex, toKeyIndices)
import TextShow (TextShow (showb, showt), fromText)
import qualified Palantype.DE.Keys as DE
import Data.Function (($))
import Data.Semigroup ((<>))
import Data.Int (Int)
import Data.Ord (Ord)
import Data.Aeson (FromJSON, ToJSON, FromJSONKey, ToJSONKey)
import GHC.Num (Num)
import Data.Data (Proxy (Proxy), Data (toConstr), constrIndex, fromConstr, indexConstr)
import Data.Proxied (dataTypeOfProxied)

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

toKeys :: forall key . Data key => KIChord -> [key]
toKeys = fmap fromIndex <<< unKIChord
