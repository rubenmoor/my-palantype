{-|
Description : use language-independent key indices to define commands

For simplicity, the commands are defined using the original Palantype.EN.
But the generic key codes are exported.

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
    ) where

import           Control.Category               ( (<<<) )
import           Data.Eq                        ( Eq )
import           Data.Foldable                  ( Foldable(foldl') )
import           Data.Function                  ( ($) )
import           Data.Functor                   ( (<$>)
                                                , Functor(fmap)
                                                )
import           Data.HashMap.Strict            ( HashMap )
import qualified Data.HashMap.Strict           as HashMap
import           Data.Hashable                  ( Hashable )
import           Data.List                      ( (++) )
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           Palantype.Common               ( Chord
                                                , KeyIndex
                                                , Palantype(fromIndex, keyCode)
                                                , toKeyIndices
                                                )
import           Palantype.Common.RawSteno      ( RawSteno(RawSteno)
                                                , parseChordLenient
                                                )
import qualified Palantype.DE.Keys             as DE
import qualified Palantype.EN.Keys             as EN
import           TextShow                       ( TextShow(showb, showt)
                                                , fromText
                                                )

{-|
a "key-index chord", an index based steno chord representation
-}
newtype KIChord = KIChord { unKIChord :: [KeyIndex] }
  deriving (Eq, Hashable)

instance TextShow KIChord where
    showb = fromText <<< showt <<< toRaw @EN.Key

toRaw :: forall key . Palantype key => KIChord -> RawSteno
toRaw =
    RawSteno <<< Text.pack <<< fmap (keyCode <<< fromIndex @key) <<< unKIChord

fromChord :: Palantype k => Chord k -> KIChord
fromChord = KIChord <<< toKeyIndices
