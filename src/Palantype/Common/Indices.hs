{-|
Description : use language-independent key indices to define commands

Cf. https://github.com/openstenoproject/plover/wiki/Dictionary-Format
-}

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Palantype.Common.Indices
    ( KIChord
    , toRaw
    , fromChord
    , parseChordDE
    , toKeys
    , allKeys
    ) where

import Language.Haskell.TH (Exp, Q)
import           Data.Eq                        ( Eq )
import           Data.Functor                   ( Functor(fmap)
                                                , (<$>)
                                                )
import           Data.Hashable                  ( Hashable )
import           Palantype.Common.Internal      ( Chord (..)
                                                )
import Control.Category ((.), (<<<))
import Palantype.Common.KeyIndex (KeyIndex (..), fromIndex, toKeyIndices)
import Palantype.Common.RawSteno (parseChordMaybe)
import TextShow (TextShow (showb, showt), fromText)
import qualified Palantype.DE.Keys as DE
import Data.Function (($))
import Data.Semigroup ((<>))
import Data.Data (Data)
import Data.Maybe (Maybe(..))
import Palantype.Common.TH (failure)
import Text.Show (Show(show))
import Palantype.Common.RawSteno.Type (RawSteno)
import qualified Palantype.Common.RawSteno as Raw
import Palantype.Common.Class (Palantype, allKeyIndices)

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
    Raw.fromChord <<< Chord <<< fmap (fromIndex @key) <<< unKIChord

fromChord :: Palantype k => Chord k -> KIChord
fromChord = KIChord <<< toKeyIndices

toKeys :: forall key . Data key => KIChord -> [key]
toKeys = fmap fromIndex <<< unKIChord

parseChordDE :: Q Exp
parseChordDE =
  [|
    \raw -> case parseChordMaybe @DE.Key raw of
        Just chord -> fromChord chord
        Nothing    -> $failure $ "Parse error: " <> show raw
  |]

allKeys :: forall key . Palantype key => [key]
allKeys = fromIndex . KeyIndex <$> allKeyIndices @key
