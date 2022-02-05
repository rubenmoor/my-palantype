{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}

module Palantype.Common.AutoDoc
  ( patternDoc
  , PatternDoc
  ) where

import           Data.Foldable                  ( Foldable
                                                    ( foldl'

                                                    )
                                                )
import           Data.Function                  ( ($), (.)
                                                )
import           Data.Functor                   ( (<$>), Functor (fmap)
                                                )
import           Data.List                      ( (++)
                                                , sort
                                                )
import qualified Data.Map.Strict                      as Map
import           Data.Text                      ( Text)
import qualified Data.Text.Encoding            as Text
import           Palantype.Common.Internal      ( Greediness, PatternPos (..)
                                                )
import Data.Bifunctor (Bifunctor(first))
import Control.Category ((<<<))
import Palantype.Common.Class (patCapitalize, Palantype (PatternGroup, lsPrimitives), RawSteno)
import qualified Palantype.Common.Indices as KI
import Palantype.Common.Dictionary (kiCapNext)

{-|
Pattern documentation, automatically generated from the primitives

Each `PatternGroup` has a list  of patterns, grouped again by greediness,
and then by their position in the word part (onset, nucleus, coda).
Given a pattern group, `Greediness`, and `PatternPos`, there is a list
of natural language literals and their corresponding steno code.

The `PatternGroup` for capitalization, `PatCapitalize`, is added manually, because it does
not feature in the primitives.
-}
type PatternDoc key =
  [(PatternGroup key, [(Greediness, [(PatternPos, [(Text, RawSteno)])])])]

patternDoc
  :: forall key
  .  Palantype key
  => PatternDoc key
patternDoc =
    Map.toList
      $ Map.insert patCapitalize [(0, [(Onset, [("", KI.toRaw @key kiCapNext)])])]
      $ Map.toList . fmap (Map.toList <<< fmap (sort <<< fmap (first Text.decodeUtf8)))
      <$> foldl' accByBs Map.empty lsPrimitives
  where
    accByBs m (bs, entries) =
      foldl' (accByPattern bs) m entries

    accByPattern bs m (g, r, p, bNoDoc, pPos) =
      if bNoDoc
      then m
      else Map.insertWith (Map.unionWith (Map.unionWith (++)))
                          p
                          (Map.singleton g $ Map.singleton pPos [(bs, r)])
                          m
