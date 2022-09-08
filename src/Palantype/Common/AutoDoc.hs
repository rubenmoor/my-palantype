{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Palantype.Common.AutoDoc
    ( patternDoc
    , PatternDoc
    ) where

import           Data.Bifunctor                 ( Bifunctor(first, second) )
import           Data.Bool                      ( Bool(False) )
import           Data.Foldable                  ( Foldable(foldl') )
import           Data.Function                  ( ($)
                                                )
import           Data.Functor                   ( (<$>)
                                                , Functor(fmap)
                                                )
import           Data.List                      ( sort
                                                )
import qualified Data.Map.Strict               as Map
import           Data.Map.Strict                ( Map )
import           Data.Semigroup                 ( Semigroup((<>)) )
import           Data.Text                      ( Text )
import qualified Data.Text.Encoding            as Text
import           Palantype.Common.Class         ( Palantype
                                                    ( PatternGroup
                                                    , lsPrimitives
                                                    , mapExceptions
                                                    )
                                                , patCapitalize
                                                )
import           Palantype.Common.Dictionary.Plover
                                                ( kiCapNext )
import qualified Palantype.Common.Indices      as KI
import           Palantype.Common.Internal      ( Greediness
                                                , PatternPos(..)
                                                )
import           Palantype.Common.RawSteno.Type ( RawSteno )

{-|
Pattern documentation, automatically generated from the primitives

Each `PatternGroup` has a list  of patterns, grouped again by greediness,
and then by their position in the word part (onset, nucleus, coda).
Given a pattern group, `Greediness`, and `PatternPos`, there is a list
of natural language literals and their corresponding steno code.

The `PatternGroup` for capitalization, `PatCapitalize`, is added manually, because it does
not feature in the primitives.
-}
type PatternDoc key
    = Map (PatternGroup key) (Map Greediness (Map PatternPos [(Text, RawSteno)]))

patternDoc :: forall key . Palantype key => PatternDoc key
patternDoc =
    fmap (fmap sort) <$> Map.insert patCapitalize pgCapitalize mapPatternDoc
  where
    pgCapitalize = Map.singleton 0 $
      Map.singleton Onset [("", KI.toRaw @key kiCapNext)]

    mapPatternDoc
        :: Map (PatternGroup key) (Map Greediness (Map PatternPos [(Text, RawSteno)]))
    mapPatternDoc =
        foldl' accByBs Map.empty
            $  (first Text.decodeUtf8 <$> lsPrimitives)
            <> lsExceptions

    accByBs
        :: Map (PatternGroup key) (Map Greediness (Map PatternPos [(Text, RawSteno)]))
        -> (Text, [(Greediness, RawSteno, PatternGroup key, Bool, PatternPos)])
        -> Map (PatternGroup key) (Map Greediness (Map PatternPos [(Text, RawSteno)]))
    accByBs m (str, entries) = foldl' (accByPattern str) m entries

    accByPattern
        :: Text
        -> Map (PatternGroup key) (Map Greediness (Map PatternPos [(Text, RawSteno)]))
        -> (Greediness, RawSteno, PatternGroup key, Bool, PatternPos)
        -> Map (PatternGroup key) (Map Greediness (Map PatternPos [(Text, RawSteno)]))
    accByPattern str m (g, r, p, bNoDoc, pPos) = if bNoDoc
        then m
        else Map.insertWith
            (Map.unionWith (Map.unionWith (<>)))
            p
            (Map.singleton g $ Map.singleton pPos [(str, r)])
            m

    lsExceptions = second (fmap $ \(g, r, p, bNoDoc) -> (g, r, p, bNoDoc, PPException))
        <$> Map.toList mapExceptions
