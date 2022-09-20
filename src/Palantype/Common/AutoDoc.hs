{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Palantype.Common.AutoDoc
    ( patternDoc
    , PatternDoc
    ) where

import           Data.Bifunctor                 ( Bifunctor(first) )
import           Data.Bool                      ( Bool(False) )
import           Data.Foldable                  ( Foldable(foldl') )
import           Data.Function                  ( ($)
                                                )
import           Data.Functor                   ( (<$>)
                                                , Functor(fmap), (<&>)
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
import           Palantype.Common.RawSteno.Type ( RawSteno (RawSteno) )
import Control.Category ((<<<))

{-|
Pattern documentation, automatically generated from the primitives

Each `PatternGroup` has a list  of patterns, grouped again by greediness,
and then by their position in the word part (onset, nucleus, coda).
Given a pattern group, `Greediness`, and `PatternPos`, there is a list
of natural language literals and their corresponding steno code.
-}
type PatternDoc key
    = Map (PatternGroup key) (Map Greediness (Map PatternPos [(Text, RawSteno)]))

(<<<$>>>)
  :: forall m n o a b
  . ( Functor m
    , Functor n
    , Functor o
    )
  => (a -> b)
  -> m (n (o a))
  -> m (n (o b))
(<<<$>>>) = fmap <<< fmap <<< fmap

patternDoc :: forall key . Palantype key => PatternDoc key
patternDoc =
    sort <<<$>>> mapPatternDoc
  where
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

    lsExceptions :: [(Text, [(Greediness, RawSteno, PatternGroup key, Bool, PatternPos)])]
    lsExceptions = Map.toList $ mapExceptions <&> \(_, lsEntries) ->
       lsEntries <&> \(g, r, pg, nodoc) -> (g, r, pg, nodoc, PPException)
