{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Palantype.Common.Primitives
    ( triePrimitives
    , lsPatterns
    , module Palantype.Common.Primitives.Types
    ) where

import           Data.Bifunctor                 ( Bifunctor(second) )
import           Data.ByteString                ( ByteString )
import           Data.Foldable                  ( Foldable
                                                    ( foldl'
                                                    )
                                                )
import           Data.Function                  ( ($) )
import           Data.Functor                   ( (<&>)
                                                , Functor(fmap)
                                                )
import qualified Data.Map.Strict               as Map
import           Data.Monoid                    ( (<>) )
import           Data.Trie                      ( Trie )
import qualified Data.Trie                     as Trie
import           Palantype.Common.Class         ( Palantype
                                                    ( lsPrimitives
                                                    )
                                                )
import Palantype.Common.Stage ( StageIndex, getStageIndexMaybe )
import           Palantype.Common.RawSteno.Type ( RawSteno )
import           Text.Show                      ( Show(show) )
import Data.Maybe (Maybe(..))
import Palantype.Common.TH (failure)

import Palantype.Common.Primitives.Types

lsPatterns :: forall key . Palantype key => [(StageIndex, [ByteString])]
lsPatterns =
    Map.toList $ foldl' accFunc Map.empty (lsPrimitives @key)
  where
    accFunc m (bstr, entries) = foldl' accFuncPatterns m entries
      where
        accFuncPatterns m' (g, _, pg, _, _) =
          let si = case getStageIndexMaybe pg g of
                Just    si' -> si'
                Nothing     -> $failure $ "No stage for " <> show pg <> " " <> show g
          in  Map.insertWith (<>) si [bstr] m'

triePrimitives
    :: forall key
     . Palantype key
    => Trie [(RawSteno, StageIndex)]
triePrimitives =
  Trie.fromList $ lsPrimitives @key <&> second (fmap (\(g, r, p, _, _) ->
    let si = case getStageIndexMaybe p g of
            Just    si' -> si'
            Nothing     -> $failure $ "No stage for " <> show p <> " " <> show g
    in  (r, si)))
