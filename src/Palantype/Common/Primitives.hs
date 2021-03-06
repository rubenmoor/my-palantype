{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Palantype.Common.Primitives
  ( triePrimitives
  , lsPatterns
  , PrimMap (..)
  , ExceptionsMap (..)
  , stripComments
  ) where

import           Data.Trie                      ( Trie )
import qualified Data.Trie                     as Trie
import Data.ByteString ( ByteString, ByteString )
import           Control.Applicative            ( Applicative
                                                    ( pure
                                                    )
                                                )
import           Control.Monad                  ( MonadPlus(mzero)
                                                , foldM, when
                                                )
import           Control.Monad.Fail             ( MonadFail(fail) )
import           Data.Aeson                     ( FromJSON(parseJSON)
                                                , Value(Array)
                                                )
import           Data.Aeson.Types               ( Parser )
import           Data.Either                    ( Either(Left, Right), isLeft

                                                )
import           Data.Foldable                  ( Foldable
                                                    ( foldl'
                                                    , toList, length
                                                    )
                                                )
import           Data.Function                  ( ($)
                                                )
import           Data.Functor                   ( (<$>), Functor (fmap), (<&>)
                                                )
import           Data.List                      ( (++)
                                                , head
                                                )
import           Data.Map.Strict                       ( Map )
import qualified Data.Map.Strict                      as Map
import           Data.Monoid                    ( (<>)
                                                )
import           Data.Text                      ( Text)
import qualified Data.Text                     as Text
import qualified Data.Text.Encoding            as Text
import           Palantype.Common.Internal      ( Greediness, PatternPos (..)
                                                )
import qualified Palantype.Common.RawSteno     as Raw
import           Text.Show                      ( Show(show) )
import Data.Bool (Bool(True, False))
import Data.Bifunctor (Bifunctor(second))
import Data.Ord (Ord((>=)))
import Palantype.Common.Class (Palantype (PatternGroup, lsPrimitives))
import Palantype.Common.RawSteno.Type (RawSteno)

lsPatterns
  :: forall key
  . Palantype key
  => [(Greediness, [ByteString])]
lsPatterns =
  let
      accPatternsG m (bstr, entries) =
        foldl' (\m' (g, _, _, _, _) -> Map.insertWith (++) g [bstr] m') m entries
  in
      Map.toList $ foldl' accPatternsG Map.empty (lsPrimitives @key)

triePrimitives
  :: forall key
  .  Palantype key
  => Trie [(Greediness, RawSteno, PatternGroup key)]
triePrimitives =
  Trie.fromList $ lsPrimitives <&> second (fmap (\(g, r, p, _, _) -> (g, r, p)))

stripComments :: ByteString -> ByteString
stripComments content =
    let txt = Text.decodeUtf8 content
    in  Text.encodeUtf8 $ Text.unlines $ stripComment <$> Text.lines txt
  where
    stripComment :: Text -> Text
    stripComment str = head $ Text.splitOn "//" str

newtype ExceptionsMap key = ExceptionsMap
  { unExceptionsMap :: Map Text [(RawSteno, PatternGroup key)]
  }

instance (Palantype key) => FromJSON (ExceptionsMap key) where

  parseJSON (Array vs) = ExceptionsMap <$> foldM acc Map.empty vs

    where
      acc m jv@(Array vec) = do
        (k, rem) <- case toList vec of
          k:rem | length rem >= 2 -> pure (k, rem)
          _ -> fail $ "malformed entry: " <> show jv

        key <- parseJSON k
        pairs <- parseEntries key rem
        pure $ Map.insert key pairs m
      acc _ other = fail $ "malformed: " <> show other

      parseEntries _ [] = pure []
      parseEntries key (vSteno:vPat:r) = do
        steno <- parseJSON vSteno
        when (isLeft $ Raw.parseSteno @key steno) $
          fail $ "malformed raw steno: "
              <> show key <> ": "
              <> show steno
        pat <- parseJSON vPat
        ((steno, pat) :) <$> parseEntries key r
      parseEntries key _ = fail $ "Uneven number of entries for " <> show key

  parseJSON _ = mzero

newtype PrimMap key = PrimMap
  { unPrimMap :: Map ByteString [(Greediness, RawSteno, PatternGroup key, Bool, PatternPos)]
  }

instance Palantype key => FromJSON (PrimMap key) where

    parseJSON (Array vs) = PrimMap <$> foldM acc Map.empty vs

      where

        acc
            :: Map ByteString [(Greediness, RawSteno, PatternGroup key, Bool, PatternPos)]
            -> Value
            -> Parser (Map ByteString [(Greediness, RawSteno, PatternGroup key, Bool, PatternPos)])

        acc m jv@(Array vec) = do
            (k, v) <- case toList vec of
                [k, Array v] -> pure (k, v)
                _            -> fail $ "malformed entry: " <> show jv

            key    <- parseJSON k
            (g, r, p, bNoDoc) <- case toList v of
                vG : vRaw : vPattern : rem -> do
                    g   <- parseJSON vG
                    raw <- parseJSON vRaw
                    pat <- parseJSON vPattern
                    bNoDoc <- case rem of
                      [] -> pure False
                      ["no-doc"] -> pure True
                      _ -> fail $ "malformed entry: "
                               <> show rem
                    pure (g, raw, pat, bNoDoc)
                _ ->
                    fail
                        $  "malformed entry: "
                        <> Text.unpack key
                        <> ": "
                        <> show v
            pPos <- case Raw.evalStenoPattern @key r of
              Left err -> fail $  "malformed raw steno: "
                              <> Text.unpack key <> ": " <> show r <> " "
                              <> Text.unpack err
              Right pp -> pure pp

            let keyBs = Text.encodeUtf8 key

            pure $ Map.insertWith (++) keyBs [(g, r, p, bNoDoc, pPos)] m

        acc _ other = fail $ "malformed: " <> show other

    parseJSON _ = mzero
