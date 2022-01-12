{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell            #-}

module Palantype.DE.Primitives
  ( mapExceptions
  , triePrimitives
  , lsPrimitives
  , lsPatterns
  , patternDoc
  , Greediness
  ) where

import Palantype.Common.RawSteno
    ( RawSteno )
import           Data.Trie                      ( Trie )
import qualified Data.Trie                     as Trie
import Data.ByteString ( ByteString, ByteString )
import           Data.FileEmbed                 ( embedFile )
import           Control.Applicative            ( Applicative
                                                    ( pure
                                                    )
                                                )
import           Control.Monad                  ( MonadPlus(mzero)
                                                , foldM
                                                )
import           Control.Monad.Fail             ( MonadFail(fail) )
import           Data.Aeson                     ( FromJSON(parseJSON)
                                                , Value(Array)
                                                )
import qualified Data.Aeson                    as Aeson
import           Data.Aeson.Types               ( Parser )
import           Data.Either                    ( Either(Left, Right)

                                                )
import           Data.Foldable                  ( Foldable
                                                    ( foldl'
                                                    , toList
                                                    )
                                                )
import           Data.Function                  ( ($), (.)
                                                )
import           Data.Functor                   ( (<$>), Functor (fmap), (<&>)
                                                )
import           Data.HashMap.Strict            ( HashMap )
import           Data.List                      ( (++)
                                                , head, sort
                                                )
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Monoid                    ( (<>)
                                                )
import           Data.Text                      ( Text)
import qualified Data.Text                     as Text
import qualified Data.Text.Encoding            as Text
import           GHC.Err                        ( error )
import           Palantype.Common               ( Palantype, PatternGroup, Greediness, PatternPos
                                                )
import qualified Palantype.Common.RawSteno     as Raw
import qualified Palantype.DE.Keys             as DE
import           Text.Show                      ( Show(show) )
import Data.Bool (Bool(True, False))
import qualified Palantype.DE.Pattern as DE
import Data.Bifunctor (Bifunctor(second, first))
import Control.Category ((<<<))

-- | the primitives as defined in "primitives.json"
lsPrimitives :: [(ByteString, [(Greediness, RawSteno, DE.Pattern, Bool, PatternPos)])]
lsPrimitives =
    let str = stripComments $(embedFile "DE/primitives.json5")
    in  Map.toList $ unPrimMap $ case Aeson.eitherDecodeStrict str of
          Right m   -> m :: PrimMap DE.Key DE.Pattern
          Left  err -> error $ "Could not decode primitives.json5: " <> err

lsPatterns :: [(Greediness, [ByteString])]
lsPatterns =
  let
      accPatternsG m (bstr, entries) =
        foldl' (\m' (g, _, _, _, _) -> Map.insertWith (++) g [bstr] m') m entries
  in
      Map.toList $ foldl' accPatternsG Map.empty lsPrimitives

triePrimitives :: Trie [(Greediness, RawSteno, DE.Pattern)]
triePrimitives =
  Trie.fromList $ lsPrimitives <&> second (fmap (\(g, r, p, _, _) -> (g, r, p)))

stripComments :: ByteString -> ByteString
stripComments content =
    let txt = Text.decodeUtf8 content
    in  Text.encodeUtf8 $ Text.unlines $ stripComment <$> Text.lines txt
  where
    stripComment :: Text -> Text
    stripComment str = head $ Text.splitOn "//" str

-- | full word exceptions
-- exceptions that span several chords go here
mapExceptions :: HashMap Text [RawSteno]
mapExceptions =
    let str = stripComments $(embedFile "DE/exceptions.json5")
    in  case Aeson.eitherDecodeStrict str of
            Right ls  -> ls
            Left  err -> error $ "Could not decode exceptions.json5: " <> err

patternDoc :: [(DE.Pattern, [(Greediness, [(PatternPos, [(Text, RawSteno)])])])]
patternDoc =
    Map.toList
      $   Map.toList . fmap (Map.toList <<< fmap (sort <<< fmap (first Text.decodeUtf8)))
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

-- TODO: move to generic level, module Common
newtype PrimMap key p =
  PrimMap { unPrimMap :: Map ByteString [(Greediness, RawSteno, p, Bool,  PatternPos)] }

instance (Palantype key, PatternGroup p) => FromJSON (PrimMap key p) where

    parseJSON (Array vs) = PrimMap <$> foldM acc Map.empty vs

      where

        acc
            :: Map ByteString [(Greediness, RawSteno, p, Bool, PatternPos)]
            -> Value
            -> Parser (Map ByteString [(Greediness, RawSteno, p, Bool, PatternPos)])

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
