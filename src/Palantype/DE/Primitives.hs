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
import Data.Int (Int)
import           Control.Applicative            ( Applicative
                                                    ( pure
                                                    )
                                                )
import           Control.Monad                  ( MonadPlus(mzero)
                                                , foldM
                                                , when
                                                )
import           Control.Monad.Fail             ( MonadFail(fail) )
import           Data.Aeson                     ( FromJSON(parseJSON)
                                                , Value(Array)
                                                )
import qualified Data.Aeson                    as Aeson
import           Data.Aeson.Types               ( Parser )
import           Data.Either                    ( Either(Left, Right)
                                                , isLeft
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

                                                , head


                                                )
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Monoid                    ( (<>)


                                                )
import           Data.Text                      ( Text


                                                )
import qualified Data.Text                     as Text
import qualified Data.Text.Encoding            as Text
import           GHC.Err                        ( error )
import           Palantype.Common               ( Palantype, PatternGroup
                                                )
import qualified Palantype.Common.RawSteno     as Raw
import qualified Palantype.DE.Keys             as DE
import           Text.Show                      ( Show(show) )
import Data.Bool (Bool(True, False))
import qualified Palantype.DE.Pattern as DE
import Data.Bifunctor (Bifunctor(second, first))

type Greediness = Int

-- | the primitives as defined in "primitives.json"
lsPrimitives :: [(ByteString, [(Greediness, RawSteno, DE.Pattern, Bool)])]
lsPrimitives =
    let str = stripComments $(embedFile "DE/primitives.json5")
    in  Map.toList $ unPrimMap $ case Aeson.eitherDecodeStrict str of
          Right m   -> m :: PrimMap DE.Key DE.Pattern
          Left  err -> error $ "Could not decode primitives.json5: " <> err

lsPatterns :: [(Greediness, [ByteString])]
lsPatterns =
  let
      accPatternsG m (bstr, entries) =
        foldl' (\m' (g, _, _, _) -> Map.insertWith (++) g [bstr] m') m entries
  in
      Map.toList $ foldl' accPatternsG Map.empty lsPrimitives

triePrimitives :: Trie [(Greediness, RawSteno, DE.Pattern)]
triePrimitives =
  Trie.fromList $ lsPrimitives <&> second (fmap (\(g, r, p, _) -> (g, r, p)))

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

patternDoc :: [(DE.Pattern, [(Greediness, [(Text, RawSteno)])])]
patternDoc =
    Map.toList
      $   Map.toList . fmap (fmap $ first Text.decodeUtf8)
      <$> foldl' accByBs Map.empty lsPrimitives
  where
    -- accByBs m (bs, entries) = Map.unionWith (Map.unionWith (++)) m $ foldl' accByPattern
    accByBs m (bs, entries) = foldl' (accByPattern bs) m entries
    accByPattern bs m (g, r, p, bNoDoc) =
      if bNoDoc
      then m
      else Map.insertWith (Map.unionWith (++)) p (Map.singleton g [(bs, r)]) m

-- TODO: move to generic level, module Common
newtype PrimMap key p = PrimMap { unPrimMap :: Map ByteString [(Greediness, RawSteno, p, Bool)] }

instance (Palantype key, PatternGroup p) => FromJSON (PrimMap key p) where

    parseJSON (Array vs) = PrimMap <$> foldM acc Map.empty vs

      where

        acc
            :: Map ByteString [(Greediness, RawSteno, p, Bool)]
            -> Value
            -> Parser (Map ByteString [(Greediness, RawSteno, p, Bool)])

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
            when (isLeft $ Raw.parseSteno @key r)
                $  fail
                $  "malformed raw steno: "
                <> Text.unpack key
                <> ": "
                <> show r
            let keyBs = Text.encodeUtf8 key

            pure $ Map.insertWith (++) keyBs [(g, r, p, bNoDoc)] m

        acc _ other = fail $ "malformed: " <> show other

    parseJSON _ = mzero
