{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Palantype.Common.Primitives
    ( triePrimitives
    , lsPatterns
    , PrimMap(..)
    , ExceptionsMap(..)
    , stripComments
    ) where

import           Control.Applicative            ( Applicative(pure) )
import           Control.Monad                  ( MonadPlus(mzero)
                                                , foldM
                                                , when
                                                )
import           Control.Monad.Fail             ( MonadFail(fail) )
import           Data.Aeson                     ( FromJSON(parseJSON)
                                                , Value(Array)
                                                )
import           Data.Aeson.Types               ( Parser )
import           Data.Bifunctor                 ( Bifunctor(second) )
import           Data.Bool                      ( Bool(False, True) )
import           Data.ByteString                ( ByteString )
import           Data.Either                    ( Either(Left, Right)
                                                , isLeft
                                                )
import           Data.Foldable                  ( Foldable
                                                    ( foldl'
                                                    , length
                                                    , toList
                                                    )
                                                )
import           Data.Function                  ( ($) )
import           Data.Functor                   ( (<$>)
                                                , (<&>)
                                                , Functor(fmap)
                                                )
import           Data.List                      ( (++)
                                                , head
                                                )
import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as Map
import           Data.Monoid                    ( (<>) )
import           Data.Ord                       ( Ord((>=)) )
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import qualified Data.Text.Encoding            as Text
import           Data.Trie                      ( Trie )
import qualified Data.Trie                     as Trie
import           Palantype.Common.Class         ( Palantype
                                                    ( PatternGroup
                                                    , lsPrimitives
                                                    )
                                                )
import           Palantype.Common.Internal      ( Greediness
                                                , PatternPos(..)
                                                , ExceptionInterpretation (..)
                                                )
import qualified Palantype.Common.RawSteno     as Raw
import           Palantype.Common.RawSteno.Type ( RawSteno )
import           Text.Show                      ( Show(show) )

lsPatterns :: forall key . Palantype key => [((PatternGroup key, Greediness), [ByteString])]
lsPatterns =
    let accPatternsG m (bstr, entries) = foldl'
            (\m' (g, _, pg, _, _) -> Map.insertWith (++) (pg, g) [bstr] m')
            m
            entries
    in  Map.toList $ foldl' accPatternsG Map.empty (lsPrimitives @key)

triePrimitives
    :: forall key
     . Palantype key
    => Trie [(Greediness, RawSteno, PatternGroup key)]
triePrimitives = Trie.fromList $ lsPrimitives <&> second
    (fmap (\(g, r, p, _, _) -> (g, r, p)))

stripComments :: ByteString -> ByteString
stripComments content =
    let txt = Text.decodeUtf8 content
    in  Text.encodeUtf8 $ Text.unlines $ stripComment <$> Text.lines txt
  where
    stripComment :: Text -> Text
    stripComment str = head $ Text.splitOn "//" str

newtype ExceptionsMap key = ExceptionsMap
  { unExceptionsMap
      :: Map Text
           ( ExceptionInterpretation
           , [(Greediness, RawSteno, PatternGroup key, Bool)]
           )
  }

instance (Palantype key) => FromJSON (ExceptionsMap key) where

    parseJSON (Array vs) = ExceptionsMap <$> foldM acc Map.empty vs

      where
        acc m jv@(Array vec) = do
            (k, interp, rem) <- case toList vec of
                k : vInterp : rem | length rem >= 3 -> case vInterp of
                    "rule-addition" -> pure (k, ExcRuleAddition, rem)
                    "substitution"  -> pure (k, ExcSubstitution, rem)
                    _ ->
                        fail
                            $  "unknown exception interpretation: "
                            <> show vInterp
                            <> ", must be one of \"substitution\", \"rule-addition\""
                _ -> fail $ "malformed entry: " <> show jv

            key    <- parseJSON k
            tuples <- parseEntries key rem
            pure $ Map.insert key (interp, tuples) m

        acc _ other = fail $ "malformed: " <> show other

        parseEntries _   []                       = pure []
        parseEntries key (vG : vSteno : vPat : r) = do
            g     <- parseJSON vG
            steno <- parseJSON vSteno
            when (isLeft $ Raw.parseSteno @key steno)
                $  fail
                $  "malformed raw steno: "
                <> show key
                <> ": "
                <> show steno
            pat <- parseJSON vPat
            let (bNoDoc, rem) = case r of
                    "no-doc" : vas -> (True, vas)
                    vas            -> (False, vas)
            ((g, steno, pat, bNoDoc) :) <$> parseEntries key rem
        parseEntries key _ = fail $ "Uneven number of entries for " <> show key

    parseJSON _ = mzero

newtype PrimMap key = PrimMap
  { unPrimMap :: Map ByteString [(Greediness, RawSteno, PatternGroup key, Bool, PatternPos)]
  }

instance Palantype key => FromJSON (PrimMap key) where

    parseJSON (Array vs) = PrimMap <$> foldM acc Map.empty vs

      where

        acc :: Map ByteString [(Greediness, RawSteno, PatternGroup key, Bool, PatternPos)]
            -> Value
            -> Parser ( Map ByteString [
                          ( Greediness
                           , RawSteno
                           , PatternGroup key
                           , Bool
                           , PatternPos
                           )])

        acc m jv@(Array vec) = do
            (k, v) <- case toList vec of
                [k, Array v] -> pure (k, v)
                _            -> fail $ "malformed entry: " <> show jv

            key               <- parseJSON k
            (g, r, p, bNoDoc) <- case toList v of
                vG : vRaw : vPattern : rem -> do
                    g      <- parseJSON vG
                    raw    <- parseJSON vRaw
                    pat    <- parseJSON vPattern
                    bNoDoc <- case rem of
                        []         -> pure False
                        ["no-doc"] -> pure True
                        _          -> fail $ "malformed entry: " <> show rem
                    pure (g, raw, pat, bNoDoc)
                _ ->
                    fail
                        $  "malformed entry: "
                        <> Text.unpack key
                        <> ": "
                        <> show v
            pPos <- case Raw.evalStenoPattern @key r of
                Left err ->
                    fail
                        $  "malformed raw steno: "
                        <> Text.unpack key
                        <> ": "
                        <> show r
                        <> " "
                        <> Text.unpack err
                Right pp -> pure pp

            let keyBs = Text.encodeUtf8 key

            pure $ Map.insertWith (++) keyBs [(g, r, p, bNoDoc, pPos)] m

        acc _ other = fail $ "malformed: " <> show other

    parseJSON _ = mzero
