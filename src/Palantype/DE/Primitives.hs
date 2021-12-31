{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell            #-}

module Palantype.DE.Primitives
  ( mapExceptions
  , triePrimitives
  , lsPrimitives
  , lsPatterns
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
import           Data.Function                  ( ($)

                                                )
import           Data.Functor                   ( (<$>)


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
import           Palantype.Common               ( Palantype
                                                )
import qualified Palantype.Common.RawSteno     as Raw
import qualified Palantype.DE.Keys             as DE
import           Text.Show                      ( Show(show) )

type Greediness = Int

-- | the primitives as defined in "primitives.json" parsed to a TRIE
lsPrimitives :: [(ByteString, [(Greediness, RawSteno)])]
lsPrimitives =
    let str = stripComments $(embedFile "DE/primitives.json5")
    in  Map.toList $ unPrimMap $ case Aeson.eitherDecodeStrict str of
          Right m   -> m :: PrimMap DE.Key
          Left  err -> error $ "Could not decode primitives.json5: " <> err

lsPatterns :: [(Greediness, [ByteString])]
lsPatterns =
  let
      accPatternsG m (bstr, entries) =
        foldl' (\m' (g, _) -> Map.insertWith (++) g [bstr] m') m entries
  in  Map.toList $ foldl' accPatternsG Map.empty lsPrimitives

triePrimitives :: Trie [(Greediness, RawSteno)]
triePrimitives = Trie.fromList lsPrimitives

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

newtype PrimMap key = PrimMap { unPrimMap :: Map ByteString [(Greediness, RawSteno)] }

instance Palantype key => FromJSON (PrimMap key) where

    parseJSON (Array vs) = PrimMap <$> foldM acc Map.empty vs

      where

        acc
            :: Map ByteString [(Greediness, RawSteno)]
            -> Value
            -> Parser (Map ByteString [(Greediness, RawSteno)])

        acc m jv@(Array vec) = do
            (k, v) <- case toList vec of
                [k, Array v] -> pure (k, v)
                _            -> fail $ "malformed entry: " <> show jv

            key    <- parseJSON k
            (g, r) <- case toList v of
                [vG, vRaw] -> do
                    g   <- parseJSON vG
                    raw <- parseJSON vRaw
                    pure (g, raw)
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

            pure $ Map.insertWith (++) keyBs [(g, r)] m

        acc _ other = fail $ "malformed: " <> show other

    parseJSON _ = mzero
