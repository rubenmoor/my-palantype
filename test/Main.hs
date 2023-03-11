{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-deferred-type-errors #-}

module Main where

import Palantype.Common.Class (mapExceptions)
import Palantype.Common.Primitives (triePrimitives)
import qualified Palantype.DE as DE
import Control.Monad ((<$!>), unless)
import qualified Data.Map.Strict as Map
import Data.List (sort, sortOn)
import Control.Exception (IOException, catch, ErrorCall (ErrorCall))
import Test.HUnit (assertFailure, runTestTTAndExit, Test (TestList, TestLabel, TestCase), assertEqual, assertBool)
import qualified Palantype.Common.Indices as KI
import Palantype.Common.Indices (parseChordDE)
import qualified Palantype.Common.RawSteno as Raw
import TextShow (TextShow(showt))
import Control.Category ((<<<))
import Data.Text (Text)
import Palantype.Common.AutoDoc (patternDoc)
import Data.Maybe (isJust)
import Palantype.Common.Stage (StageSpecialGeneric(StageGeneric), findStage, mapStages)

primitives :: Test
primitives = TestLabel "primitives.json5" $ TestCase $
    triePrimitives @DE.Key `seq` pure ()

exceptions :: Test
exceptions = TestLabel "exceptions.json5" $ TestCase $
    mapExceptions @DE.Key `seq` pure ()

rawSteno :: Text -> Test
rawSteno str = TestLabel "raw steno code" $ TestCase $
    assertEqual "" str $ roundtrip str
  where
    roundtrip = showt <<< KI.toRaw @DE.Key <<< $parseChordDE <<< Raw.fromText

stages :: Test
stages =
    let lsMisses = Map.foldlWithKey' acc [] patternDoc
    in  TestLabel "Stages.hs DE" $ TestCase $
          unless (null lsMisses) $
            assertFailure $ "Stages missing:\n\n" <> unlines (show <$> lsMisses)
  where
    acc     ls pg mapG = Map.foldlWithKey' (accG pg) ls mapG
    accG pg ls g  _    = case findStage @DE.Key (StageGeneric pg g) of
      Just _ -> ls
      Nothing -> (pg, g) : ls

main :: IO ()
main = runTestTTAndExit $ TestList $
  [ exceptions
  , primitives
  , stages
  ] <> (rawSteno <$>
        [ "NN"
        , "LM"
        , "-NS"
        , "N-+"
        , "N+-"
        , "DSN-"
        , "DSN+-"
        , "-+NSD"
        , "-LNSD"
        , "DUN"
        , "AN"
        , "AU"
        , "GANS"
        , "G-SD"
        , "F+-S"
        , "v"
        , "b"
        , "s"
        , "n"
        ]
       )
