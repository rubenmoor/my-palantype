{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Palantype.Common.Class (mapExceptions)
import Palantype.Common.Primitives (triePrimitives)
import qualified Palantype.DE as DE
import Control.Monad ((<$!>))
import qualified Data.Map.Strict as Map
import Data.List (sort, sortOn)
import Control.Exception (IOException, catch, ErrorCall (ErrorCall))
import Test.HUnit (assertFailure, runTestTTAndExit, Test (TestList, TestLabel, TestCase), assertEqual)
import qualified Palantype.Common.Indices as KI
import Palantype.Common.Indices (parseChordDE)
import qualified Palantype.Common.RawSteno as Raw
import TextShow (TextShow(showt))
import Control.Category ((<<<))
import Data.Text (Text)

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

main :: IO ()
main = runTestTTAndExit $ TestList $
  [ exceptions
  , primitives
  ] <> (rawSteno <$>
        [ "NN"
        , "LM"
        , "J-N"
        , "JN-"
        , "BDJ"
        , "BDJN-"
        , "+NSD"
        , "-LNSD"
        , "DUN"
        , "AN"
        , "AU"
        , "GANS"
        , "G-NS"
        , "GN-S"
        ]
       )
