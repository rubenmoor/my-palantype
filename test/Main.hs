{-# LANGUAGE TypeApplications #-}

module Main where

import Palantype.Common.Class (mapExceptions)
import Palantype.Common.Primitives (triePrimitives)
import qualified Palantype.DE as DE
import Control.Monad ((<$!>))
import qualified Data.Map.Strict as Map
import Data.List (sort, sortOn)
import Control.Exception (IOException, catch, ErrorCall (ErrorCall))
import Test.HUnit (assertFailure, runTestTTAndExit, Test (TestList, TestLabel, TestCase))

primitives :: Test
primitives = TestLabel "primitives.json5" $ TestCase $
  triePrimitives @DE.Key `seq` pure ()

exceptions :: Test
exceptions = TestLabel "exceptions.json5" $ TestCase $
  mapExceptions @DE.Key `seq` pure ()

main :: IO ()
main = runTestTTAndExit $ TestList
  [ exceptions
  , primitives
  ]
