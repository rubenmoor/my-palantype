{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Palantype.Common.Dictionary
  ( dictLiterals
  ) where

import Data.Char (Char)
import Data.Semigroup (Semigroup ((<>)))
import Data.Function (($))

import Palantype.Common.Indices (KIChord, parseChordDE)
import Palantype.Common.Dictionary.Numbers (dictNumberLiterals)
import Palantype.Common.Dictionary.Special (dictSpecialLiterals)
import Palantype.DE.FingerSpelling (dictFingerspellingLiterals)
import qualified Palantype.Common.RawSteno as Raw

dictWhiteSpaceLiterals :: [(KIChord, Char)]
dictWhiteSpaceLiterals =
  [ ( $parseChordDE $ Raw.fromText "N+Ü" , ' ' )
  , ( $parseChordDE $ Raw.fromText "N+-D", '\n')
  , ( $parseChordDE $ Raw.fromText "N+I" , '\t')
  ]

dictLiterals :: [(KIChord, Char)]
dictLiterals =
     dictNumberLiterals
  <> dictSpecialLiterals
  <> dictFingerspellingLiterals
  <> dictWhiteSpaceLiterals
