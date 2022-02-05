module Palantype.Common
  ( module Palantype.Common.Class
  , module Palantype.Common.Internal
  , module Palantype.Common.Primitives
  , module Palantype.Common.AutoDoc
  , module Palantype.Common.Indices
  , module Palantype.Common.KeyIndex
  , module Palantype.Common.Dictionary
  , module Palantype.Common.RawSteno
  , MapStenoWordTake100
  ) where

import Palantype.Common.AutoDoc ( PatternDoc, patternDoc )
import Palantype.Common.KeyIndex
    ( fromIndex, keyIndex, toKeyIndices, KeyIndex (..))
import Palantype.Common.Class
    (
      Palantype(..),
      RawSteno(..),
      showH,
      mkChord )
import Palantype.Common.Internal
    ( Chord(..),
      Series(..),
      Finger(..),
      PatternPos(..),
      Greediness,
      Lang(..) )
import Palantype.Common.Primitives
    ( PrimMap(..),
      ExceptionsMap(..),
      lsPatterns,
      triePrimitives,
      stripComments
    )
import Palantype.Common.Indices ( toRaw, KIChord )
import Palantype.Common.RawSteno
    ( fromChord,
      unparts,
      evalStenoPattern,
      parseSteno,
      parseStenoLenient,
      parseWord,
      parseChordLenient,
      parseStenoKey,
      sentence,
      word,
      chord,
      keys,
      keyWithHyphen,
      keyLeftHand,
      keyOrHyphenKey,
      key )
import Palantype.Common.Dictionary
    ( kiUp, kiDown, kiBackUp, kiCapNext, kiEnter, kiAcronym, commands )
import Data.Map.Strict (Map)
import Data.Int (Int)
import Data.Text (Text)

type MapStenoWordTake100 key =
  Map (PatternGroup key) (Map Greediness (Int, [(Text, RawSteno)]))
