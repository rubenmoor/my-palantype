module Palantype.Common
  ( module Palantype.Common.Class
  , module Palantype.Common.Internal
  , module Palantype.Common.Primitives
  , module Palantype.Common.AutoDoc
  , module Palantype.Common.Indices
  , module Palantype.Common.KeyIndex
  , module Palantype.Common.Dictionary.Commands
  , module Palantype.Common.Dictionary.Numbers
  , module Palantype.Common.Dictionary.Plover
  , module Palantype.Common.Dictionary.Special
  , module Palantype.Common.RawSteno
  , module Palantype.Common.RenderPlover
  , module Palantype.Common.RawSteno.Type
  , MapStenoWordTake100
  ) where

import Palantype.Common.AutoDoc ( PatternDoc, patternDoc )
import Palantype.Common.KeyIndex
    ( fromIndex, keyIndex, toKeyIndices, KeyIndex (..))
import Palantype.Common.Class
    (
      Palantype(..),
      mkChord )
import Palantype.Common.Internal
    ( Chord(..),
      Series(..),
      Finger(..),
      PatternPos(..),
      Greediness,
      Lang(..) )
import Palantype.Common.Internal.Instances ()
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
      parseStenoMaybe,
      parseWord,
      parseChordMaybe,
      parseStenoKey,
      sentence,
      word,
      chord,
      keys,
      keyWithHyphen,
      keyLeftHand,
      keyOrHyphenKey,
      key )
import Palantype.Common.RawSteno.Type (RawSteno)
import Palantype.Common.Dictionary.Commands
    ( kiEnter, kiDown, kiUp, kiPageUp, kiPageDown, dictCommands, kiChordsStart )
import Palantype.Common.Dictionary.Plover
    ( dictPlover, kiBackUp, kiCapNext, kiAcronym )
import Palantype.Common.Dictionary.Special (dictSpecial)
import Palantype.Common.Dictionary.Numbers (dictNumbers)
import Data.Map.Strict (Map)
import Data.Int (Int)
import Data.Text (Text)
import Palantype.Common.RenderPlover (renderPlover)

type MapStenoWordTake100 key =
  Map (PatternGroup key) (Map Greediness (Int, [(Text, RawSteno)]))
