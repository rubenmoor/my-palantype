module Palantype.Common
  ( module Palantype.Common.Class
  , module Palantype.Common.Internal
  , module Palantype.Common.SystemLang
  , module Palantype.Common.Primitives
  , module Palantype.Common.AutoDoc
  , module Palantype.Common.Indices
  , module Palantype.Common.KeyIndex
  , module Palantype.Common.Dictionary
  , module Palantype.Common.Dictionary.Commands
  , module Palantype.Common.Dictionary.CommandsFKeys
  , module Palantype.Common.Dictionary.Numbers
  , module Palantype.Common.Dictionary.Plover
  , module Palantype.Common.Dictionary.Special
  , module Palantype.Common.RawSteno
  , module Palantype.Common.RenderPlover
  , module Palantype.Common.RawSteno.Type
  , module Palantype.Common.Stage
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
      ExceptionInterpretation (..),
      Finger(..),
      PatternPos(..),
      Greediness,
      showPretty )
import Palantype.Common.Internal.Instances ()
import Palantype.Common.Primitives
    ( PrimMap(..),
      ExceptionsMap(..) ,
      lsPatterns,
      triePrimitives,
      stripComments
    )
import Palantype.Common.Indices ( toRaw, allKeys, KIChord )
import Palantype.Common.SystemLang ( SystemLang (..) )
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
import Palantype.Common.Dictionary (dictLiterals)
import Palantype.Common.Dictionary.CommandsFKeys (dictFKeys)
import Palantype.Common.Dictionary.Commands
    ( kiEnter, kiDown, kiUp, kiPageUp, kiPageDown, dictCommands
    , kiLeft
    , kiRight
    , kiHome
    , kiEnd
    , kiInsert
    , kiDelete
    )
import Palantype.Common.Dictionary.Plover
    ( dictPlover, kiBackUp, kiCapNext, kiAcronym )
import Palantype.Common.Dictionary.Special (dictSpecial)
import Palantype.Common.Dictionary.Numbers (dictNumbers, kiCtrlNumber, kiFromSmallNumber, kiChordToInt)
import Palantype.Common.RenderPlover (renderPlover)
import Palantype.Common.Stage
    ( findStage
    , getSystemLang
    , getStageIndexMaybe
    , Stage(..)
    , StageRepr
    , StageSpecialGeneric (..)
    , StageHierarchy (..)
    , StageIndex
    , stageIndexPatZero
    , stageIndexPatSimpleMulti
    , stageIndexPatCapitalize
    , stageIndexPatAcronym
    , findStageIndex
    , mapStages
    , mapHierarchyStageIndex
    , toStageRepr
    , toPageName
    , mkStageIndex
    )
