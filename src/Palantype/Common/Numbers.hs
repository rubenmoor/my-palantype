{-|
Description: Common, language-independent dictionary for numbers

For simplicity, the commands are defined using Palantype.DE.
But only the generic indices are exported.
-}

{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}

module Palantype.Common.Numbers
    (
    ) where

import           Control.Category               ( (<<<) )
import           Data.Bifunctor                 ( Bifunctor(first) )
import           Data.Function                  ( ($) )
import           Data.Functor                   ( (<$>) )
import           Data.HashMap.Strict            ( HashMap )
import qualified Data.HashMap.Strict           as HashMap
import           Data.Maybe                     ( Maybe(..) )
import           Data.Semigroup                 ( (<>) )
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           Data.Tuple                     ( fst )
import           GHC.Err                        ( error )
import           Palantype.Common.Indices       ( KIChord )
import qualified Palantype.Common.Indices      as KI
import           Palantype.Common.RawSteno      ( parseChordMaybe
                                                )
import qualified Palantype.DE.Keys             as DE
import Palantype.Common.Class (RawSteno (RawSteno))
import Palantype.Common.TH (fromJust)
import Data.Char (Char)

{-|
DE raw steno for the Modifier steno code, i.e.

type this with your left hand and the number code with
your right hand to reach a number
-}
rawModifier :: RawSteno
rawModifier = "WN-"

simpleKIChord :: RawSteno -> KIChord
simpleKIChord = KI.fromChord <<< $fromJust <<< parseChordMaybe @DE.Key

{-| modifier keys, shift isn't really a thing for number keys -}
data Modifier
  = ModNone
  | ModCtrl
  | ModWin
  | ModAlt
  | ModShift

toPloverStr :: Modifier -> Text
toPloverStr = \case
    ModNone  -> ""
    ModCtrl  -> "Control_L"
    ModWin   -> "Super_L"
    ModAlt   -> "Alt_L"
    -- ModShift -> "Shift_L"

toStenoStr :: Modifier -> Text
toStenoStr = \case
    ModNone  -> "WN"
    ModCtrl  -> "HWN"
    ModWin   -> "DWN"
    ModAlt   -> "FWN"
    -- ModShift -> "Shift_L"


modifiers :: [Modifier]
modifiers = [ModNone, ModCtrl, ModWin, ModAlt]

{-|

{#Control_L(0)}
{#Alt_L(0)}
{#Super_L(0)}

cf. https://github.com/openstenoproject/plover/wiki/Dictionary-Format
-}
addModifier :: (Text, Char) -> Modifier -> (Text, Text)
addModifier (strNum, chr) mod =
  -- TODO: insert - where necessary
    ( "{#" <> toPloverStr mod <> "(" <> strNum <> ")}"
    , toStenoStr mod
        <> if last (toKeys chr) >= last (toKeys 'F')
           then "-"
           else ""
        <> Text.fromChar chr

numbersThumb :: [(Text, Char)]
numbersThumb =
  [ ("0"  , 'U')
  , ("1"  , 'I')
  , ("2"  , 'O')
  , ("9"  , 'Ü')

numbersIndex :: [(Text, Char)]
numbersIndex =
  , ("1"  , 'M')
  , ("4"  , 'L')
  , ("7"  , '+')

numbersMiddle :: [(Text, Char)]
numbersMiddle =
  , ("2"  , 'B')
  , ("5"  , 'N')
  , ("8"  , 'G')

numbersRing :: [(Text, Char)]
numbersRing =
  , ("3"  , 'ʃ')
  , ("6"  , 'S')
  , ("9"  , 'F')

numbersPinky :: [(Text, Char)]
numbersPinky =
  , ("0"  , 's')
  , ("00" , 'D')
  , ("000", 'n')
  ]
