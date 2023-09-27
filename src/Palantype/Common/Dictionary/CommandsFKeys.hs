{-|
Common, language-independent dictionary for the "arrow key group" and
some extra commands

Use the left hand for mode selection (cf. strModeSteno) and the right hand to type.

The "arrow key group" can be combined with modifier keys (Ctrl, Shift,
Alt, Super), they are mapped to the right hand like this:

           Insert Home      PageUp   Backspace
           Delete End       PageDown Return
           Left   Up        Down     Right

    Escape Tab    Win (Tap) Space

missing:

caps lock
pause
print screen

"Extra commands" cannot be combined with any modifier keys

    Ctrl+Alt+Delete

For simplicity, the commands are defined using Palantype.DE.
But only the generic indices are exported.
-}

{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}

module Palantype.Common.Dictionary.CommandsFKeys
    ( dictFKeys
    , strModeSteno
    , fromIndex
    ) where

import           Data.Function                  ( ($) )
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           Palantype.Common.Indices       ( KIChord, parseChordDE )
import           Data.Maybe                     ( Maybe(..) )
import Palantype.Common.KeyIndex (KeyIndex)
import Palantype.Common.TH (failure)
import Control.Applicative (Applicative(pure))
import Data.Char (Char)
import Palantype.Common.Dictionary.Shared (toPloverCommand, ModifierPrimary (..), ModifierSecondary (..), toStenoStrRightHand)
import qualified Palantype.Common.RawSteno as Raw
import qualified Data.Set as Set
import Data.Set (notMember)
import Control.Monad (guard)

strModeSteno :: Text
strModeSteno = "G+"

fKeys :: [(Text, Char)]
fKeys =
  [ ("F3" , 'M')
  , ("F2" , '+')
  , ("F1" , 'L')
  , ("F6" , 'G')
  , ("F5" , 'N')
  , ("F4" , 'B')
  , ("F9" , 'ʃ')
  , ("F8" , 'S')
  , ("F7" , 'F')
  , ("Caps_Lock", 'n')
  -- , ("PAUSE" , 'D') -- can't find this key in plover
  -- , ("PRINT" , 's') -- can't find this key in plover
  , ("F10", 'U')
  , ("F11", 'I')
  , ("F12", 'O')
  -- , ("NUM", 'Ü') -- can't find this key in plover
  ]

dictFKeys :: [(KIChord, Text)]
dictFKeys = do
    modPrim <- [ModPrimNone, ModPrimAlt, ModPrimCtrl, ModPrimWin]
    modSec  <- [ModSecNone, ModSecShift]
    (strCommand, chrSteno) <- fKeys

    guard $ (modPrim, modSec, strCommand) `notMember` exclusions

    pure ( $parseChordDE $ Raw.fromText $
               toStenoStrRightHand strModeSteno modPrim modSec $ Text.singleton chrSteno
         , toPloverCommand modPrim modSec strCommand
         )
  where
    exclusions = Set.fromList
      [ (ModPrimAlt , ModSecNone, "F12")
      , (ModPrimWin , ModSecNone, "F11")
      , (ModPrimCtrl, ModSecNone, "F12")
      ]

{-|
Map a key index to a command key in command mode.
For visualization of the command key mode on the virtual keyboard.
-}
fromIndex :: KeyIndex -> Maybe Text
fromIndex = \case
    1  -> Nothing
    2  -> Just "SHIFT"
    3  -> Nothing
    4  -> Just "CTRL"
    5  -> Just "WIN"
    6  -> Just "ALT"
    7  -> Just "G"
    8  -> Nothing
    9  -> Nothing
    10 -> Nothing
    11 -> Just "+"
    12 -> Nothing
    13 -> Nothing
    14 -> Nothing
    15 -> Nothing
    16 -> Nothing
    17 -> Just "F10"
    18 -> Just "F11"
    19 -> Just "F12"
    20 -> Nothing
    21 -> Just "F3"
    22 -> Just "F2"
    23 -> Just "F1"
    24 -> Just "F6"
    25 -> Just "F5"
    26 -> Just "F4"
    27 -> Just "F9"
    28 -> Just "F8"
    29 -> Just "F7"
    30 -> Just "CPSLCK"
    31 -> Nothing
    32 -> Nothing
    _  -> $failure "Numbers.fromIndex: impossible"
