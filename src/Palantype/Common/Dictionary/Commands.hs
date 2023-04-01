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

module Palantype.Common.Dictionary.Commands
    ( dictCommands
    , fromIndex
    , kiUp
    , kiDown
    , kiLeft
    , kiRight
    , kiEnter
    , kiPageUp
    , kiPageDown
    , kiHome
    , kiEnd
    , kiInsert
    , kiDelete
    , strModeSteno
    ) where

import           Data.Function                  ( ($) )
import           Data.Semigroup                 ( (<>) )
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           Palantype.Common.Indices       ( KIChord, parseChordDE )
import           Data.Maybe                     ( Maybe(..) )
import Palantype.Common.KeyIndex (KeyIndex)
import Palantype.Common.TH (fromJust, failure)
import Control.Applicative (Applicative(pure))
import Data.Char (Char)
import Palantype.Common.Dictionary.Shared (toPloverCommand, ModifierPrimary (..), ModifierSecondary (..), toStenoStrRightHand)
import Data.Functor ((<&>))
import Data.List (lookup)
import qualified Palantype.Common.RawSteno as Raw

strModeSteno :: Text
strModeSteno = "N+"

keysModifiable :: [(Text, Char)]
keysModifiable =
  [ ("insert"   , 'M')
  , ("delete"   , '+')
  , ("left"     , 'L')
  , ("home"     , 'G')
  , ("end"      , 'N')
  , ("up"       , 'B')
  , ("page_up"  , 'ʃ')
  , ("page_down", 'S')
  , ("down"     , 'F')
  , ("backspace", 'n')
  , ("return"   , 'D')
  , ("right"    , 's')
  , ("escape"   , 'U')
  , ("tab"      , 'I')
  , ("super"    , 'O')
  , ("space"    , 'Ü')
  ]

keysUnmodifiable :: [(Text, Text)]
keysUnmodifiable =
  [ ("{#control(alt(delete))}", "+NSD")
  ]

dictCommands :: [(KIChord, Text)]
dictCommands = dictModifiable <> dictUnmodifiable

dictModifiable :: [(KIChord, Text)]
dictModifiable = do
    modPrim <- [ModPrimNone, ModPrimAlt, ModPrimCtrl, ModPrimWin]
    modSec  <- [ModSecNone, ModSecShift]
    (strCommand, chrSteno) <- keysModifiable

    pure ( $parseChordDE $ Raw.fromText $
               toStenoStrRightHand strModeSteno modPrim modSec $ Text.singleton chrSteno
         , toPloverCommand modPrim modSec strCommand
         )

dictUnmodifiable :: [(KIChord, Text)]
dictUnmodifiable = keysUnmodifiable <&> \(strPlover, strSteno) ->
   ( $parseChordDE $ Raw.fromText $
         toStenoStrRightHand strModeSteno ModPrimNone ModSecNone strSteno
   , strPlover
   )

mkKIChordSimple :: Text -> KIChord
mkKIChordSimple str =
    let strSteno = Text.singleton $ $fromJust $ lookup str keysModifiable
    in  $parseChordDE $ Raw.fromText $
            toStenoStrRightHand strModeSteno ModPrimNone ModSecNone strSteno

{-|
arrow key: up
-}
kiUp :: KIChord
kiUp = mkKIChordSimple "up"

{-|
arrow key: down
-}
kiDown :: KIChord
kiDown = mkKIChordSimple "down"

{-|
arrow key: left
-}
kiLeft :: KIChord
kiLeft = mkKIChordSimple "left"

{-|
arrow key: right
-}
kiRight :: KIChord
kiRight = mkKIChordSimple "right"

{-|
enter key, not to be confused with paragraph
-}
kiEnter :: KIChord
kiEnter = mkKIChordSimple "return"

-- {-|
-- the word "Start" in two versions
-- -}
-- kiChordsStart :: [KIChord]
-- kiChordsStart = $parseChordDE . Raw.fromText <$> ["DSAÜD", "DSAÜ+D"]

{-|
page up key
-}
kiPageUp :: KIChord
kiPageUp = mkKIChordSimple "page_up"

{-|
page down key
-}
kiPageDown :: KIChord
kiPageDown = mkKIChordSimple "page_down"

{-|
home key
-}
kiHome :: KIChord
kiHome = mkKIChordSimple "home"

{-|
end key
-}
kiEnd :: KIChord
kiEnd = mkKIChordSimple "end"

{-|
insert key
-}
kiInsert :: KIChord
kiInsert = mkKIChordSimple "insert"

{-|
delete key
-}
kiDelete :: KIChord
kiDelete = mkKIChordSimple "delete"

{-|
Map a key index to a command key in command mode.
For visualization of the number mode on the virtual keyboard.
-}
fromIndex :: KeyIndex -> Maybe Text
fromIndex = \case
    1  -> Nothing
    2  -> Just "SHIFT"
    3  -> Nothing
    4  -> Just "CTRL"
    5  -> Just "WIN"
    6  -> Just "ALT"
    7  -> Nothing
    8  -> Just "N"
    9  -> Nothing
    10 -> Nothing
    11 -> Just "+"
    12 -> Nothing
    13 -> Nothing
    14 -> Nothing
    15 -> Nothing
    16 -> Nothing
    17 -> Just "ESC"
    18 -> Just "⭾"
    19 -> Just "⊞"
    20 -> Just "␣"
    21 -> Just "INS"
    22 -> Just "DEL"
    23 -> Just "⯇"
    24 -> Just "HOME"
    25 -> Just "END"
    26 -> Just "⯅"
    27 -> Just "⇞"
    28 -> Just "⇟"
    29 -> Just "⯆"
    30 -> Just "⌫"
    31 -> Just "⏎"
    32 -> Just "⯈"
    _  -> $failure "Numbers.fromIndex: impossible"
