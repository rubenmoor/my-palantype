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
    , kiUp
    , kiDown
    , kiEnter
    , strModeSteno
    ) where

import           Data.Function                  ( ($) )
import           Data.Semigroup                 ( (<>) )
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           Palantype.Common.Indices       ( KIChord, parseChordDE )
import Palantype.Common.Class (RawSteno (RawSteno))
import Palantype.Common.TH (fromJust)
import Control.Applicative (Applicative(pure))
import Data.Char (Char)
import Palantype.Common.Dictionary.Shared (toPloverCommand, ModifierPrimary (..), ModifierSecondary (..), toStenoStrRightHand)
import Data.Functor ((<&>))
import Data.List (lookup)

strModeSteno :: Text
strModeSteno = "JN"

keysModifiable :: [(Text, Char)]
keysModifiable =
  [ ("insert"   , '+')
  , ("delete"   , 'L')
  , ("left"     , 'M')
  , ("home"     , 'G')
  , ("end"      , 'N')
  , ("up"       , 'B')
  , ("page_up"  , 'F')
  , ("page_down", 'S')
  , ("down"     , 'ʃ')
  , ("backspace", 's')
  , ("return"   , 'D')
  , ("right"    , 'n')
  , ("escape"   , 'U')
  , ("tab"      , 'I')
  , ("super"    , 'O')
  , ("space"    , 'Ü')
  ]

keysUnmodifiable :: [(Text, Text)]
keysUnmodifiable =
  [ ("{#control(alt(delete))}", "LNSD")]

dictCommands :: [(KIChord, Text)]
dictCommands = dictModifiable <> dictUnmodifiable

dictModifiable :: [(KIChord, Text)]
dictModifiable = do
    modPrim <- [ModPrimNone, ModPrimAlt, ModPrimCtrl, ModPrimWin]
    modSec  <- [ModSecNone, ModSecShift]
    (strCommand, chrSteno) <- keysModifiable

    pure ( $parseChordDE $ RawSteno $
               toStenoStrRightHand strModeSteno modPrim modSec $ Text.singleton chrSteno
         , toPloverCommand modPrim modSec strCommand
         )

dictUnmodifiable :: [(KIChord, Text)]
dictUnmodifiable = keysUnmodifiable <&> \(strPlover, strSteno) ->
   ( $parseChordDE $ RawSteno $
         toStenoStrRightHand strModeSteno ModPrimNone ModSecNone strSteno
   , strPlover
   )

{-|
arrow key: up
-}
kiUp :: KIChord
kiUp = mkKIChordSimple "up"

mkKIChordSimple :: Text -> KIChord
mkKIChordSimple str =
    let strSteno = Text.singleton $ $fromJust $ lookup str keysModifiable
    in  $parseChordDE $ RawSteno $
            toStenoStrRightHand strModeSteno ModPrimNone ModSecNone strSteno

{-|
arrow key: down
-}
kiDown :: KIChord
kiDown = mkKIChordSimple "down"

{-|
enter key, not to be confused with paragraph
-}
kiEnter :: KIChord
kiEnter = mkKIChordSimple "return"
