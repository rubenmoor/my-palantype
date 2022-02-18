{-|
Common, language-independent dictionary for the "arrow key group" and
some extra commands

The "arrow key group" can be combined with modifier keys (Ctrl, Shift,
Alt, Super), they are mapped to the right hand like this:

           Insert Home      PageUp   Backspace
           Delete End       PageDown Return
           Left   Up        Down     Right

    Escape Tab    Win (Tap) Space

... once the left hand pressed JN

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
    ) where

import           Data.Function                  ( ($) )
import           Data.Semigroup                 ( (<>) )
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           Palantype.Common.Indices       ( KIChord )
import qualified Palantype.Common.Indices      as KI
import Palantype.Common.Class (RawSteno (RawSteno))
import Palantype.Common.TH (fromJust)
import Control.Applicative (Applicative(pure))
import Data.Char (Char)
import Palantype.Common.Dictionary.RightHand (toStenoStr, toPloverStr, ModifierPrimary (..), ModifierSecondary (..))
import Data.Functor ((<&>))
import Data.List (lookup)

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

    pure ( KI.parseChordDE $ RawSteno $
               toStenoStr strModeSteno modPrim modSec $ Text.singleton chrSteno
         , toPloverStr modPrim modSec strCommand
         )

dictUnmodifiable :: [(KIChord, Text)]
dictUnmodifiable = keysUnmodifiable <&> \(strPlover, strSteno) ->
   ( KI.parseChordDE $ RawSteno $
         toStenoStr strModeSteno ModPrimNone ModSecNone strSteno
   , strPlover
   )

strModeSteno :: Text
strModeSteno = "JN"

{-|
arrow key: up
-}
kiUp :: KIChord
kiUp = mkKIChordSimple "up"

mkKIChordSimple :: Text -> KIChord
mkKIChordSimple str =
    let strSteno = Text.singleton $ $fromJust $ lookup str keysModifiable
    in  KI.parseChordDE $ RawSteno $
            toStenoStr strModeSteno ModPrimNone ModSecNone strSteno
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
