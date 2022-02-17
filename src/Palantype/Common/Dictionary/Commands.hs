{-|
Common, language-independent dictionary for the "arrow key group",
"extra commands", and "plover commands".

The "arrow key group" can be combined with modifier keys (Ctrl, Shift,
Alt, Super):

    Insert Home   PageUp   Backspace
    Delete End    PageDown Return
    Left   Up     Down     Right

           Escape Tab      Win (Tap) Space

missing:

caps lock
pause
print screen

"Extra commands" cannot be combined with any modifier keys

    Ctrl+Alt+Delete

"Plover commands" cannot be combined with any modifier keys

    back up (remove last input)
    toggle plover
    full stop
    paragraph
    indent
    ...

For simplicity, the commands are defined using Palantype.DE.
But only the generic indices are exported.
-}

{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}

module Palantype.Common.Dictionary.Commands
    ( commands
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

{-|
DE raw steno for back-up command, i.e. undo last input
-}
rawBackUp :: RawSteno
rawBackUp = "ILNSD"

{-|
DE raw steno for capitalization of next word
-}
rawCapNext :: RawSteno
rawCapNext = "BDJNN"

simpleKIChord :: RawSteno -> KIChord
simpleKIChord = KI.fromChord <<< $fromJust <<< parseChordMaybe @DE.Key

txtUp :: Text
txtUp = "D"

txtDown :: Text
txtDown = "J"

txtEnter :: Text
txtEnter = "A"

unmodifiedKIChord :: Text -> KIChord
unmodifiedKIChord str =
    simpleKIChord $ fst $ mkModified str []

{-|
arrow key: up
-}
kiUp :: KIChord
kiUp = unmodifiedKIChord txtUp

{-|
arrow key: down
-}
kiDown :: KIChord
kiDown = unmodifiedKIChord txtDown

{-|
back-up, i.e. undo last input
-}
kiBackUp :: KIChord
kiBackUp = simpleKIChord rawBackUp

{-|
capitalize next chord
-}
kiCapNext :: KIChord
kiCapNext = simpleKIChord rawCapNext

{-|
enter key, not to be confused with paragraph
-}
kiEnter :: KIChord
kiEnter = unmodifiedKIChord txtEnter

{-|
qualifier for acronyms
-}
kiAcronym :: KIChord
kiAcronym = simpleKIChord "NÜM"

data Modifier
  = ModShift
  | ModCtrl
  | ModAlt

mkModified :: Text -> [Modifier] -> (RawSteno, Text)
mkModified str [] = case HashMap.lookup str mapENModify of
    Just ploverCode -> (RawSteno $ str <> commandKeys, ploverCode)
    Nothing -> error $ "mkModified: not found in map: " <> Text.unpack str
    where commandKeys = "-MNSD"
mkModified _ _ = error "mkModified: not implemented"

{-|
Common commands that can be modified with Shift, Control, Alt.
TODO: how to combine CTRL, SHIFT, ALT if applicable? Maybe using left thumb?
-}
mapENModify :: HashMap Text Text
mapENModify = HashMap.fromList
    [ (txtUp   , "{#up}")
    , (txtDown , "{#down}")
    , ("B"     , "{#left}")
    , ("N"     , "{#right}")
    , (txtEnter, "{#return}")
    , ("G"     , "{#tab}")
    , ("S"     , "{#home}")
    , ("L"     , "{#end}")
    , ("H"     , "{#pageup}")
    , ("M"     , "{#pagedown}")
    ]

{-|
more commands that are steno specific and cannot be modified by CTRL, SHIFT, ALT
cf. https://github.com/openstenoproject/plover/wiki/Dictionary-Format#capitalizing
-}
mapEN :: [(RawSteno, Text)]
mapEN =
    [ (rawBackUp , "=undo")
    , (rawCapNext, "{-|}")  -- plover: capitalize next word
    , ("BDJNLNSD", "{PLOVER:TOGGLE}")
    ]

commands :: HashMap KIChord Text
commands = HashMap.union m mModified
  where
    m = HashMap.fromList $ first simpleKIChord <$> mapEN
    mModified =
        HashMap.fromList
            $   first unmodifiedKIChord
            <$> HashMap.toList mapENModify

-- TODO:
--  [ ("{*-|}"        , "S") -- capitalize last word retroactively
--  , ("{-|}"         , "B") -- capitalize next word
--  , ("{*>}"         , "G") -- uncapitalize last word retroactively
--  , ("{*?}"         , "H") -- retroactively add space
--  , ("{^.\n\n^}{-|}", "D") -- paragraph
--  , ("*"            , "DM") -- "* ": markdown paragraph
--  , ("{*!}"         , "F") -- retroactively delete space
--  , ("{^.}{-|}"     , "J") -- full stop: . w/o space and capitalize next word
--  , ("{^:}{-|}"     , "JL") -- full stop: . w/o space and capitalize next word
--  , ("{^?}{-|}"     , "JN") -- full stop: . w/o space and capitalize next word
--  , ("{^!}{-|}"     , "JR") -- full stop: . w/o space and capitalize next word
--  --  TODO: N
--  , ("{^,}"         , "A") -- attachkomma
--  , ("{^;}"         , "NA") -- attach semicolon
--  , ("{^:}"         , "LA") -- attach colon
--  , ("{^-^}"        , "H") -- hyphen to attach words
--  , ("{^\t^}", "DJ")
