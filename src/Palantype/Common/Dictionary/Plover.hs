{-|
Plover commands for steno typing

    back up (remove last input)
    toggle plover
    full stop
    paragraph
    indent
    ...
-}

{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}

module Palantype.Common.Dictionary.Plover
    ( kiBackUp
    , kiCapNext
    , kiAcronym
    , dictPlover
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

lsCommands :: [(Text, Text)]
lsCommands =
    [ ("{^,}"         , "A"  ) -- attach comma
    , ("{^;}"         , "NA" ) -- attach semicolon
    , ("{^-^}"        , "~"  ) -- hyphen to attach words
    , ("{^\t^}"       , "DJ" ) -- tab: like T
    , ("{*-|}"        , "B-" ) -- capitalize last word retroactively
    , ("{-|}"         , "D-" ) -- capitalize next word
    , ("{*>}"         , "S-" ) -- uncapitalize last word retroactively
    , ("{*?}"         , "G-" ) -- retroactively add space
    , ("{^.\n\n^}{-|}", "J"  ) -- paragraph
    , ("*"            , "DM-") -- "* ": markdown paragraph
    , ("{*!}"         , "F-" ) -- retroactively delete space
    , ("{^.}{-|}"     , "N-" ) -- full stop: period (capitalize next word)
    , ("{^:}"         , "L-" ) -- attach colon
    , ("{^:}{-|}"     , "JL-") -- full stop: . w/o space and capitalize next word
    , ("{^?}{-|}"     , "JN-") -- full stop: . w/o space and capitalize next word
    , ("{^!}"         , "R"  ) -- attach exclamation mark
    , ("{^!}{-|}"     , "JR" ) -- full stop exclamation mark
    , ("{PLOVER:TOGGLE}", "BDJNLNSD")
    ]

dictPlover :: [(KIChord, Text)]
dictPlover = []

{-|
DE raw steno for back-up command, i.e. undo last input
-}
rawBackUp :: RawSteno
rawBackUp = "ILNSD"

{-|
DE raw steno for capitalization of next word
-}
rawCapNext :: RawSteno
rawCapNext = "D"

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
qualifier for acronyms
-}
kiAcronym :: KIChord
kiAcronym = simpleKIChord "NÜM"

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
