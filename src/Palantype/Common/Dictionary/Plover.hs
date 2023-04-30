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

import           Data.Function                  ( ($) )
import           Data.Text                      ( Text )
import           Palantype.Common.Indices       ( KIChord, parseChordDE)
import Palantype.Common.TH (fromJust)
import Data.List (lookup)
import Data.Functor ((<&>))
import qualified Palantype.Common.RawSteno as Raw

lsCommands :: [(Text, Text)]
lsCommands =

    -- common orthography
    [ ("{^,}"         , "A"   ) -- attach comma
    , ("{^;}"         , "+A"  ) -- attach semicolon
    , ("{^-^}"        , "~"   ) -- hyphen to attach words
    , ("{^\t^}"       , "D+-" ) -- tab like T
    , ("{*-|}"        , "D-"  ) -- capitalize last word retroactively
    , ("{-|}"         , "S-"  ) -- capitalize next word
    , ("{*>}"         , "v"   ) -- uncapitalize last word retroactively
    , ("{*?}"         , "b"   ) -- retroactively add space
    , ("{^.\n\n^}{-|}", "N-"  ) -- paragraph
    , ("{*!}"         , "F-"  ) -- retroactively delete space
    , ("{^.}{-|}"     , "+-"  ) -- full stop: attach period and capitalize next word
    , ("{^:}"         , "M-"  ) -- attach colon
    , ("{^:}{-|}"     , "NM-" ) -- attach colon and capitalize next word
    , ("{^?}{-|}"     , "N+-" ) -- attach question mark and capitalize next word
    , ("{^!}{-|}"     , "NL"  ) -- attach exclamation mark and capitalize next word
    , ("{\\#^}"       , "N+-" ) -- hashtag with next word attached
    , ("§"            , "B+-" ) -- p like paragraph
    , ("{^°}"         , "G-"  ) -- attach °
    , ("{^™}"         , "DM-" ) -- trademark symbol
    , ("{^©}"         , "DʃG-") -- c like copyright
    , ("€"            , "E"   ) -- e like euro
    , ("—"            , "~Ü"  ) -- em dash/Geviertstrich
    , ("{^s^}"        , "s"   ) -- attach s and attach next word

    -- parentheses
    , ("{«^}"  , "-MG" ) -- guillemet: attach to next word
    , ("{^»}"  , "-ʃn") --            attach
    , ("{„^}"  , "-+" ) -- german quotation marks: attach to next word
    , ("{^“}"  , "-N" ) --                         attach
    , ("{‹^}"  , "-L" ) -- chevron: attach to next word
    , ("{^›}"  , "-B" ) --          attach
    , ("{[^}"  , "-F"  ) -- square brackets: attach to next word
    , ("{^]}"  , "n"  ) --                  attach
    , ("{(^}"  , "-S" ) -- parenthesis: attach to next word
    , ("{^)}"  , "-D" ) --              attach
    , ("{\\{^}", "-M"  ) -- brackets: attach to next word
    , ("{^\\}}", "-G" ) --           attach

    -- ascii smileys
    , ("¯\\_(ツ)_/¯"   , "v-+NSD" )
    , ("ʕ•ᴥ•ʔ"        , "D-+NSD" )
    , ("(´･_･`)"       , "b-+NSD" )
    , ("(⊃｡•́‿•̀｡)⊃"     , "ʃ-+NSD" )
    , ("(╯°□°）╯︵ ┻━┻", "S-+NSD" )
    , ("(☞ﾟヮﾟ)☞"      , "F-+NSD" )
    , ("(๑•́ ₃ •̀๑)"     , "G-+NSD" )
    , ("┬─┬⃰͡ (ᵔᵕᵔ͜ )" , "N-+NSD" )
    , ("( ˘ ³˘)♥"      , "B-+NSD" )
    , ("( ͡° ͜ʖ ͡°)"    , "M-+NSD" )
    , ("( ಠ ʖ̯ ಠ )"    , "++NSD" )
    , ("(ᵔᴥᵔ)"         , "L-+NSD" )

    -- plover
    , ("=undo"                   , "I+NSD" ) -- undo last input
    , ("{PLOVER:TOGGLE}"         , "DSN++D")
    , ("{PLOVER:ADD_TRANSLATION}", "DSN+A" )
    , ("{PLOVER:LOOKUP}"         , "DSN+-L") -- plover search dialogue
    , ("{PLOVER:SUGGESTIONS}"    , "DSN+-S") -- plover suggestions window
    , ("{PLOVER:FOCUS}"          , "DSN+-F") -- focus plvoer main window
    , ("{PLOVER:CONFIGURE}"      , "DSN+-G") -- plover configuration window
    , ("{PLOVER:CONFIGURE}"      , "DSN++G") -- quit plover
    ]

dictPlover :: [(KIChord, Text)]
dictPlover = lsCommands <&> \(plover, steno) ->
    ( $parseChordDE $ Raw.fromText steno
    , plover
    )

{-|
back-up, i.e. undo last input
-}
kiBackUp :: KIChord
kiBackUp = mkKIChordSimple "=undo"

{-|
capitalize next chord
-}
kiCapNext :: KIChord
kiCapNext = mkKIChordSimple "{-|}"

{-|
qualifier for acronyms
-}
kiAcronym :: KIChord
kiAcronym = $parseChordDE $ Raw.fromText "NÜM"

mkKIChordSimple :: Text -> KIChord
mkKIChordSimple str =
    let strSteno = $fromJust $ lookup str lsCommands
    in  $parseChordDE $ Raw.fromText strSteno
