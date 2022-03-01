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
    , ("{^;}"         , "NA"  ) -- attach semicolon
    , ("{^-^}"        , "~"   ) -- hyphen to attach words
    , ("{^\t^}"       , "DJ"  ) -- tab like T
    , ("{*-|}"        , "B-"  ) -- capitalize last word retroactively
    , ("{-|}"         , "D-"  ) -- capitalize next word
    , ("{*>}"         , "S-"  ) -- uncapitalize last word retroactively
    , ("{*?}"         , "G-"  ) -- retroactively add space
    , ("{^.\n\n^}{-|}", "J"   ) -- paragraph
    , ("{*!}"         , "F-"  ) -- retroactively delete space
    , ("{^.}{-|}"     , "N-"  ) -- full stop: attach period and capitalize next word
    , ("{^:}"         , "L-"  ) -- attach colon
    , ("{^:}{-|}"     , "JL-" ) -- attach colon and capitalize next word
    , ("{^?}{-|}"     , "JN-" ) -- attach question mark and capitalize next word
    , ("{^!}{-|}"     , "JR"  ) -- attach exclamation mark and capitalize next word
    , ("{\\#^}"       , "H"   ) -- hashtag with next word attached
    , ("§"            , "BD-" ) -- p like paragraph
    , ("{^°}"         , "GD-" ) -- attach °
    , ("{^™}"         , "DM-" ) -- t like trademark
    , ("{^©}"         , "GDM-") -- c like copyright
    , ("€"            , "E"   ) -- e like euro
    , ("—"            , "~Ü"  ) -- em dash/Geviertstrich

    -- parentheses
    , ("{«^}"  , "+" ) -- guillemet: attach to next word
    , ("{^»}"  , "-G") --            attach
    , ("{„^}"  , "-L") -- german quotation marks: attach to next word
    , ("{^“}"  , "-N") --                         attach
    , ("{‹^}"  , "-M") -- chevron: attach to next word
    , ("{^›}"  , "-B") --          attach
    , ("{[^}"  , "-F") -- square brackets: attach to next word
    , ("{^]}"  , "s" ) --                  attach
    , ("{(^}"  , "-S") -- parenthesis: attach to next word
    , ("{^)}"  , "-D") --              attach
    , ("{\\{^}", "ʃ" ) -- brackets: attach to next word
    , ("{^\\}}", "n" ) --           attach

    -- ascii smileys
    , ("¯\\_(ツ)_/¯"   , "SLNSD" )
    , ("ʕ•ᴥ•ʔ"        , "BLNSD" )
    , ("(´･_･`)"       , "GLNSD" )
    , ("(⊃｡•́‿•̀｡)⊃"     , "HLNSD" )
    , ("(╯°□°）╯︵ ┻━┻", "DLNSD" )
    , ("(☞ﾟヮﾟ)☞"      , "FLNSD" )
    , ("(๑•́ ₃ •̀๑)"     , "MLNSD" )
    , ("┬─┬⃰͡ (ᵔᵕᵔ͜ )" , "JLNSD" )
    , ("( ˘ ³˘)♥"      , "WLNSD" )
    , ("( ͡° ͜ʖ ͡°)"    , "LLNSD" )
    , ("( ಠ ʖ̯ ಠ )"    , "NLNSD" )
    , ("(ᵔᴥᵔ)"         , "RLNSD" )

    -- plover
    , ("=undo"                   , "ILNSD" ) -- undo last input
    , ("{PLOVER:TOGGLE}"         , "BDJN+D")
    , ("{PLOVER:ADD_TRANSLATION}", "BDJNA" )
    , ("{PLOVER:LOOKUP}"         , "BDJNL" ) -- plover search dialogue
    , ("{PLOVER:SUGGESTIONS}"    , "BDJNS" ) -- plover suggestions window
    , ("{PLOVER:FOCUS}"          , "BDJNF" ) -- focus plvoer main window
    , ("{PLOVER:CONFIGURE}"      , "BDJNG" ) -- plover configuration window
    , ("{PLOVER:CONFIGURE}"      , "BDJN+G") -- quit plover
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
