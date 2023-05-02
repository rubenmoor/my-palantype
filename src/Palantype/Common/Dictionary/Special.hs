{-|
special characters: everything on the US keyboard layout that is NOT

* an alphanumeric character
* a commando (e.g. backspace)

Use the right hand to select the special-character input mode (cf. `strModeStenoModifiable`,
and `strModeStenoUnmodifiable`) and the left hand to type.
-}

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Palantype.Common.Dictionary.Special
    ( dictSpecial
    , dictSpecialLiterals
    , fromIndex
    , strModeSteno
    , keysExtra
    ) where

import Data.Text (Text)
import Data.Semigroup (Semigroup((<>)))
import Data.Maybe (Maybe (..))
import Palantype.Common.Indices (KIChord, parseChordDE)
import Palantype.Common.KeyIndex (KeyIndex)
import Palantype.Common.TH (failure)
import Control.Applicative (Applicative(pure))
import Data.Function (($))
import GHC.Base (Char)
import Palantype.Common.Dictionary.Shared (ModifierPrimary(..), ModifierSecondary (..), toPloverCommand, toStenoStrLeftHand, toPloverLiteralGlued)
import qualified Data.Text as Text
import qualified Palantype.Common.RawSteno as Raw

dictSpecial :: [(KIChord, Text)]
dictSpecial = gluedLiterals <> dictCommands

-- | special keys of category 1: all the special keys on US layout
--   that are not `shift` + some number key.
--   There are 22 of them, or 11, when you allow modification by Shift.
--   11 fit on the fingers of the left hand w/o use of the thumb.
--   This is important to avoid boundary issues (or straight collisions)
--   when using "N" as mode selector
strModeSteno :: Text
strModeSteno = "N" -- key index 25

-- | special characters, with and w/o Shift
keysExtra :: [(Char, Char, Text, Char, Text)]
keysExtra =
    [ ('`' , '~'  , "~"  , 'v', "grave"       )
    --                      ʃ not in use
    , ('-' , '_'  , "_"  , 'G', "minus"       )
    , ('=' , '+'  , "+"  , 'M', "equal"       )
    , ('[' , '{'  , "\\{", 'D', "bracketleft" )
    , (']' , '}'  , "}"  , 'S', "bracketright")
    , ('\\', '|'  , "|"  , 'N', "backslash"   )
    , ('\'', '\"' , "\"" , '+', "apostrophe"  )
    , (',' , '<'  , "<"  , 'b', "comma"       )
    , ('.' , '>'  , ">"  , 'F', "period"      )
    , (';' , ':'  , ":"  , 'B', "semicolon"   )
    , ('/' , '?'  , "?"  , 'L', "slash"       )
    ]

-- | literals, plover syntax, e.g. {&[}
gluedLiterals :: [(KIChord, Text)]
gluedLiterals = do
    (literal, _, shiftedPlover, steno, _) <- keysExtra
    modSec <- [ModSecNone, ModSecShift]
    pure
        ( $parseChordDE $ Raw.fromText $
              toStenoStrLeftHand strModeSteno
                                 ModPrimNone
                                 modSec
                                 $ Text.singleton steno
        , case modSec of
              ModSecNone  -> toPloverLiteralGlued $ Text.singleton literal
              ModSecShift -> toPloverLiteralGlued shiftedPlover
        )

-- | literals, for use in exercise learn-palantype, no plover syntax
dictSpecialLiterals :: [(KIChord, Char)]
dictSpecialLiterals = do
    (literal, shifted, _, steno, _) <- keysExtra
    modSec <- [ModSecNone, ModSecShift]
    pure
        ( $parseChordDE $ Raw.fromText $
              toStenoStrLeftHand strModeSteno
                                 ModPrimNone
                                 modSec
                                 $ Text.singleton steno
        , case modSec of
              ModSecNone  -> literal
              ModSecShift -> shifted
        )

-- | commands, plover syntax, e.g. shift(backslash)
dictCommands :: [(KIChord, Text)]
dictCommands = do
    modPrim <- [ModPrimAlt, ModPrimCtrl, ModPrimWin]
    modSec  <- [ModSecNone, ModSecShift]
    (_, _, _, chrSteno, strCommand) <- keysExtra
    pure ( $parseChordDE $ Raw.fromText $
               toStenoStrLeftHand strModeSteno
                                  modPrim
                                  modSec
                                  (Text.singleton chrSteno)
         , toPloverCommand modPrim modSec strCommand
         )

{-|
Map a key index to a special character in special character mode.
For visualization of the special character mode on the virtual keyboard.
-}
fromIndex :: KeyIndex -> Maybe (Text, Text)
fromIndex = \case
    1  -> Just ("`" , "~" )
    2  -> Just ("[" , "{" )
    3  -> Just ("," , "<" )
    4  -> Nothing
    5  -> Just ("]" , "}" )
    6  -> Just ("." , ">" )
    7  -> Just ("-" , "_" )
    8  -> Just ("\\", "|" )
    9  -> Just (";" , ":" )
    10 -> Just ("=" , "+" )
    11 -> Just ("'" , "\"")
    12 -> Just ("/" , "?" )
    13 -> Nothing
    14 -> Nothing
    15 -> Nothing
    16 -> Nothing
    17 -> Nothing
    18 -> Nothing
    19 -> Nothing
    20 -> Nothing
    21 -> Nothing
    22 -> Nothing
    23 -> Nothing
    24 -> Nothing
    25 -> Just ("N", "N")
    26 -> Nothing
    27 -> Just ("CTRL", "CTRL")
    28 -> Just ("WIN" , "WIN" )
    29 -> Just ("ALT" , "ALT" )
    30 -> Nothing
    31 -> Just ("SHIFT", "SHIFT")
    32 -> Nothing
    _  -> $failure "Numbers.fromIndex: impossible"
