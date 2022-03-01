{-|
special characters: everything on the US keyboard layout that is NOT

* an alphanumeric character
* a commando (e.g. backspace)

Use the right hand to select to special-character input mode (cf. `strModeStenoModifiable`,
and `strModeStenoUnmodifiable`) and the left hand to type.
-}

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Palantype.Common.Dictionary.Special
    ( dictSpecial
    , strModeSteno
    , keysExtra
    ) where

import Data.Text (Text)
import Data.Semigroup (Semigroup((<>)))
import Palantype.Common.Indices (KIChord, parseChordDE)
import Control.Applicative (Applicative(pure))
import Data.Function (($))
import GHC.Base (Char)
import Palantype.Common.Dictionary.Shared (ModifierPrimary(..), ModifierSecondary (..), toPloverCommand, toStenoStrLeftHand, toPloverLiteralGlued)
import qualified Data.Text as Text
import qualified Palantype.Common.RawSteno as Raw

dictSpecial :: [(KIChord, Text)]
dictSpecial = literals <> commands

-- | special keys of category 1: all the special keys on US layout
--   that are not `shift` + some number key.
--   There are 22 of them, or 11, when you allow modification by Shift.
--   11 fit on the fingers of the left hand w/o use of the thumb.
--   This is important to avoid boundary issues (or straight collisions)
--   when using "N" as mode selector
strModeSteno :: Text
strModeSteno = "N" -- key index 25

-- | special characters, with and w/o Shift
keysExtra :: [(Char, Text, Char, Text)]
keysExtra =
    [ ('`' , "~"  , 'S', "grave"       )
    --               H not in use
    , ('-' , "_"  , 'M', "minus"       )
    , ('=' , "+"  , 'L', "equal"       )
    , ('[' , "\\{", 'B', "bracketleft" )
    , (']' , "}"  , 'D', "bracketright")
    , ('\\', "|"  , 'J', "backslash"   )
    , ('\'', "\"" , 'N', "apostrophe"  )
    , (',' , "<"  , 'G', "comma"       )
    , ('.' , ">"  , 'F', "period"      )
    , (';' , ":"  , 'W', "semicolon"   )
    , ('/' , "?"  , 'R', "slash"       )
    ]

literals :: [(KIChord, Text)]
literals = do
    (literal, shifted, steno, _) <- keysExtra
    modSec <- [ModSecNone, ModSecShift]
    pure
        ( $parseChordDE $ Raw.fromText $
              toStenoStrLeftHand strModeSteno
                                 ModPrimNone
                                 modSec
                                 $ Text.singleton steno
        , case modSec of
              ModSecNone  -> toPloverLiteralGlued $ Text.singleton literal
              ModSecShift -> toPloverLiteralGlued shifted
        )

commands :: [(KIChord, Text)]
commands = do
    modPrim <- [ModPrimAlt, ModPrimCtrl, ModPrimWin]
    modSec  <- [ModSecNone, ModSecShift]
    (_, _, chrSteno, strCommand) <- keysExtra
    pure ( $parseChordDE $ Raw.fromText $
               toStenoStrLeftHand strModeSteno
                                  modPrim
                                  modSec
                                  (Text.singleton chrSteno)
         , toPloverCommand modPrim modSec strCommand
         )
