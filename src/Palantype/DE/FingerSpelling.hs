{- |
fingerspelling: access to all the letters of the alphabet individually

select the fingerspelling mode with the right hand, see `strModeSteno`,
and type with the left hand.
-}

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Palantype.DE.FingerSpelling
  ( dictFingerSpelling
  , keysLetterUS
  , keysLetterOther
  , strModeSteno
  ) where

import Control.Applicative (Applicative (pure))
import Data.Char (Char, toUpper)
import Data.Function (($))
import Data.Text (Text)
import qualified Data.Text as Text
import Palantype.Common.Class (RawSteno (RawSteno))
import Palantype.Common.Dictionary.Shared (ModifierPrimary (..), ModifierSecondary (..), toStenoStrLeftHand, toPloverLiteralGlued, toPloverCommand)
import Palantype.Common.Indices (KIChord, parseChordDE)
import Data.Semigroup (Semigroup((<>)))

{- | DE raw steno for the fingerspelling mode
   The key is -L, but will be combined and the - will be added on demand
-}
strModeSteno :: Text
strModeSteno = "L"

-- | letters from the US keyboard layout can be used as commands
--   when combined with a primary modifier: ctrl, alt or win
keysLetterUS :: [(Char, Text)]
keysLetterUS =
    [ ('a', "A"   )
    , ('b', "B-"  )
    , ('c', "GDM-")
    , ('d', "D-"  )
    , ('e', "E"   )
    , ('f', "F-"  )
    , ('g', "G-"  )
    , ('h', "H-"  )
    , ('i', "I"   )
    , ('j', "J"   )
    , ('k', "GD-" )
    , ('l', "L-"  )
    , ('m', "M-"  )
    , ('n', "N-"  )
    , ('o', "O"   )
    , ('p', "BD-" )
    , ('q', "GDW-")
    , ('r', "R"   )
    , ('s', "S-"  )
    , ('t', "BD-" )
    , ('u', "U"   )
    , ('v', "FW"  )
    , ('w', "W"   )
    , ('x', "GDM-")
    , ('y', "ÄI"  )
    , ('z', "SHM-")
    ]

-- | letters that are not part of the US keyboard layout cannot be used as
--   commands. They are only combined with the secondary modifier shift.
keysLetterOther :: [(Char, Text)]
keysLetterOther =
    [ ('á', "BA"  )
    , ('à', "JA"  )
    , ('ç', "GDJ-")
    , ('é', "BE"  )
    , ('ê', "DE"  )
    , ('è', "JE"  )
    , ('ô', "DO"  )
    , ('ä', "Ä"   )
    , ('ö', "ÄO"  )
    , ('ü', "Ü"   )
    , ('ß', "GFW-")
    ]

dictFingerSpelling :: [(KIChord, Text)]
dictFingerSpelling = gluedLiterals <> letterCommands

gluedLiterals :: [(KIChord, Text)]
gluedLiterals = do
    (literal, steno) <- keysLetterUS <> keysLetterOther
    modSec <- [ModSecNone, ModSecShift]
    pure
        ( $parseChordDE $ RawSteno $
              toStenoStrLeftHand strModeSteno ModPrimNone modSec steno
        , toPloverLiteralGlued $ Text.singleton $ case modSec of
              ModSecNone  -> literal
              ModSecShift -> toUpper literal
        )

letterCommands :: [(KIChord, Text)]
letterCommands = do
    modPrim <- [ModPrimAlt, ModPrimCtrl, ModPrimWin]
    modSec <- [ModSecNone, ModSecShift]
    (literal, steno) <- keysLetterUS
    pure
        ( $parseChordDE $ RawSteno $
              toStenoStrLeftHand strModeSteno modPrim modSec steno
        , toPloverCommand modPrim
                          modSec
                          $ Text.singleton literal
        )
