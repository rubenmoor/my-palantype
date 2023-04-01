{- |
fingerspelling: access to all the letters of the alphabet individually

select the fingerspelling mode with the right hand, see `strModeSteno`,
and type with the left hand.
-}

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Palantype.DE.FingerSpelling
  ( dictFingerSpelling
  , dictLiterals
  , keysLetterUS
  , keysLetterOther
  , strModeSteno
  ) where

import Control.Applicative (Applicative (pure))
import Data.Char (Char, toUpper)
import Data.Function (($))
import Data.Text (Text)
import qualified Data.Text as Text
import Palantype.Common.Dictionary.Shared (ModifierPrimary (..), ModifierSecondary (..), toStenoStrLeftHand, toPloverLiteralGlued, toPloverCommand)
import Palantype.Common.Indices (KIChord, parseChordDE)
import Data.Semigroup (Semigroup((<>)))
import qualified Palantype.Common.RawSteno as Raw

{- | DE raw steno for the fingerspelling mode
   The key is -+, but will be combined and the - will be added on demand
-}
strModeSteno :: Text
strModeSteno = "+"

-- | letters from the US keyboard layout can be used as commands
--   when combined with a primary modifier: ctrl, alt or win
keysLetterUS :: [(Char, Text)]
keysLetterUS =
    [ ('a', "A"   )
    , ('b', "B-"  )
    , ('c', "DʃG-")
    , ('d', "D-"  )
    , ('e', "E"   )
    , ('f', "F-"  )
    , ('g', "G-"  )
    , ('h', "SN-" )
    , ('i', "I"   )
    , ('j', "FN-" )
    , ('k', "G+-" )
    , ('l', "L-"  )
    , ('m', "M-"  )
    , ('n', "N-"  )
    , ('o', "O"   )
    , ('p', "B+-" )
    , ('q', "FG+-")
    , ('r', "SM-" )
    , ('s', "S-"  )
    , ('t', "D+-" )
    , ('u', "U"   )
    , ('v', "FBL-")
    , ('w', "F+"  )
    , ('x', "DSG-")
    , ('y', "ÄI"  )
    , ('z', "ʃG-" )
    ]

-- | letters that are not part of the US keyboard layout cannot be used as
--   commands. They are only combined with the secondary modifier shift.
keysLetterOther :: [(Char, Text)]
keysLetterOther =
    [ ('á', "DA"  )
    , ('à', "NA"  )
    , ('ç', "DʃG+-")
    , ('é', "DE"  )
    , ('ê', "SE"  )
    , ('è', "NE"  )
    , ('ô', "SO"  )
    , ('ä', "Ä"   )
    , ('ö', "ÄO"  )
    , ('ü', "Ü"   )
    , ('ß', "S+"  )
    ]

dictFingerSpelling :: [(KIChord, Text)]
dictFingerSpelling = gluedLiterals <> letterCommands

-- | reduced dictionary for an exercise of learn-palantype,
--   only unmodified and shift-modified keys, no plover syntax
dictLiterals :: [(KIChord, Text)]
dictLiterals = do
    (literal, steno) <- keysLetterUS <> keysLetterOther
    modSec <- [ModSecNone, ModSecShift]
    pure
        ( $parseChordDE $ Raw.fromText $
              toStenoStrLeftHand strModeSteno ModPrimNone modSec steno
        , Text.singleton $ case modSec of
              ModSecNone  -> literal
              ModSecShift -> toUpper literal
        )

gluedLiterals :: [(KIChord, Text)]
gluedLiterals = do
    (literal, steno) <- keysLetterUS <> keysLetterOther
    modSec <- [ModSecNone, ModSecShift]
    pure
        ( $parseChordDE $ Raw.fromText $
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
        ( $parseChordDE $ Raw.fromText $
              toStenoStrLeftHand strModeSteno modPrim modSec steno
        , toPloverCommand modPrim
                          modSec
                          $ Text.singleton literal
        )
