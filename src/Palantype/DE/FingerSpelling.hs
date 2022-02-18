{-|
fingerspelling: access to all the letters of the alphabet individually
-}

module Palantype.DE.FingerSpelling
    ( dictFingerSpelling
    ) where

import Data.Text (Text)
import Palantype.Common.Indices (KIChord, parseChordDE)
import Control.Applicative (Applicative(pure))
import Palantype.Common.Class (RawSteno(RawSteno))
import Data.Function (($))
import Palantype.Common.Dictionary.LeftHand (ModifierPrimary (..), ModifierSecondary(..), toStenoStr, toPloverStr)
import Data.Char (Char, toUpper)
import qualified Data.Text as Text
import Control.Category ((<<<))

strModeSteno :: Text
strModeSteno = "L"

keys :: [(Char, Text)]
keys =
  [ ('a', "A"   )
  , ('á', "BA"  )
  , ('à', "JA"  )
  , ('b', "B-"  )
  , ('c', "GDM-")
  , ('ç', "GDJ-")
  , ('d', "D-"  )
  , ('e', "E"   )
  , ('é', "BE"  )
  , ('ê', "DE"  )
  , ('è', "JE"  )
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
  , ('ô', "DO"  )
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
  , ('ä', "Ä"   )
  , ('ö', "ÄO"  )
  , ('ü', "Ü"   )
  , ('ß', "GFW-")
  ]

dictFingerSpelling :: [(KIChord, Text)]
dictFingerSpelling = do
    modPrim <- [ModPrimNone, ModPrimAlt, ModPrimCtrl, ModPrimWin]
    modSec  <- [ModSecNone, ModSecShift]
    (plover, steno) <- keys
    pure ( parseChordDE $ RawSteno $ toStenoStr strModeSteno modPrim modSec steno
         , toPloverStr modPrim modSec (Text.singleton <<< toUpper) plover
         )
