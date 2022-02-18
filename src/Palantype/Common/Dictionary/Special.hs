{-|
special characters: everything on the US keyboard layout that is not

* an alphanumeric character
* a commando (e.g. backspace)

-}

{-# LANGUAGE TemplateHaskell #-}

module Palantype.Common.Dictionary.Special
    ( dictSpecialModifiable
    , dictSpecialUnmodifiable
    ) where

import Data.Text (Text)
import Data.Semigroup (Semigroup((<>)))
import Palantype.Common.Indices (KIChord, parseChordDE)
import Control.Applicative (Applicative(pure))
import Palantype.Common.Class (RawSteno(RawSteno))
import Data.Function (($))
import Palantype.Common.TH (failure)
import GHC.Base (Char)
import qualified Data.Map.Strict as Map
import Palantype.Common.Dictionary.LeftHand (ModifierPrimary(..), ModifierSecondary (..), toStenoStr, toPloverStr)
import qualified Data.Text as Text
import Text.Show (Show(show))
import Data.Functor ( (<&>) )

-- | special keys of category 1: all the special keys on US layout
--   that are not `shift` + some number key.
--   There are 22 of them, or 11, when you allow modification by Shift.
--   11 fit on the fingers of the left hand w/o use of the thumb.
--   This is important to avoid boundary issues (or straight collisions)
--   when using "N" as mode selector
strModeStenoModifiable :: Text
strModeStenoModifiable = "N"

keysModifiable :: [(Char, Char)]
keysModifiable =
    [ ('`' , 'S')
    --        H   not in use
    , ('-' , 'M')
    , ('=' , 'L')
    , ('[' , 'B')
    , (']' , 'D')
    , ('\\', 'J')
    , ('\'', 'N')
    , (',' , 'G')
    , ('.' , 'F')
    , (';' , 'W')
    , ('/' , 'R')
    ]

dictSpecialModifiable :: [(KIChord, Text)]
dictSpecialModifiable = do
    modPrim <- [ModPrimNone, ModPrimAlt, ModPrimCtrl, ModPrimWin]
    modSec  <- [ModSecNone, ModSecShift]
    (plover, steno) <- keysModifiable
    pure ( parseChordDE $ RawSteno $
               toStenoStr strModeStenoModifiable
                          modPrim
                          modSec
                          (Text.singleton steno)
         , toPloverStr modPrim modSec shiftSpecialCharUS plover
         )

shiftSpecialCharUS :: Char -> Text
shiftSpecialCharUS chr =
    Map.findWithDefault ($failure $ "Unknown character: " <> show chr)
                        chr
                        mapChars
  where
    mapChars = Map.fromList
        [ ('`' , "~"  )
        , ('-' , "_"  )
        , ('=' , "+"  )
        , ('\\', "|"  )
        , ('[' , "\\{") -- plover dictionary syntax: { needs escaping
        , (']' , "}"  )
        , (';' , ":"  )
        , (',' , "<"  )
        , ('.' , ">"  )
        , ('/' , "?"  )
        , ('\'', "\""  )
        ]

strModeStenoUnmodifiable :: Text
strModeStenoUnmodifiable = "LN"

keysUnmodifiable :: [(Char, Text)]
keysUnmodifiable =
    [ ('!', "S" )
    , ('@', "H" )
    , ('#', "M" )
    , ('$', "L" )
    , ('§', "B" )
    , ('%', "D" )
    , ('^', "J" )
    , ('&', "N" )
    , ('*', "G" )
    , ('(', "F" )
    , (')', "W" )
    , ('°', "R" )
    , ('«', "GF")
    , ('»', "WR")
    , ('„', "BD")
    , ('“', "JN")
    , ('™', "Ä" )
    , ('©', "E" )
    , ('€', "A" )
    , ('—', "~" )
    ]

dictSpecialUnmodifiable :: [(KIChord, Text)]
dictSpecialUnmodifiable = keysUnmodifiable <&> \(plover, steno) ->
    ( parseChordDE $ RawSteno $
          toStenoStr strModeStenoUnmodifiable
                     ModPrimNone
                     ModSecNone
                     steno
    , toPloverStr ModPrimNone ModSecNone ($failure "impossible") plover
    )
