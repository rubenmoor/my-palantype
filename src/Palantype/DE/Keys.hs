{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Palantype.DE.Keys where

import           Control.Category               ( (<<<) )
import           Palantype.Common.Class         ( Palantype(..) )
import           TextShow                       ( TextShow(..)
                                                , singleton, fromString
                                                )
import Data.Aeson.Types (FromJSON)
import Data.Aeson (ToJSON, ToJSONKey, FromJSONKey)
import GHC.Generics (Generic)
import Text.Show (Show, show)
import Text.Read (Read, readMaybe)
import Servant.API (FromHttpApiData (parseUrlPiece), ToHttpApiData (toUrlPiece))
import qualified Data.Text as Text
import Data.Maybe (maybe)
import Data.Either (Either(..))
import qualified Data.Map.Strict as Map
import qualified Data.Aeson as Aeson
import Palantype.Common.Primitives (PrimMap (..), stripComments, ExceptionsMap (unExceptionsMap))
import Data.FileEmbed (embedFile)
import Data.Function (($))
import GHC.Err (error)
import Data.Monoid ((<>))
import Control.DeepSeq (NFData)
import           Data.Data                      ( Data )
import           Data.Eq                        ( Eq )
import           Data.Ord                       ( Ord )

-- the palantype.de keyboard
--
-- S|Z   H/e  M  L       .     + G    F/W/V  -s/-er
-- B|P   D|T  J  N       .     L N    S/Z|Tz D/T
-- G|K   F/V  W  R/er-   .     M B|P  ʃ|ç    -en
-- thumb      Ä  E  A ~  . U I O Ü

-- a key on a steno keyboard
data Key
  = LeftSZ
  | LeftBP
  | LeftGK
  | LeftHE
  | LeftDT
  | LeftFV
  | LeftM
  | LeftJ
  | LeftW
  | LeftL
  | LeftN
  | LeftREr
  | LeftAUmlaut
  | LeftE
  | LeftA
  | LeftStretch
  | RightU
  | RightI
  | RightOStretch
  | RightUUmlaut
  | RightModifier
  | RightL
  | RightM
  | RightGK
  | RightN
  | RightBP
  | RightFWVIv
  | RightSZTz
  | RightSchCh
  | RightSE
  | RightDT
  | RightEn
  deriving stock (Eq, Ord, Data)

instance Palantype Key where
    type PatternGroup Key = Pattern

    keyCode = \case
        LeftSZ        -> 'S'
        LeftBP        -> 'B'
        LeftGK        -> 'G'
        LeftHE        -> 'H'
        LeftDT        -> 'D'
        LeftFV        -> 'F'
        LeftM         -> 'M'
        LeftJ         -> 'J'
        LeftW         -> 'W'
        LeftL         -> 'L'
        LeftN         -> 'N'
        LeftREr       -> 'R'
        LeftAUmlaut   -> 'Ä'
        LeftE         -> 'E'
        LeftA         -> 'A'
        LeftStretch   -> '~'
        RightU        -> 'U'
        RightI        -> 'I'
        RightOStretch -> 'O'
        RightUUmlaut  -> 'Ü'
        RightModifier -> '+'
        RightL        -> 'L'
        RightM        -> 'M'
        RightGK       -> 'G'
        RightN        -> 'N'
        RightBP       -> 'B'
        RightFWVIv    -> 'F'
        RightSZTz     -> 'S'
        RightSchCh    -> 'ʃ' -- U+0283
        RightSE       -> 's'
        RightDT       -> 'D'
        RightEn       -> 'n'

    patSimpleMulti = PatSimpleMulti
    patCapitalize = PatCapitalize

    toDescription = \case
      PatSimple -> "Identical letters"
      PatSimpleMulti -> "Identical letters, multiple chords"
      PatSmallS -> "Making use of the (small) s key"
      PatReplCommon -> "Replacements for common letters"
      PatDiConsonant -> "Double consonants"
      PatCodaH -> "Long vowels"
      PatCodaR -> "Vowels followed by r"
      PatCodaRR -> "Vowels followed by -rr"
      PatCodaHR -> "Vowels followed by -hr"
      PatDt -> "The dt-rule"
      PatDiphtong -> "Multiple vowels"
      PatReplC -> "Different replacements for c"

      -- TODO
      PatCodaGK -> "G and k in the coda"

      PatSZ -> "The letter ß"
      PatIJ -> "Using J for i"
      PatSwapS -> "S-swapping in the coda"
      PatSwapSch -> "Sch-swapping in the coda"
      PatSwapZ -> "Z-swapping in the coda"
      PatDiVowel -> "Double vowels"
      PatReplH -> "Replacing a silent h"
      PatReplRare -> "Replacements for less common letters"
      PatSCStretch -> "Irregular stretch key"
      PatSCPlus -> "Irregular plus key"
      PatSCOther -> "Unspecific irregularities"
      PatCapitalize -> "Explicit capitalization"
      PatShortSyllable -> "Efficiency for short syllables"
      PatBrief -> "Brief for a common word"
      PatAcronym -> "Acronym spellings"
      PatChemistry -> "Symbols of chemical elements"
      PatCommonPrefix -> "Efficiency for common prefixes"
      PatAnglAI -> "Anglicisms with ÄI"
      PatAnglAE -> "Anglicisms with Ä"
      PatAnglI -> "Anglicisms with I"
      PatAnglEI -> "Anglicisms with EI"
      PatAnglA -> "Anglicisms with A"
      PatAnglU -> "Anglicisms with U"
      PatAnglStretchO -> "Anglicisms with ~O"
      PatAnglJU -> "Anglicisms with JU"
      PatAnglO -> "Anglicisms with O"
      PatAnglStretchU -> "Anglicisms with ~U"
      PatAnglAU -> "Anglicisms with AU"
      PatAnglSch -> "Anglicisms with SJ and ʃ"
      PatAnglOther -> "Other anglicisms"
      PatFrankOther -> "Gallicisms"
      PatForeignOther -> "Other foreign words"

    lsPrimitives =
        let str = stripComments $(embedFile "DE/primitives.json5")
        in  Map.toList $ unPrimMap $ case Aeson.eitherDecodeStrict str of
              Right m   -> m :: PrimMap Key
              Left  err -> error $ "Could not decode DE/primitives.json5: " <> err

    mapExceptions =
        -- TODO: "DE" depends on key
        let str = stripComments $(embedFile "DE/exceptions.json5")
        in  unExceptionsMap $ case Aeson.eitherDecodeStrict str of
                Right map -> map :: ExceptionsMap Key
                Left  err -> error $ "Could not decode exceptions.json5: " <> err

instance TextShow Key where
    showb = singleton <<< keyCode

data Pattern
  -- simple pattern
  = PatSimple
  | PatSimpleMulti
  | PatSmallS
  -- common replacement rules
  | PatReplCommon
  | PatDiConsonant
  | PatCodaH
  | PatCodaR
  | PatCodaRR
  | PatCodaHR
  | PatDt
  | PatDiphtong
  | PatReplC
  -- TODO
  | PatCodaGK
  | PatSZ
  | PatIJ
  | PatSwapS
  | PatSwapSch
  | PatSwapZ
  | PatDiVowel
  | PatReplH
  -- rare replacement rules and special cases
  | PatReplRare
  | PatSCStretch
  | PatSCPlus
  | PatSCOther
  -- rules for capitalization
  | PatCapitalize
  -- rules for increased efficiency
  | PatShortSyllable
  | PatBrief
  | PatCommonPrefix
  -- acronyms
  | PatAcronym
  | PatChemistry
  -- anglicisms
  | PatAnglAI
  | PatAnglAE
  | PatAnglI
  | PatAnglEI
  | PatAnglA
  | PatAnglU
  | PatAnglStretchO
  | PatAnglJU
  | PatAnglO
  | PatAnglStretchU
  | PatAnglAU
  | PatAnglSch
  | PatAnglOther
  -- other foreign words
  | PatFrankOther
  | PatForeignOther

  deriving stock (Data, Eq, Generic, Ord, Read, Show)

instance FromJSON Pattern
instance FromJSONKey Pattern
instance NFData Pattern
instance ToJSON Pattern
instance ToJSONKey Pattern

instance TextShow Pattern where
  showb = fromString <<< show

instance ToHttpApiData Pattern where
  toUrlPiece = Text.pack <<< show

instance FromHttpApiData Pattern where
  parseUrlPiece =
    maybe (Left "failed to read: Pattern") Right <<< readMaybe <<< Text.unpack
