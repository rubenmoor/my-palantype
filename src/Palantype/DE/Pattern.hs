{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
module Palantype.DE.Pattern where

import Data.Ord (Ord)
import Data.Eq (Eq)
import Palantype.Common (PatternGroup (toDescription))
import Data.Data (Data)
import Data.Aeson (FromJSON)
import GHC.Generics (Generic)
import Servant.API (FromHttpApiData (..), ToHttpApiData (..))
import Text.Show (show, Show)
import qualified Data.Text as Text
import Control.Category ((<<<))
import Text.Read (readMaybe, Read)
import Data.Either (Either(..))
import Data.Maybe (maybe)

data Pattern
  = PatSimple
  | PatReplCommon
  | PatDiConsonant
  | PatCodaH
  | PatCodaR
  | PatCodaRR
  | PatCodaHR
  | PatDt
  | PatDiphtong
  | PatReplC
  | PatCodaGK
  | PatSZ
  | PatIJ
  | PatTsDsPs
  | PatDiVowel
  | PatReplH
  | PatSmallS
  | PatReplRare
  | PatSpecialCase
  | PatShortSyllable
  | PatCommonPrefix
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
  | PatFrankOther
  | PatForeignOther

  deriving stock (Data, Eq, Generic, Ord, Read, Show)

instance FromJSON Pattern

instance PatternGroup Pattern where
  toDescription = \case
    PatSimple -> "Identical letters"
    PatReplCommon -> "Replacements for common letters"
    PatDiConsonant -> "Double consonants"
    PatCodaH -> "Vowels followed by h"
    PatCodaR -> "Vowels followed by r"
    PatCodaRR -> "Vowels followed by -rr"
    PatCodaHR -> "Vowels followed by -hr"
    PatDt -> "The dt-rule"
    PatDiphtong -> "Multiple vowels"
    PatReplC -> "Different replacements for c"
    PatCodaGK -> "G and k in the coda"
    PatSZ -> "The letter ß"
    PatIJ -> "Using J for i"
    PatTsDsPs -> "S-swapping in the coda"
    PatDiVowel -> "Double vowels"
    PatReplH -> "Replacing a silent h"
    PatSmallS -> "Usage of -s"
    PatReplRare -> "Replacements for less common letters"
    PatSpecialCase -> "Special cases"
    PatShortSyllable -> "Efficiency for short syllables"
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

instance ToHttpApiData Pattern where
  toUrlPiece = Text.pack <<< show

instance FromHttpApiData Pattern where
  parseUrlPiece =
    maybe (Left "failed to read: Pattern") Right <<< readMaybe <<< Text.unpack
