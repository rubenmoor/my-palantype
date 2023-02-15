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
import Data.Foldable (Foldable(elem))

-- the palantype.de keyboard
--
-- v ʃ G M         M G ʃ n
-- D S N +         + N S D
-- b F B L         L B F s
--     Ä E A ~ U I O Ü

-- a key on a steno keyboard
data Key
  = LeftVSmall
  | LeftD
  | LeftBSmall
  | LeftSch
  | LeftS
  | LeftF
  | LeftG
  | LeftN
  | LeftB
  | LeftM
  | LeftPlus
  | LeftL
  | LeftAUmlaut
  | LeftE
  | LeftA
  | LeftStretch
  | RightU
  | RightI
  | RightOStretch
  | RightUUmlaut
  | RightM
  | RightPlus
  | RightL
  | RightG
  | RightN
  | RightB
  | RightSch
  | RightS
  | RightF
  | RightNSmall
  | RightD
  | RightSSmall
  deriving stock (Eq, Ord, Data, Generic)

instance Palantype Key where
    type PatternGroup Key = Pattern

    keyCode = \case
        LeftVSmall    -> 'v'
        LeftD         -> 'D'
        LeftBSmall    -> 'b'
        LeftSch       -> 'ʃ'
        LeftS         -> 'S'
        LeftF         -> 'F'
        LeftG         -> 'G'
        LeftN         -> 'N'
        LeftB         -> 'B'
        LeftM         -> 'M'
        LeftPlus      -> '+'
        LeftL         -> 'L'
        LeftAUmlaut   -> 'Ä'
        LeftE         -> 'E'
        LeftA         -> 'A'
        LeftStretch   -> '~'
        RightU        -> 'U'
        RightI        -> 'I'
        RightOStretch -> 'O'
        RightUUmlaut  -> 'Ü'
        RightM        -> 'M'
        RightPlus     -> '+'
        RightL        -> 'L'
        RightG        -> 'G'
        RightN        -> 'N'
        RightB        -> 'B'
        RightSch      -> 'ʃ' -- U+0283
        RightS        -> 'S'
        RightF        -> 'F'
        RightNSmall   -> 'n'
        RightD        -> 'D'
        RightSSmall   -> 's'

    patZero = PatSimple
    patSimpleMulti = PatSimpleMulti
    patCapitalize = PatCapitalize
    patAcronym = PatAcronym

    toDescription = \case
      PatSimple -> "Identical letters"
      PatSimpleMulti -> "Identical letters, multiple chords"
      PatReplCommon1 -> "Replacements for common letters 1/2"
      PatReplCommon2 -> "Replacements for common letters 2/2"
      PatCodaComboT -> "Fitting a consonant in between +D"
      PatOnsetR -> "R in the onset"
      PatOnsetL -> "Exceptions for L in the onset"
      PatDiConsonant -> "Double consonants"
      PatCodaH -> "Long vowels"
      PatCodaR -> "Vowels followed by r"
      PatCodaRR -> "Vowels followed by -rr"
      PatCodaHR -> "Vowels followed by -hr"
      PatDt -> "The dt-rule"
      PatDiphtong -> "Multiple vowels"
      PatReplC -> "Different replacements for c"
      PatBreakUpI -> "Break up ia and io"
      PatSwapS -> "S-swapping in the coda"
      PatSwapSch -> "Sch-swapping in the coda"
      PatSwapZ -> "Z-swapping in the coda"
      PatDiVowel -> "Double vowels"

      -- TODO: extra rule for th, both for coda and onset
      -- TODO: common replacement for h in onset
      PatReplH -> "Replacing a silent h"

      PatCodaGK -> "The stretching of a in ak"

      -- TODO: rework
      PatReplRare -> "Replacements for less common letters"
      PatSmallS -> "Making use of the (small) s key"
      PatSCStretch -> "Irregular stretch key"
      PatSCPlus -> "Irregular plus key"
      PatSCOther -> "Unspecific irregularities"
      PatCapitalize -> "Explicit capitalization"
      PatShortSyllable -> "Efficiency for short syllables"
      PatBrief -> "Brief for a common word"
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

      PatAcronym -> "Acronym spellings"
      PatChemistry -> "Symbols of chemical elements"

    lsPrimitives =
        let str = stripComments $(embedFile "DE/primitives.json5")
        in  Map.toList $ unPrimMap $ case Aeson.eitherDecodeStrict str of
              Right m   -> m :: PrimMap Key
              Left  err -> error $ "Could not decode DE/primitives.json5: " <> err

    mapExceptions =
        let str = stripComments $(embedFile "DE/exceptions.json5")
        in  unExceptionsMap $ case Aeson.eitherDecodeStrict str of
                Right map -> map :: ExceptionsMap Key
                Left  err -> error $ "Could not decode exceptions.json5: " <> err

instance TextShow Key where
    showb k =
        let ambiguousLeft =
                [ LeftD
                , LeftSch
                , LeftS
                , LeftF
                , LeftG
                , LeftN
                , LeftB
                , LeftM
                , LeftPlus
                , LeftL
                ]
            ambiguousRight =
                [ RightM
                , RightPlus
                , RightL
                , RightG
                , RightN
                , RightB
                , RightSch
                , RightS
                , RightF
                , RightD
                ]
            prefix = if k `elem` ambiguousRight then "-" else ""
            suffix = if k `elem` ambiguousLeft  then "-" else ""
        in  prefix <> singleton (keyCode k) <> suffix

data Pattern
  -- simple pattern
  = PatSimple
  | PatSimpleMulti
  -- common replacement rules
  | PatReplCommon1
  | PatReplCommon2
  | PatCodaComboT
  | PatOnsetR
  | PatOnsetL
  | PatDiConsonant
  | PatCodaH
  | PatCodaR
  | PatCodaRR
  | PatCodaHR
  | PatDt
  | PatDiphtong
  | PatReplC
  | PatBreakUpI
  | PatSwapS
  | PatSwapSch
  | PatSwapZ
  | PatDiVowel
  | PatCodaGK
  | PatReplH
  -- rare replacement rules and special cases
  | PatReplRare
  | PatSmallS
  | PatSCStretch
  | PatSCPlus
  | PatSCOther
  -- rules for capitalization
  | PatCapitalize
  -- rules for increased efficiency
  | PatShortSyllable
  | PatBrief
  | PatCommonPrefix
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
  -- acronyms
  | PatAcronym
  | PatChemistry

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
