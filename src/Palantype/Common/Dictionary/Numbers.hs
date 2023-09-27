{-|
Description: Common, language-independent dictionary for numbers

Use the left hand to select the numbers mode (cf. `strModeSteno`)
and use the right hand to type.

For simplicity, the commands are defined using Palantype.DE.
But only the generic indices are exported.
-}

{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}

module Palantype.Common.Dictionary.Numbers
    ( dictNumbers
    , dictNumberLiterals
    , fromIndex
    , strModeSteno
    , kiCtrlNumber
    , kiChordToInt
    , kiFromSmallNumber
    ) where

import Control.Category ((.))
import Data.Bifunctor (first, second, bimap)
import Data.Ord (Ord(..))
import Data.Bool ((&&))
import           Data.Function                  ( ($) )
import           Data.Functor                   ( (<$>), (<&>), Functor (fmap) )
import           Data.Maybe                     ( Maybe(..), catMaybes )
import           Data.Semigroup                 ( (<>) )
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           Palantype.Common.Indices       ( KIChord (..), parseChordDE )
import qualified Palantype.Common.Indices      as KI
import qualified Palantype.DE.Keys as DE ( Key )
import Palantype.Common.TH (fromJust)
import Control.Applicative (Applicative(pure))
import Data.Tuple (fst, snd)
import Palantype.Common.KeyIndex (KeyIndex, keyIndex)
import GHC.Err (error)
import Data.List ((!!), drop)
import Data.Char (Char)
import Palantype.Common.Dictionary.Shared
    ( ModifierPrimary(..), ModifierSecondary(..), toStenoStrRightHand, toPloverLiteralGlued, toPloverCommand )
import qualified Palantype.Common.RawSteno as Raw
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Int (Int)
import Data.Foldable (foldl')
import Text.Read (readMaybe)
import GHC.Real (div, mod)
import qualified Data.List.NonEmpty as NonEmpty
import Palantype.Common.Class (toKeys)
import TextShow (TextShow (showt))

-- | mode selection for numbers mode
--   the string is combined and the - is added on demand
strModeSteno :: Text
strModeSteno = "NM" -- key indices 8 and 10

dictNumbers :: [(KIChord, Text)]
dictNumbers =
       unmodifiedNumberStrs
    <> shiftedNumberUSSpecialChars
    <> numberCommands

-- | combined numbers, a modifier key for more than a single digit doesn't
--   make too much sense and would require plover command syntax instead of
--   just a string
unmodifiedNumberStrs :: [(KIChord, Text)]
unmodifiedNumberStrs = catMaybes $ do

    mLT <- Nothing : (Just <$> keysLeftThumb )
    mRT <- Nothing : (Just . first (first Text.singleton) <$> keysThumb )
    mI  <- Nothing : (Just . first (first Text.singleton) <$> keysIndex )
    mM  <- Nothing : (Just . first (first Text.singleton) <$> keysMiddle)
    mR  <- Nothing : (Just . first (first Text.singleton) <$> keysRing  )
    mP  <- Nothing : (Just . first (first Text.singleton) <$> keysPinky )

    let mEntry =
          Nothing `combine` mLT
                  `combine` mRT
                  `combine` mI
                  `combine` mM
                  `combine` mR
                  `combine` mP

    pure $ mEntry <&> \(strNum, rightHand) ->
        ( $parseChordDE $ Raw.fromText $
              toStenoStrRightHand strModeSteno ModPrimNone ModSecNone rightHand
        -- number input is glued using {& }, such that plover's space is suppressed
        --, "{&" <> strNum <> "}"
        , toPloverLiteralGlued strNum
        )
  where
    combine :: Maybe (Text, Text) -> Maybe ((Text, Maybe Char), Char) -> Maybe (Text, Text)
    combine Nothing (Just ((strNum, _), chr)) = Just (strNum, Text.singleton chr)
    combine x Nothing = x
    combine (Just (strs, strSteno)) (Just ((strNum, _), chr)) =
      Just ( strs <> strNum
           , strSteno <> Text.singleton chr
           )

-- | Shift + some number key on US keyboard layout gives one of the
--   `shiftedNumberUSSpecialChars`. They are treated as literals, not
--   commands, because literals give the user more flexibility
shiftedNumberUSSpecialChars :: [(KIChord, Text)]
shiftedNumberUSSpecialChars = do
    ((_, mLiteral), steno) <-
           keysThumb
        <> keysIndex
        <> keysMiddle
        <> keysRing
        <> [keysPinky !! 1] -- 0
    pure
        ( $parseChordDE $ Raw.fromText $
              toStenoStrRightHand strModeSteno
                                  ModPrimNone
                                  ModSecShift
                                  $ Text.singleton steno
        , toPloverLiteralGlued $ Text.singleton $ $fromJust mLiteral
        )

-- | single digit numbers, can be modified and then will be
--   treated as plover commands
numberCommands :: [(KIChord, Text)]
numberCommands = do
    modPrim <- [ModPrimAlt, ModPrimCtrl, ModPrimWin]
    modSec  <- [ModSecNone, ModSecShift]
    ((literal, _), steno) <- keysThumb
                          <> keysIndex
                          <> keysMiddle
                          <> keysRing
                          <> [keysPinky !! 1] -- 0

    pure ( $parseChordDE $ Raw.fromText $
               toStenoStrRightHand strModeSteno
                                   modPrim
                                   modSec
                                   $ Text.singleton steno
         , toPloverCommand modPrim modSec $ Text.singleton literal
         )

-- | literals, for use in exercise learn-palantype, no plover syntax
dictNumberLiterals :: [(KIChord, Char)]
dictNumberLiterals = leftThumb <> do
    modSec <- [ModSecNone, ModSecShift]
    ((literalNumber, mLiteralShifted), steno) <-
           keysThumb
        <> keysIndex
        <> keysMiddle
        <> keysRing
        <> keysPinky
    pure
        ( $parseChordDE $ Raw.fromText $
              toStenoStrRightHand strModeSteno
                                  ModPrimNone
                                  modSec
                                  $ Text.singleton steno
        , case modSec of
            ModSecNone  -> literalNumber
            ModSecShift -> $fromJust mLiteralShifted
        )
  where
    leftThumb = do
      ((literal, _), steno) <- drop 2 keysLeftThumb
      pure
        ( $parseChordDE $ Raw.fromText
            $ toStenoStrRightHand strModeSteno ModPrimNone ModSecNone
              $ Text.singleton steno
        , Text.head literal
        )

kiFromSmallNumber :: Int -> Maybe KIChord
kiFromSmallNumber i | i >= 0 && i < 10 = Just $
    -- KI.fromChord $ $fromJust $ Raw.parseChordMaybe @DE.Key $ Raw.fromText
    --   $ Text.singleton $ $fromJust $ Map.lookup i mapSingleDigitCodes
    KIChord $ fmap keyIndex $ NonEmpty.tail $ $fromJust $ toKeys @DE.Key $ $fromJust $ Map.lookup i mapSingleDigitCodes
kiFromSmallNumber i | i >= 10 && i < 30 = Just $
    let
        firstDigit = snd $ keysThumb !! (i `div` 10)
        secondDigit = $fromJust $ Map.lookup (i `mod` 10) mapSingleDigitCodes
    in  KI.fromChord $ $fromJust $ Raw.parseChordMaybe @DE.Key
          $ Raw.fromText $ firstDigit `Text.cons` Text.singleton secondDigit
kiFromSmallNumber _ = Nothing

mapSingleDigitCodes :: Map Int Char
mapSingleDigitCodes =
  let
      accFunc m ((literal, _), steno) =
        case readMaybe $ pure literal of
          Just d  -> Map.insert d steno m
          Nothing -> m
  in  foldl' accFunc Map.empty
        $ keysIndex <> keysMiddle <> keysRing <> keysPinky

kiChordToInt :: KIChord -> Maybe Int
kiChordToInt kiChord = do
    str <- Text.stripPrefix "ʃB+" (showt $ KI.toRaw @DE.Key kiChord)
    numStr <- foldl' accFunc (Just "") $ Text.unpack str
    readMaybe $ Text.unpack numStr
  where
    accFunc Nothing    _   = Nothing
    accFunc (Just str) '-' = Just str
    accFunc (Just str) c   = (str <>) <$> Map.lookup c mapCharDigit

mapCharDigit :: Map Char Text
mapCharDigit =
    foldl' accFunc Map.empty
      $  keysLeftThumb
      <> fmap (first $ first Text.singleton)
          ( keysThumb
          <> keysIndex
          <> keysMiddle
          <> keysRing
          <> keysPinky
          )
  where
    accFunc m ((str, _), c) = Map.insert c str m

keysLeftThumb :: [((Text, Maybe Char), Char)]
keysLeftThumb =
  [ (("19", Nothing), 'Ä')
  , (("20", Nothing), 'E')
  , (("'" , Nothing), 'A')
  , ((":" , Nothing), '~')
  ]

keysThumb :: [((Char, Maybe Char), Char)]
keysThumb =
  [ (('0', Just ')'), 'U')
  , (('1', Just '!'), 'I')
  , (('2', Just '@'), 'O')
  , (('9', Just '('), 'Ü')
  ]

keysIndex :: [((Char, Maybe Char), Char)]
keysIndex =
  [ (('1', Just '!'), 'L')
  , (('4', Just '$'), '+')
  , (('7', Just '&'), 'M')
  ]

keysMiddle :: [((Char, Maybe Char), Char)]
keysMiddle =
  [ (('2', Just '@'), 'B')
  , (('5', Just '%'), 'N')
  , (('8', Just '*'), 'G')
  ]

keysRing :: [((Char, Maybe Char), Char)]
keysRing =
  [ (('3', Just '#'), 'F')
  , (('6', Just '^'), 'S')
  , (('9', Just '('), 'ʃ')
  ]

keysPinky :: [((Char, Maybe Char), Char)]
keysPinky =
  [ (('.', Just '>'), 's')
  , (('0', Just ')'), 'D')
  , ((',', Just '<'), 'n')
  ]

{-|
Map a key index to a character in number mode.
For visualization of the number mode on the virtual keyboard.
-}
fromIndex :: KeyIndex -> Maybe (Text, Maybe Text)
fromIndex = \case
    1  -> Nothing
    2  -> Just ("SHIFT", Just "SHIFT")
    3  -> Nothing
    4  -> Just ("CTRL", Just "CTRL")
    5  -> Just ("WIN", Just "WIN")
    6  -> Just ("ALT", Just "ALT")
    7  -> Nothing
    8  -> Just ("N", Just "N")
    9  -> Nothing
    10 -> Just ("M", Just "M")
    11 -> Nothing
    12 -> Nothing
    13 -> Just $ second (fmap Text.singleton) $ fst $ keysLeftThumb !! 0
    14 -> Just $ second (fmap Text.singleton) $ fst $ keysLeftThumb !! 1
    15 -> Just $ second (fmap Text.singleton) $ fst $ keysLeftThumb !! 2
    16 -> Just $ second (fmap Text.singleton) $ fst $ keysLeftThumb !! 3
    17 -> Just $ bimap Text.singleton (fmap Text.singleton) (fst $ keysThumb !! 0)
    18 -> Just $ bimap Text.singleton (fmap Text.singleton) (fst $ keysThumb !! 1)
    19 -> Just $ bimap Text.singleton (fmap Text.singleton) (fst $ keysThumb !! 2)
    20 -> Just $ bimap Text.singleton (fmap Text.singleton) (fst $ keysThumb !! 3)
    21 -> Just $ bimap Text.singleton (fmap Text.singleton) (fst $ keysIndex !! 2)
    22 -> Just $ bimap Text.singleton (fmap Text.singleton) (fst $ keysIndex !! 1)
    23 -> Just $ bimap Text.singleton (fmap Text.singleton) (fst $ keysIndex !! 0)
    24 -> Just $ bimap Text.singleton (fmap Text.singleton) (fst $ keysMiddle !! 2)
    25 -> Just $ bimap Text.singleton (fmap Text.singleton) (fst $ keysMiddle !! 1)
    26 -> Just $ bimap Text.singleton (fmap Text.singleton) (fst $ keysMiddle !! 0)
    27 -> Just $ bimap Text.singleton (fmap Text.singleton) (fst $ keysRing !! 2)
    28 -> Just $ bimap Text.singleton (fmap Text.singleton) (fst $ keysRing !! 1)
    29 -> Just $ bimap Text.singleton (fmap Text.singleton) (fst $ keysRing !! 0)
    30 -> Just $ bimap Text.singleton (fmap Text.singleton) (fst $ keysPinky !! 2)
    31 -> Just $ bimap Text.singleton (fmap Text.singleton) (fst $ keysPinky !! 1)
    32 -> Just $ bimap Text.singleton (fmap Text.singleton) (fst $ keysPinky !! 0)
    _  -> error "Numbers.fromIndex: impossible"

kiCtrlNumber :: KIChord
kiCtrlNumber = $parseChordDE $ Raw.fromText "ʃB+-"
