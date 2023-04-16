{-|
Description: Common, language-independent dictionary for numbers

Use the left hand to select the numbers mode (cf. `strModeSteno`)
and use the right hand to type.

For simplicity, the commands are defined using Palantype.DE.
But only the generic indices are exported.
-}

{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}

module Palantype.Common.Dictionary.Numbers
    ( dictNumbers
    , fromIndex
    , strModeSteno
    , kiCtrlNumber
    , kiChordToInt
    , kiFromSmallNumber
    ) where

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
import Palantype.Common.TH (fromJust, failure)
import Control.Applicative (Applicative(pure))
import Data.Tuple (fst, snd)
import Palantype.Common.KeyIndex (KeyIndex, keyIndex)
import GHC.Err (error)
import Data.List ((!!))
import Control.Monad (unless)
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
strModeSteno = "B+" -- key indices 9 and 11

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
    mRT <- Nothing : (Just <$> keysThumb )
    mI  <- Nothing : (Just <$> keysIndex )
    mM  <- Nothing : (Just <$> keysMiddle)
    mR  <- Nothing : (Just <$> keysRing  )
    mP  <- Nothing : (Just <$> keysPinky )

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
    combine :: Maybe (Text, Text) -> Maybe ((Text, Maybe Text), Char) -> Maybe (Text, Text)
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
        , toPloverLiteralGlued $ $fromJust mLiteral
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

    let rem = snd $ $fromJust $ Text.uncons literal
    unless (Text.null rem) $
        $failure $ "Expected empty string: " <> Text.unpack rem

    pure ( $parseChordDE $ Raw.fromText $
               toStenoStrRightHand strModeSteno
                                   modPrim
                                   modSec
                                   $ Text.singleton steno
         , toPloverCommand modPrim modSec literal
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
        case readMaybe $ Text.unpack literal of
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
      <> keysThumb
      <> keysIndex
      <> keysMiddle
      <> keysRing
      <> keysPinky
  where
    accFunc m ((str, _), c) = Map.insert c str m

keysLeftThumb :: [((Text, Maybe Text), Char)]
keysLeftThumb =
  [ (("19", Nothing), 'Ä')
  , (("20", Nothing), 'E')
  , (("'" , Nothing), 'A')
  , (("," , Nothing), '~')
  ]

keysThumb :: [((Text, Maybe Text), Char)]
keysThumb =
  [ (("0", Just ")"), 'U')
  , (("1", Just "!"), 'I')
  , (("2", Just "@"), 'O')
  , (("9", Just "("), 'Ü')
  ]

keysIndex :: [((Text, Maybe Text), Char)]
keysIndex =
  [ (("1", Just "!"), 'L')
  , (("4", Just "$"), '+')
  , (("7", Just "&"), 'M')
  ]

keysMiddle :: [((Text, Maybe Text), Char)]
keysMiddle =
  [ (("2", Just "@"), 'B')
  , (("5", Just "%"), 'N')
  , (("8", Just "*"), 'G')
  ]

keysRing :: [((Text, Maybe Text), Char)]
keysRing =
  [ (("3", Just "#"), 'F')
  , (("6", Just "^"), 'S')
  , (("9", Just "("), 'ʃ')
  ]

keysPinky :: [((Text, Maybe Text), Char)]
keysPinky =
  [ ((".", Nothing ), 's')
  , (("0", Just ")"), 'D')
  , ((":", Nothing ), 'n')
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
    8  -> Nothing
    9  -> Just ("B", Just "B")
    10 -> Nothing
    11 -> Just ("+", Just "+")
    12 -> Nothing
    13 -> Just $ fst $ keysLeftThumb !! 0
    14 -> Just $ fst $ keysLeftThumb !! 1
    15 -> Just $ fst $ keysLeftThumb !! 2
    16 -> Just $ fst $ keysLeftThumb !! 3
    17 -> Just $ fst $ keysThumb !! 0
    18 -> Just $ fst $ keysThumb !! 1
    19 -> Just $ fst $ keysThumb !! 2
    20 -> Just $ fst $ keysThumb !! 3
    21 -> Just $ fst $ keysIndex !! 2
    22 -> Just $ fst $ keysIndex !! 1
    23 -> Just $ fst $ keysIndex !! 0
    24 -> Just $ fst $ keysMiddle !! 2
    25 -> Just $ fst $ keysMiddle !! 1
    26 -> Just $ fst $ keysMiddle !! 0
    27 -> Just $ fst $ keysRing !! 2
    28 -> Just $ fst $ keysRing !! 1
    29 -> Just $ fst $ keysRing !! 0
    30 -> Just $ fst $ keysPinky !! 2
    31 -> Just $ fst $ keysPinky !! 1
    32 -> Just $ fst $ keysPinky !! 0
    _  -> error "Numbers.fromIndex: impossible"

kiCtrlNumber :: KIChord
kiCtrlNumber = $parseChordDE $ Raw.fromText "ʃB+-"
