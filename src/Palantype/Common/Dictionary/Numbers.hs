{-|
Description: Common, language-independent dictionary for numbers

For simplicity, the commands are defined using Palantype.DE.
But only the generic indices are exported.
-}

{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}

module Palantype.Common.Dictionary.Numbers
    ( dictNumbers
    , fromIndex
    ) where

import           Data.Function                  ( ($) )
import           Data.Functor                   ( (<$>), (<&>) )
import           Data.Maybe                     ( Maybe(..), catMaybes )
import           Data.Semigroup                 ( (<>) )
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           Palantype.Common.Indices       ( KIChord )
import Palantype.Common.Class
    ( RawSteno(RawSteno), Palantype(toKeys) )
import Palantype.Common.TH (fromJust, failure)
import Data.Char (Char, GeneralCategory (ModifierLetter))
import Data.Ord (Ord((>=)))
import Data.Foldable (Foldable(maximum))
import Palantype.DE (Key(RightFWVIv))
import Control.Applicative (Applicative(pure))
import Data.Tuple (fst)
import Palantype.Common.KeyIndex (KeyIndex)
import GHC.Err (error)
import Data.List ((!!))
import qualified Palantype.Common.Indices as KI
import Control.Monad (unless)

dictNumbers :: [(KIChord, Text)]
dictNumbers = dictModified <> dictUnmodified

-- | combined numbers, a modifier key doesn't make too much sense
--   and requires plover command syntax instead of just a string
dictUnmodified :: [(KIChord, Text)]
dictUnmodified = catMaybes $ do

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
        let fstRight = maximum $ toKeys $ fst $ $fromJust $ Text.uncons rightHand
        in  ( KI.parseChordDE $ RawSteno $ "WN"
              <> ( if fstRight >= RightFWVIv then "-" else "" )
              <> rightHand

            -- number input is glued using {& }, such that plover's space is suppressed
            , "{&" <> strNum <> "}"
            )
  where
    combine Nothing (Just (strNum, chr)) = Just (strNum, Text.singleton chr)
    combine x Nothing = x
    combine (Just (strNums, strSteno)) (Just (strNum, chr)) =
      Just ( strNums <> strNum
           , strSteno <> Text.singleton chr
           )

{-| modifier keys -}
data ModifierPrimary
  = ModPrimCtrl
  | ModPrimWin
  | ModPrimAlt

data ModifierSecondary
  = ModSecNone
  | ModSecShift

-- | single digit numbers, can be modified and then will be
--   treated as plover commands
dictModified :: [(KIChord, Text)]
dictModified = do

    modPrim <- modifiersPrimary
    modSec  <- modifiersSecondary
    (strNum, chrSteno) <- keysThumb
                       <> keysIndex
                       <> keysMiddle
                       <> keysRing
                       <> [keysPinky !! 1] -- 0

    let (chrNum, rem) = $fromJust $ Text.uncons strNum
    unless (Text.null rem) $
        $failure $ "Expected empty string: " <> Text.unpack rem

    let fstRight = maximum $ toKeys chrSteno
    pure ( KI.parseChordDE $ RawSteno $ toStenoStr modPrim modSec
              <> ( if fstRight >= RightFWVIv then "-" else "" )
              <> Text.singleton chrSteno
         , toPloverStr modPrim modSec chrNum
         )
  where
    {-|

    {#control(1)}
    {#alt(2)}
    {#super(3)}

    Note that the output of {#shift(2)} depends on the keyboard layout
    as configured by your system. Palantype.DE.Special contains finger spelling
    for special characters too and thus offers a layout-independent alternative.

    {#shift(4)}

    cf. https://github.com/openstenoproject/plover/wiki/Dictionary-Format
    -}
    toPloverStr :: ModifierPrimary -> ModifierSecondary -> Char -> Text
    toPloverStr modPrim modSec chr =
      let str = Text.singleton chr
          commandStr = case modPrim of
              -- ModShift -> "shift"
              ModPrimCtrl  -> "control"
              ModPrimWin   -> "super"
              ModPrimAlt   -> "alt"
          (shiftStr, closing) = case modSec of
              ModSecNone -> ("", "")
              ModSecShift -> ("shift(", ")")
      in  "{#" <> commandStr <> "(" <> shiftStr <> str <> closing <> ")}"

        -- ModShift -> "Shift_L"

    toStenoStr :: ModifierPrimary -> ModifierSecondary -> Text
    toStenoStr modPrim modSec =
        let stenoPrim = case modPrim of
                ModPrimCtrl  -> "HWN"
                ModPrimWin   -> "DWN"
                ModPrimAlt   -> "FWN"
            stenoSec = case modSec of
                ModSecNone  -> ""
                ModSecShift -> "B"
        in  stenoSec <> stenoPrim

-- | Ctrl, Win, and Alt
modifiersPrimary :: [ModifierPrimary]
modifiersPrimary =
    [ ModPrimCtrl
    , ModPrimWin
    , ModPrimAlt
    ]

-- | None or Shift
modifiersSecondary :: [ModifierSecondary]
modifiersSecondary =
    [ ModSecNone
    , ModSecShift
    ]

keysLeftThumb :: [(Text, Char)]
keysLeftThumb =
  [ ("19", 'Ä')
  , ("20", 'E')
  , ("'" , 'A')
  , ("," , '~')
  ]

keysThumb :: [(Text, Char)]
keysThumb =
  [ ("0", 'U')
  , ("1", 'I')
  , ("2", 'O')
  , ("9", 'Ü')
  ]

keysIndex :: [(Text, Char)]
keysIndex =
  [ ("1", 'M')
  , ("4", 'L')
  , ("7", '+')
  ]

keysMiddle :: [(Text, Char)]
keysMiddle =
  [ ("2", 'B')
  , ("5", 'N')
  , ("8", 'G')
  ]

keysRing :: [(Text, Char)]
keysRing =
  [ ("3", 'ʃ')
  , ("6", 'S')
  , ("9", 'F')
  ]

keysPinky :: [(Text, Char)]
keysPinky =
  [ (".", 's')
  , ("0", 'D')
  , (":", 'n')
  ]

{-|
Map a key index to a character in number mode.
This servers to visualize the number mode on the virtual keyboard.
-}
fromIndex :: KeyIndex -> Maybe Text
fromIndex = \case
    1  -> Nothing
    2  -> Nothing
    3  -> Nothing
    4  -> Just "CTRL"
    5  -> Just "WIN"
    6  -> Just "ALT"
    7  -> Nothing
    8  -> Nothing
    9  -> Just "W"
    10 -> Nothing
    11 -> Just "N"
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
