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
    , fromIndex
    , strModeSteno
    ) where

import           Data.Function                  ( ($) )
import           Data.Functor                   ( (<$>), (<&>) )
import           Data.Maybe                     ( Maybe(..), catMaybes )
import           Data.Semigroup                 ( (<>) )
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           Palantype.Common.Indices       ( KIChord, parseChordDE )
import Palantype.Common.Class
    ( RawSteno(RawSteno) )
import Palantype.Common.TH (fromJust, failure)
import Control.Applicative (Applicative(pure))
import Data.Tuple (fst, snd)
import Palantype.Common.KeyIndex (KeyIndex)
import GHC.Err (error)
import Data.List ((!!))
import Control.Monad (unless)
import Data.Char (Char)
import Palantype.Common.Dictionary.Shared
    ( ModifierPrimary(..), ModifierSecondary(..), toStenoStrRightHand, toPloverLiteralGlued, toPloverCommand )
import qualified Data.Map.Strict as Map
import Text.Show (Show(show))
import Data.Bifunctor (Bifunctor(first))

-- | mode selection for numbers mode: WN-
--   the string is combined and the - is added on demand
strModeSteno :: Text
strModeSteno = "WN"

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
        ( $parseChordDE $ RawSteno $
              toStenoStrRightHand strModeSteno ModPrimNone ModSecNone rightHand
        -- number input is glued using {& }, such that plover's space is suppressed
        --, "{&" <> strNum <> "}"
        , toPloverLiteralGlued strNum
        )
  where
    combine Nothing (Just (strNum, chr)) = Just (strNum, Text.singleton chr)
    combine x Nothing = x
    combine (Just (strNums, strSteno)) (Just (strNum, chr)) =
      Just ( strNums <> strNum
           , strSteno <> Text.singleton chr
           )

-- | Shift + some number key on US keyboard layout gives one of the
--   `shiftedNumberUSSpecialChars`. They are treated as literals, not
--   commands, because literals give the user more flexibility
shiftedNumberUSSpecialChars :: [(KIChord, Text)]
shiftedNumberUSSpecialChars = do
    (literal, steno) <- first shiftNumberUS
        <$>    keysThumb
            <> keysIndex
            <> keysMiddle
            <> keysRing
            <> [keysPinky !! 1] -- 0
    pure
        ( $parseChordDE $ RawSteno $
              toStenoStrRightHand strModeSteno
                                  ModPrimNone
                                  ModSecShift
                                  $ Text.singleton steno
        , toPloverLiteralGlued literal
        )

-- | single digit numbers, can be modified and then will be
--   treated as plover commands
numberCommands :: [(KIChord, Text)]
numberCommands = do
    modPrim <- [ModPrimAlt, ModPrimCtrl, ModPrimWin]
    modSec  <- [ModSecNone, ModSecShift]
    (literal, steno) <- keysThumb
                     <> keysIndex
                     <> keysMiddle
                     <> keysRing
                     <> [keysPinky !! 1] -- 0

    let rem = snd $ $fromJust $ Text.uncons literal
    unless (Text.null rem) $
        $failure $ "Expected empty string: " <> Text.unpack rem

    pure ( $parseChordDE $ RawSteno $
               toStenoStrRightHand strModeSteno
                                   modPrim
                                   modSec
                                   $ Text.singleton steno
         , toPloverCommand modPrim modSec literal
         )

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
    2  -> Just "SHIFT"
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

shiftNumberUS :: Text -> Text
shiftNumberUS chr =
    Map.findWithDefault ($failure $ "Unknown character: " <> show chr)
                        chr
                        mapChars
  where
    mapChars = Map.fromList
        [ ("1", "!")
        , ("2", "@")
        , ("3", "#")
        , ("4", "$")
        , ("5", "%")
        , ("6", "^")
        , ("7", "&")
        , ("8", "*")
        , ("9", "(")
        , ("0", ")")
        ]
