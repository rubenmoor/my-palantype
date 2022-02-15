{-|
Description: Common, language-independent dictionary for numbers

For simplicity, the commands are defined using Palantype.DE.
But only the generic indices are exported.
-}

{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}

module Palantype.Common.Numbers
    ( dictNumbers
    ) where

import           Control.Category               ( (<<<) )
import           Data.Function                  ( ($) )
import           Data.Functor                   ( (<$>), (<&>) )
import           Data.Maybe                     ( Maybe(..), catMaybes )
import           Data.Semigroup                 ( (<>) )
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           Palantype.Common.Indices       ( KIChord )
import qualified Palantype.Common.Indices      as KI
import           Palantype.Common.RawSteno      ( parseChordMaybe
                                                )
import qualified Palantype.DE.Keys             as DE
import Palantype.Common.Class
    ( RawSteno(RawSteno), Palantype(toKeys) )
import Palantype.Common.TH (fromJust)
import Data.Char (Char)
import Data.Ord (Ord((>=)))
import Data.Foldable (Foldable(maximum))
import Palantype.DE (Key(RightFWVIv))
import Control.Applicative (Applicative(pure))

{-|
DE raw steno for the Modifier steno code, i.e.

type this with your left hand and the number code with
your right hand to reach a number
-}
mkKIChord :: RawSteno -> KIChord
mkKIChord = KI.fromChord <<< $fromJust <<< parseChordMaybe @DE.Key

{-| modifier keys, shift isn't really a thing for number keys -}
data Modifier
  = ModNone
  | ModCtrl
  | ModWin
  | ModAlt

dictNumbers :: [(KIChord, Text)]
dictNumbers = catMaybes $ do

    mod <- modifiers
    mT  <- Nothing : (Just <$> numbersThumb )
    mI  <- Nothing : (Just <$> numbersIndex )
    mM  <- Nothing : (Just <$> numbersMiddle)
    mR  <- Nothing : (Just <$> numbersRing  )
    mP  <- Nothing : (Just <$> numbersPinky )

    let mEntry =
          Nothing `combine` mT
                  `combine` mI
                  `combine` mM
                  `combine` mR
                  `combine` mP

    pure $ mEntry <&> \(strNum, rightHand) ->
        ( mkKIChord $ RawSteno $ toStenoStr mod
          <> if maximum (toKeys $ Text.head rightHand) >= RightFWVIv
             then "-"
             else ""
          <> rightHand
        , toPloverStr mod strNum
        )
  where
    combine Nothing (Just (strNum, chr)) = Just (strNum, Text.singleton chr)
    combine x Nothing = x
    combine (Just (strNums, strSteno)) (Just (strNum, chr)) =
      Just (strNums <> strNum
           , strSteno
             <> if maximum (toKeys chr) >= RightFWVIv
                then "-"
                else ""
             <> Text.singleton chr
           )

{-|

{#Control_L(0)}
{#Alt_L(0)}
{#Super_L(0)}

cf. https://github.com/openstenoproject/plover/wiki/Dictionary-Format
-}
toPloverStr :: Modifier -> Text -> Text
toPloverStr mod str = case mod of
    ModNone  -> str
    ModCtrl  -> "{#control(" <> str <> ")}"
    ModWin   -> "{#super("   <> str <> ")}"
    ModAlt   -> "{#alt("     <> str <> ")}"
    -- ModShift -> "Shift_L"

toStenoStr :: Modifier -> Text

toStenoStr = \case
    ModNone  -> "WN"
    ModCtrl  -> "HWN"
    ModWin   -> "DWN"
    ModAlt   -> "FWN"
    -- ModShift -> "Shift_L"


modifiers :: [Modifier]
modifiers =
    [ ModNone
    , ModCtrl
    , ModWin
    , ModAlt
    ]

numbersThumb :: [(Text, Char)]
numbersThumb =
  [ ("0"  , 'U')
  , ("1"  , 'I')
  , ("2"  , 'O')
  , ("9"  , 'Ü')
  ]

numbersIndex :: [(Text, Char)]
numbersIndex =
  [ ("1"  , 'M')
  , ("4"  , 'L')
  , ("7"  , '+')
  ]

numbersMiddle :: [(Text, Char)]
numbersMiddle =
  [ ("2"  , 'B')
  , ("5"  , 'N')
  , ("8"  , 'G')
  ]

numbersRing :: [(Text, Char)]
numbersRing =
  [ ("3"  , 'ʃ')
  , ("6"  , 'S')
  , ("9"  , 'F')
  ]

numbersPinky :: [(Text, Char)]
numbersPinky =
  [ ("0"  , 's')
  , ("00" , 'D')
  , ("000", 'n')
  ]
