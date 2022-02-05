{-|
Description: Common dictionary, i.e. language-independent commands

For simplicity, the commands are defined using Palantype.DE.
But only the generic indices are exported.
-}

{-# LANGUAGE TypeApplications #-}

module Palantype.Common.Dictionary
    ( kiUp
    , kiDown
    , kiBackUp
    , kiEnter
    , kiCapNext
    , commands
    , kiAcronym
    ) where

import           Control.Category               ( (<<<) )
import           Data.Bifunctor                 ( Bifunctor(first) )
import           Data.Function                  ( ($) )
import           Data.Functor                   ( (<$>) )
import           Data.HashMap.Strict            ( HashMap )
import qualified Data.HashMap.Strict           as HashMap
import           Data.Maybe                     ( Maybe(..) )
import           Data.Semigroup                 ( (<>) )
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           Data.Tuple                     ( fst )
import           GHC.Err                        ( error )
import           Palantype.Common.Indices       ( KIChord )
import qualified Palantype.Common.Indices      as KI
import           Palantype.Common.RawSteno      ( parseChordLenient
                                                )
import qualified Palantype.DE.Keys             as DE
import Palantype.Common.Class (RawSteno (RawSteno))

{-|
DE raw steno for back-up command, i.e. undo last input
-}
rawBackUp :: RawSteno
rawBackUp = "ILNSD"

{-|
DE raw steno for capitalization of next word
-}
rawCapNext :: RawSteno
rawCapNext = "BDJNN"

simpleKIChord :: RawSteno -> KIChord
simpleKIChord = KI.fromChord <<< parseChordLenient @DE.Key

txtUp :: Text
txtUp = "D"

txtDown :: Text
txtDown = "J"

txtEnter :: Text
txtEnter = "A"

unmodifiedKIChord :: Text -> KIChord
unmodifiedKIChord str =
    simpleKIChord $ fst $ mkModified str []

{-|
arrow key: up
-}
kiUp :: KIChord
kiUp = unmodifiedKIChord txtUp

{-|
arrow key: down
-}
kiDown :: KIChord
kiDown = unmodifiedKIChord txtDown

{-|
back-up, i.e. undo last input
-}
kiBackUp :: KIChord
kiBackUp = simpleKIChord rawBackUp

{-|
capitalize next chord
-}
kiCapNext :: KIChord
kiCapNext = simpleKIChord rawCapNext

{-|
enter key, not to be confused with paragraph
-}
kiEnter :: KIChord
kiEnter = unmodifiedKIChord txtEnter

{-|
qualifier for acronyms
-}
kiAcronym :: KIChord
kiAcronym = simpleKIChord "NÜM"

data Modifier
  = ModShift
  | ModCtrl
  | ModAlt

mkModified :: Text -> [Modifier] -> (RawSteno, Text)
mkModified str [] = case HashMap.lookup str mapENModify of
    Just ploverCode -> (RawSteno $ str <> commandKeys, ploverCode)
    Nothing -> error $ "mkModified: not found in map: " <> Text.unpack str
    where commandKeys = "-MNSD"
mkModified _ _ = error "mkModified: not implemented"

{-|
Common commands that can be modified with Shift, Control, Alt.
TODO: how to combine CTRL, SHIFT, ALT if applicable? Maybe using left thumb?
-}
mapENModify :: HashMap Text Text
mapENModify = HashMap.fromList
    [ (txtUp   , "{#up}")
    , (txtDown , "{#down}")
    , ("B"     , "{#left}")
    , ("N"     , "{#right}")
    , (txtEnter, "{#return}")
    , ("G"     , "{#tab}")
    , ("S"     , "{#home}")
    , ("L"     , "{#end}")
    , ("H"     , "{#pageup}")
    , ("M"     , "{#pagedown}")
    ]

{-|
more commands that are steno specific and cannot be modified by CTRL, SHIFT, ALT
cf. https://github.com/openstenoproject/plover/wiki/Dictionary-Format#capitalizing
-}
mapEN :: [(RawSteno, Text)]
mapEN =
    [ (rawBackUp , "=undo")
    , (rawCapNext, "{-|}")  -- plover: capitalize next word
    , ("BDJNLNSD", "{PLOVER:TOGGLE}")
    ]

commands :: HashMap KIChord Text
commands = HashMap.union m mModified
  where
    m = HashMap.fromList $ first simpleKIChord <$> mapEN
    mModified =
        HashMap.fromList
            $   first unmodifiedKIChord
            <$> HashMap.toList mapENModify
