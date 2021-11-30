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
import           Palantype.Common.RawSteno      ( RawSteno(RawSteno)
                                                , parseChordLenient
                                                )
import qualified Palantype.DE.Keys             as DE

{-|
DE raw steno for back-up command
-}
rawBackUp :: RawSteno
rawBackUp = "ILKSD"

{-|
DE raw steno for capitalization of next word
-}
rawCapNext :: RawSteno
rawCapNext = "BDJNK"

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
    KI.fromChord $ parseChordLenient @DE.Key $ fst $ mkModified str []

kiUp :: KIChord
kiUp = unmodifiedKIChord txtUp

kiDown :: KIChord
kiDown = unmodifiedKIChord txtDown

kiBackUp :: KIChord
kiBackUp = simpleKIChord rawBackUp

kiCapNext :: KIChord
kiCapNext = simpleKIChord rawCapNext

kiEnter :: KIChord
kiEnter = unmodifiedKIChord txtEnter

data Modifier
  = ModShift
  | ModCtrl
  | ModAlt

mkModified :: Text -> [Modifier] -> (RawSteno, Text)
mkModified str [] = case HashMap.lookup str mapENModify of
    Just ploverCode -> (RawSteno $ str <> commandKeys, ploverCode)
    Nothing -> error $ "mkModified: not found in map: " <> Text.unpack str
    where commandKeys = "MKSD"
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
cf. https://github.com/openstenoproject/plover/wiki/Dictionary-Format#capitalizing
-}
mapEN :: [(RawSteno, Text)]
mapEN =
    [ (rawBackUp , "=undo")
    , (rawCapNext, "{-|}")  -- plover: capitalize next word
    ]

commands :: HashMap KIChord Text
commands = HashMap.union m mModified
  where
    m = HashMap.fromList $ first simpleKIChord <$> mapEN
    mModified =
        HashMap.fromList
            $   first unmodifiedKIChord
            <$> HashMap.toList mapENModify
