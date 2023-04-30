{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Palantype.Common.Stage
    ( fromIndex
    , findStage
    , getSystemLang
    , toTOCString
    , toPageName
    , getStageIndexMaybe
    , getGroupIndex
    , Stage(..)
    , StageRepr()
    , StageSpecialGeneric(..)
    , StageHierarchy(..)
    , mkStageIndex
    , mPrev
    , mNext
    , isValidIndex
    , findStageIndex
    , toStageRepr
    , showShort
    , mapStages
    , mapHierarchyStageIndex
    , stages
    , stageIndexPatZero
    , stageIndexPatSimpleMulti
    , stageIndexPatCapitalize
    , stageIndexPatAcronym
    , StageIndex
    ) where

import           Control.Applicative            ( Applicative(pure, (<*)) )
import           Control.Category               ( (<<<)
                                                , Category((.))
                                                )
import Control.Lens (view)
import Control.Lens.Tuple (_1)
import           Control.Lens.Wrapped           ( Wrapped )
import           Control.Monad                  ( Monad((>>=)) )
import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                )
import           Data.Bool                      ( (&&)
                                                , Bool
                                                , otherwise
                                                , (||)
                                                )
import           Data.Char                      ( isDigit
                                                , isLetter
                                                , toUpper, isAlphaNum, isAscii
                                                )
import           Data.Default                   ( Default(def) )
import           Data.Eq                        ( Eq((/=), (==)) )
import           Data.Foldable                  ( Foldable(foldl', length), elem )
import           Data.Function                  ( ($) )
import           Data.Functor                   ( (<$>)
                                                , (<&>)
                                                )
import           Data.Int                       ( Int )
import           Data.List                      ( head
                                                , zip
                                                )
import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as Map
import           Data.Maybe                     ( Maybe(Just, Nothing)
                                                , maybe, fromMaybe
                                                )
import           Data.Monoid                    ( (<>) )
import           Data.Ord                       ( (<)
                                                , (>)
                                                , Ord((>=))
                                                )
import           Data.Text                      ( Text
                                                , cons
                                                , uncons
                                                )
import qualified Data.Text                     as Text
import           Data.Tuple                     ( snd )
import           GHC.Enum                       ( Enum
                                                , pred
                                                , succ
                                                )
import           GHC.Generics                   ( Generic )
import           GHC.Num                        ( (+)
                                                , (-)
                                                , Num
                                                )
import           Palantype.Common.Class         ( Palantype
                                                    ( patAcronym
                                                    , patCapitalize
                                                    , patSimpleMulti
                                                    , patZero
                                                    , toDescription
                                                    )
                                                , PatternGroup
                                                )
import           Palantype.Common.Internal      ( Greediness )
import           Palantype.Common.SystemLang    ( SystemLang(..) )
import           Palantype.Common.TH            ( failure )
import qualified Palantype.EN.Keys             as EN
                                                ( Key )
import           Safe                           ( atMay
                                                , readMay
                                                )
import           Text.ParserCombinators.ReadP   ( char
                                                , munch1
                                                , option
                                                , pfail
                                                , sepBy1, eof
                                                )
import           Text.ParserCombinators.ReadPrec
                                                ( lift )
import           Text.Read                      ( Read(readPrec)
                                                , read
                                                )
import           Text.Show                      ( Show
                                                , show
                                                )
import           TextShow                       ( TextShow(showb)
                                                , fromText
                                                , showt
                                                )
import           Type.Reflection                ( (:~~:)(HRefl)
                                                , eqTypeRep
                                                , typeRep
                                                )
import           Web.HttpApiData                ( FromHttpApiData
                                                , ToHttpApiData
                                                )
import qualified Palantype.DE as DE

data StageSpecialGeneric key
  = StageSpecial Text
  | StageGeneric (PatternGroup key) Greediness
  deriving stock (Ord, Generic)

instance Palantype key => Show (StageSpecialGeneric key)  where
    show (StageSpecial str ) = Text.unpack str
    show (StageGeneric pg g) = show (pg :: PatternGroup key) <> "_" <> show g

instance Palantype key => Read (StageSpecialGeneric key) where
    readPrec = lift $ option Nothing generic >>= \case
        Just (pg, g) -> pure $ StageGeneric pg g
        Nothing      -> special

      where
        generic = do
            str <- munch1 (/= '-')
            pg  <- maybe pfail pure $ readMay str
            _   <- char '-'
            g   <- read <$> munch1 isDigit
            pure $ Just (pg, g)

        special = StageSpecial . Text.pack <$> munch1 isLetter

instance Palantype key => ToJSON (StageSpecialGeneric key)
instance Palantype key => FromJSON (StageSpecialGeneric key)
deriving stock instance Palantype key => Eq (StageSpecialGeneric key)

data StageHierarchy
  = StageHierarchy Int (Maybe Int)
  deriving stock (Generic, Eq, Ord)

instance Show StageHierarchy where
    show (StageHierarchy t Nothing) = show t
    show (StageHierarchy t (Just s)) = show t <> "-" <> show s

instance Read StageHierarchy where
    readPrec = lift $ option Nothing top >>= \case
        Just t  -> pure $ StageHierarchy t Nothing
        Nothing -> sub
      where
        top = Just . read <$> munch1 isDigit <* eof
        sub = do
            t <- read <$> munch1 isDigit
            _ <- char '-'
            s <- read <$> munch1 isDigit
            eof
            pure $ StageHierarchy t (Just s)

instance ToJSON StageHierarchy
instance FromJSON StageHierarchy

data Stage key = Stage
    { stageSpecialGeneric :: StageSpecialGeneric key
    , stageHierarchy      :: StageHierarchy
    }
    deriving stock (Generic, Eq)

instance Palantype key => ToJSON (Stage key)
instance Palantype key => FromJSON (Stage key)

instance Palantype key => Show (Stage key) where
    show (Stage sg h) = show sg <> "_" <> show h

instance Palantype key => Read (Stage key) where
    readPrec = lift $ munch1 isLetterOrHyphen `sepBy1` char '_' >>= \case
        [str1, str2] -> pure $ Stage (read str1) (read str2)
        _            -> pfail
        where isLetterOrHyphen l = l == '-' || isLetter l

capitalize :: Text -> Text
capitalize str = case uncons str of
    Nothing       -> ""
    Just (h, rem) -> cons (toUpper h) rem

showShort :: Stage key -> Text
showShort (Stage _ (StageHierarchy t (Just s))) = "Stage " <> showt t <> "." <> showt s
showShort (Stage (StageSpecial str) (StageHierarchy _ Nothing)) = str
showShort (Stage (StageGeneric _ _) (StageHierarchy _ Nothing)) =
    $failure "generic stage on top-level"

instance Palantype key => TextShow (Stage key) where
    showb (Stage sg h) = case sg of
        StageSpecial str -> fromText (capitalize str) <> " " <> case h of
            StageHierarchy t Nothing  -> showb t
            StageHierarchy t (Just s) -> showb t <> "." <> showb s
        StageGeneric pg g ->
            fromText (capitalize $ showt pg) <> "-G" <> showb g

-- stage representation

data StageSpecialGenericRepr
  = StageReprSpecial Text
  | StageReprGeneric Text Greediness
  deriving stock (Eq, Generic, Ord)

instance ToJSON StageSpecialGenericRepr
instance FromJSON StageSpecialGenericRepr

-- | Use the 'TextShow'
-- | instance of 'PatternGroup key' to convert a 'Stage key' in a one-way
-- | manner into a legible representation of a stage
data StageRepr = StageRepr
    { srLang                :: SystemLang
    , srStageSpecialGeneric :: StageSpecialGenericRepr
    , srStageHierarchy      :: StageHierarchy
    }
    deriving stock (Eq, Generic, Ord)

instance TextShow StageRepr where
    showb (StageRepr lang sg h) = showb lang <> "/" <> case sg of
        StageReprSpecial str -> fromText (capitalize str) <> " " <> case h of
            StageHierarchy t Nothing  -> showb t
            StageHierarchy t (Just s) -> showb t <> "." <> showb s
        StageReprGeneric str g -> fromText str <> "-G" <> showb g

instance ToJSON StageRepr
instance FromJSON StageRepr

-- stages

stages :: forall key . (Palantype key) => [Stage key]
stages =
    [ Stage (StageSpecial "Introduction")               $ StageHierarchy 0 Nothing
    , Stage (StageSpecial "Type the letters")           $ StageHierarchy 1 $ Just 1
    , Stage (StageSpecial "Memorize the order")         $ StageHierarchy 1 $ Just 2
    , Stage (StageSpecial "Type the letters blindly")   $ StageHierarchy 1 $ Just 3
    , Stage (StageSpecial "Memorize the order blindly") $ StageHierarchy 1 $ Just 4
    , Stage (StageSpecial "Memorize the left hand")     $ StageHierarchy 1 $ Just 5
    , Stage (StageSpecial "Memorize the right hand")    $ StageHierarchy 1 $ Just 6
    , Stage (StageSpecial "Memorize home row")          $ StageHierarchy 1 $ Just 7
    , Stage (StageSpecial "Memorize them all")          $ StageHierarchy 1 $ Just 8
    , Stage (StageSpecial "Building muscle memory")     $ StageHierarchy 2 $ Just 1
    , Stage (StageSpecial "Learn your first chords")    $ StageHierarchy 2 $ Just 2
    ]
    <> if
           | Just HRefl <- typeRep @key `eqTypeRep` typeRep @DE.Key
           -> stagesDE
           | Just HRefl <- typeRep @key `eqTypeRep` typeRep @EN.Key
           -> []
           | -- TODO
             otherwise
           -> []

stagesDE :: [Stage DE.Key]
stagesDE =
    [ Stage (StageSpecial "Onset, nucleus, and coda") $ StageHierarchy 2  $ Just 3
    , Stage (StageSpecial "Syllabes and word parts")  $ StageHierarchy 2  $ Just 4
    , Stage (StageGeneric DE.PatReplCommon1 0)        $ StageHierarchy 3  $ Just 1
    , Stage (StageGeneric DE.PatReplCommon2 0)        $ StageHierarchy 3  $ Just 2
    , Stage (StageGeneric DE.PatCodaComboT 0)         $ StageHierarchy 3  $ Just 3
    , Stage (StageGeneric DE.PatReplCommon1 2)        $ StageHierarchy 4  $ Just 1
    , Stage (StageGeneric DE.PatReplCommon1 3)        $ StageHierarchy 4  $ Just 2
    , Stage (StageGeneric DE.PatReplCommon2 4)        $ StageHierarchy 4  $ Just 3
    , Stage (StageGeneric DE.PatOnsetR 0)             $ StageHierarchy 5  $ Just 1
    , Stage (StageGeneric DE.PatOnsetL 0)             $ StageHierarchy 5  $ Just 2
    , Stage (StageGeneric DE.PatDiConsonant 0)        $ StageHierarchy 5  $ Just 3
    , Stage (StageGeneric DE.PatDiConsonant 2)        $ StageHierarchy 5  $ Just 4
    , Stage (StageGeneric DE.PatCodaH 0)              $ StageHierarchy 6  $ Just 1
    , Stage (StageGeneric DE.PatCodaH 1)              $ StageHierarchy 6  $ Just 2
    , Stage (StageGeneric DE.PatCodaR 0)              $ StageHierarchy 6  $ Just 3
    , Stage (StageGeneric DE.PatCodaR 4)              $ StageHierarchy 6  $ Just 4
    , Stage (StageGeneric DE.PatCodaRR 0)             $ StageHierarchy 6  $ Just 5
    , Stage (StageGeneric DE.PatCodaHR 0)             $ StageHierarchy 6  $ Just 6
    , Stage (StageGeneric DE.PatDt 0)                 $ StageHierarchy 7  $ Just 1
    , Stage (StageGeneric DE.PatDt 2)                 $ StageHierarchy 7  $ Just 2
    , Stage (StageGeneric DE.PatDiphtong 0)           $ StageHierarchy 8  $ Just 1
    , Stage (StageGeneric DE.PatDiphtong 4)           $ StageHierarchy 8  $ Just 2
    , Stage (StageGeneric DE.PatDiphtong 5)           $ StageHierarchy 8  $ Just 3
    , Stage (StageGeneric DE.PatReplC 0)              $ StageHierarchy 9  $ Just 1
    , Stage (StageGeneric DE.PatReplC 2)              $ StageHierarchy 9  $ Just 2
    , Stage (StageGeneric DE.PatBreakUpI 0)           $ StageHierarchy 9  $ Just 3
    , Stage (StageGeneric DE.PatBreakUpI 4)           $ StageHierarchy 9  $ Just 4
    , Stage (StageGeneric DE.PatSwapS 0)              $ StageHierarchy 10 $ Just 1
    , Stage (StageGeneric DE.PatSwapS 2)              $ StageHierarchy 10 $ Just 2
    , Stage (StageGeneric DE.PatSwapSch 0)            $ StageHierarchy 10 $ Just 3
    , Stage (StageGeneric DE.PatSwapSch 2)            $ StageHierarchy 10 $ Just 4
    , Stage (StageGeneric DE.PatSwapZ 0)              $ StageHierarchy 10 $ Just 5
    , Stage (StageGeneric DE.PatDiVowel 0)            $ StageHierarchy 11 $ Just 1
    , Stage (StageGeneric DE.PatDiVowel 1)            $ StageHierarchy 11 $ Just 2
    , Stage (StageGeneric DE.PatDiVowel 4)            $ StageHierarchy 11 $ Just 3
    , Stage (StageGeneric DE.PatCodaGK 3)             $ StageHierarchy 11 $ Just 4
    , Stage (StageGeneric DE.PatReplH 0)              $ StageHierarchy 12 $ Just 1
    , Stage (StageGeneric DE.PatReplH 3)              $ StageHierarchy 12 $ Just 2
    , Stage (StageGeneric DE.PatReplRare 0)           $ StageHierarchy 13 $ Just 1
    , Stage (StageGeneric DE.PatReplRare 3)           $ StageHierarchy 13 $ Just 2
    , Stage (StageGeneric DE.PatSmallS 0)             $ StageHierarchy 13 $ Just 3
    , Stage (StageGeneric DE.PatSmallS 6)             $ StageHierarchy 13 $ Just 4
    , Stage (StageGeneric DE.PatBrief 0)              $ StageHierarchy 14 $ Just 1
    , Stage (StageSpecial "Plover Commands")          $ StageHierarchy 15 $ Just 1
    , Stage (StageSpecial "Fingerspelling")           $ StageHierarchy 15 $ Just 2
    , Stage (StageSpecial "Number Mode")              $ StageHierarchy 15 $ Just 3
    , Stage (StageSpecial "Command Keys")             $ StageHierarchy 15 $ Just 4
    , Stage (StageSpecial "Special Characters")       $ StageHierarchy 15 $ Just 5
    , Stage (StageSpecial "Pattern Overview")         $ StageHierarchy 16 Nothing

    -- TODO: implement tutorials
    , Stage (StageGeneric DE.PatCommonPrefix 7)       $ StageHierarchy 18 $ Just 1
    , Stage (StageGeneric DE.PatCommonPrefix 8)       $ StageHierarchy 18 $ Just 2
    , Stage (StageGeneric DE.PatShortSyllable 5)      $ StageHierarchy 19 $ Just 1
    , Stage (StageGeneric DE.PatShortSyllable 6)      $ StageHierarchy 19 $ Just 2
    , Stage (StageGeneric DE.PatShortSyllable 8)      $ StageHierarchy 19 $ Just 3
    , Stage (StageGeneric DE.PatSCPlus 0)             $ StageHierarchy 19 $ Just 1
    , Stage (StageGeneric DE.PatSCStretch 0)          $ StageHierarchy 20 $ Just 2
    , Stage (StageGeneric DE.PatSCOther 0)            $ StageHierarchy 20 $ Just 3
    , Stage (StageGeneric DE.PatAnglAI 0)             $ StageHierarchy 21 $ Just 1
    , Stage (StageGeneric DE.PatAnglA 0)              $ StageHierarchy 21 $ Just 2
    , Stage (StageGeneric DE.PatAnglI 0)              $ StageHierarchy 21 $ Just 3
    , Stage (StageGeneric DE.PatAnglU 0)              $ StageHierarchy 21 $ Just 4
    , Stage (StageGeneric DE.PatAnglStretchU 0)       $ StageHierarchy 21 $ Just 5
    , Stage (StageGeneric DE.PatAnglO 0)              $ StageHierarchy 21 $ Just 6
    , Stage (StageGeneric DE.PatAnglStretchO 0)       $ StageHierarchy 21 $ Just 7
    , Stage (StageGeneric DE.PatAnglAU 0)             $ StageHierarchy 21 $ Just 8
    , Stage (StageGeneric DE.PatAnglAU 6)             $ StageHierarchy 21 $ Just 9
    , Stage (StageGeneric DE.PatAnglEI 0)             $ StageHierarchy 21 $ Just 10
    , Stage (StageGeneric DE.PatAnglAE 0)             $ StageHierarchy 21 $ Just 10
    , Stage (StageGeneric DE.PatAnglSch 0)            $ StageHierarchy 21 $ Just 11
    , Stage (StageGeneric DE.PatAnglJU 0)             $ StageHierarchy 21 $ Just 12
    , Stage (StageGeneric DE.PatAnglOther 0)          $ StageHierarchy 21 $ Just 13
    , Stage (StageGeneric DE.PatFrankOther 0)         $ StageHierarchy 22 $ Just 1
    , Stage (StageGeneric DE.PatFrankOther 3)         $ StageHierarchy 22 $ Just 2
    , Stage (StageGeneric DE.PatFrankOther 6)         $ StageHierarchy 22 $ Just 3
    , Stage (StageGeneric DE.PatForeignOther 0)       $ StageHierarchy 22 $ Just 2
    , Stage (StageGeneric DE.PatChemistry 0)          $ StageHierarchy 23 $ Just 2
    -- DiConsonant 4 is not meant to be implemented,
    -- delete as soon as no patterns hit anymore
    , Stage (StageGeneric DE.PatDiConsonant 4)        $ StageHierarchy 23 $ Just 2
    ]

instance Palantype key => Default (Stage key) where
    def = head stages

-- functions

mPrev :: StageIndex -> Maybe StageIndex
mPrev i = if i == 0 then Nothing else Just $ pred i

mNext :: forall key . Palantype key => StageIndex -> Maybe StageIndex
mNext i =
    let max = length $ stages @key
    in  if unStageIndex i == max - 1 then Nothing else Just $ succ i

fromIndex :: forall key . Palantype key => StageIndex -> Maybe (Stage key)
fromIndex i = stages `atMay` unStageIndex i

-- | create a link text for entries in the table-of-contents
--   if the greediness is bigger than 1, it is provided, too
toTOCString
    :: forall key
     . Palantype key
    => Stage key
    -> (Int, Maybe Greediness, Text)
toTOCString (Stage sg h) = case sg of
    StageSpecial str -> case h of
        StageHierarchy t Nothing     -> (t, Nothing, str)
        StageHierarchy _ (Just s) -> (s, Nothing, str)
    StageGeneric pg g -> case h of
        StageHierarchy _ Nothing -> $failure "Error: generic stage on top level"
        StageHierarchy _ (Just s) ->
            let mg = if g > 0 then Just g else Nothing
            in  (s, mg, toDescription pg)

toPageName
  :: forall key
   . Palantype key
  => Stage key
  -> Text
toPageName (Stage sg h) = case sg of
    StageSpecial str -> case h of
      StageHierarchy _ Nothing  -> str
      StageHierarchy t (Just s) -> "Stage" <> showt t <> "-" <> showt s <> "_" <> toFileName str
    StageGeneric pg g -> case h of
      StageHierarchy _ Nothing  -> $failure "Error: generic stage on top level"
      StageHierarchy t (Just s) ->
           "Stage" <> showt t <> "-" <> showt s <> "_" <> "G" <> showt g
        <> "_" <> toFileName (toDescription pg)
  where
    toFileName :: Text -> Text
    toFileName = Text.filter (\c -> isAscii c && (isAlphaNum c || c `elem` ['-', '_', '.']))
             <<< Text.replace " " "-"
             <<< Text.replace "ä" "ae"
             <<< Text.replace "ö" "oe"
             <<< Text.replace "ü" "ue"
             <<< Text.replace "Ä" "Ae"
             <<< Text.replace "Ö" "Oe"
             <<< Text.replace "Ü" "Ue"
             <<< Text.replace "ß" "ss"

mapStages
    :: forall key
     . Palantype key
    => Map (StageSpecialGeneric key) (StageIndex, Int, Int)
mapStages = Map.union mapStandardGroups mapFromStages
  where
    mapFromStages =
        Map.fromList $ zip [0 ..] stages
          <&> \(i, Stage ssg (StageHierarchy t ms)) ->
                (ssg, (StageIndex i, t, fromMaybe 0 ms))
    mapStandardGroups = Map.fromList
        [ (StageGeneric patZero 0       , (stageIndexPatZero       , 0, 0))
        , (StageGeneric patSimpleMulti 0, (stageIndexPatSimpleMulti, 0, 0))
        , (StageGeneric patCapitalize 0 , (stageIndexPatCapitalize , 0, 0))
        , (StageGeneric patAcronym 0    , (stageIndexPatAcronym    , 0, 0))
        ]

findStage
    :: forall key
     . Palantype key
    => StageSpecialGeneric key
    -> Maybe (StageIndex, Int, Int)
findStage ssg = Map.lookup ssg mapStages

mapHierarchyStageIndex
  :: forall key. Palantype key => Map (Int, Maybe Int) StageIndex
mapHierarchyStageIndex =
  Map.fromList $ zip [0 ..] (stages @key) <&>
    \(i, Stage _ (StageHierarchy t ms)) -> ((t, ms), i)

findStageIndex :: StageRepr -> Maybe StageIndex
findStageIndex (StageRepr lang sg h) = case lang of
    SystemEN -> snd $ foldl' acc (0, Nothing) $ stages @EN.Key
    SystemDE -> snd $ foldl' acc (0, Nothing) $ stages @DE.Key
  where
    acc
        :: forall key
         . Palantype key
        => (Int, Maybe StageIndex)
        -> Stage key
        -> (Int, Maybe StageIndex)
    acc r@(_, Just _ ) _                              = r
    acc (  i, Nothing) (Stage (StageSpecial str') h') = case sg of
        StageReprSpecial str | str == str' && h == h' ->
            (i, Just $ StageIndex i)
        _ -> (i + 1, Nothing)
    acc (i, Nothing) (Stage (StageGeneric pg' g') _) = case sg of
        StageReprGeneric str g | str == showt pg' && g == g' ->
            (i, Just $ StageIndex i)
        _ -> (i + 1, Nothing)

isValidIndex :: forall key . Palantype key => StageIndex -> Bool
isValidIndex (StageIndex i) = i > 0 && i < length (stages @key)

getSystemLang :: forall key . Palantype key => SystemLang
getSystemLang = if
    | Just HRefl <- typeRep @key `eqTypeRep` typeRep @EN.Key -> SystemEN
    | Just HRefl <- typeRep @key `eqTypeRep` typeRep @DE.Key -> SystemDE
    | otherwise -> $failure "key not implemented"

toStageRepr :: forall key . Palantype key => Stage key -> StageRepr
toStageRepr (Stage sg h) = StageRepr (getSystemLang @key) (toRepr sg) h
  where
    toRepr (StageSpecial str ) = StageReprSpecial str
    toRepr (StageGeneric pg g) = StageReprGeneric (showt pg) g

-- Stage Index

-- | allow to identify a stage by some integer number
-- | This number changes when stages are reordered
-- | Thus it can be used routes but shouldn't be stored in the database
-- | For long-term storage, there is 'StageRepr'
newtype StageIndex = StageIndex { unStageIndex :: Int }
  deriving stock (Eq, Generic, Ord)
  deriving newtype (Enum, FromJSON, ToJSON, FromHttpApiData, ToHttpApiData, Num, Read, Show)

instance Wrapped StageIndex

instance TextShow StageIndex where
    showb = showb <<< unStageIndex

mkStageIndex :: forall key . Palantype key => Int -> Maybe StageIndex
mkStageIndex i | i >= 0 && i < Map.size (mapStages @key) = Just $ StageIndex i
mkStageIndex _ = Nothing

stageIndexPatZero :: StageIndex
stageIndexPatZero = StageIndex 0

stageIndexPatSimpleMulti :: StageIndex
stageIndexPatSimpleMulti = StageIndex 1

stageIndexPatCapitalize :: StageIndex
stageIndexPatCapitalize = StageIndex 1001

stageIndexPatAcronym :: StageIndex
stageIndexPatAcronym = StageIndex 1002

getStageIndexMaybe
  :: forall key
  . Palantype key
  => PatternGroup key
  -> Greediness
  -> Maybe StageIndex
getStageIndexMaybe pg g = view _1 <$> findStage (StageGeneric pg g)

getGroupIndex :: Stage key -> Int
getGroupIndex (Stage _ (StageHierarchy t _)) = t
