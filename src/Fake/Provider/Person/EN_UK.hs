{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module Fake.Provider.Person.EN_UK
  (
    FullName(..)
  , GenderList(..)
  , Gender(..)
  , fullName
  , femaleName
  , maleName
  ) where

import Data.Text (Text)
import Data.FileEmbed (embedFile, makeRelativeToProject)
import Data.ByteString (ByteString)
import Data.Vector (Vector)
import Data.List(foldl')
import Data.List.Split (chunksOf)


import qualified Data.Vector as V
import qualified Data.Text as T
import Data.Text.Encoding(decodeUtf8)

import Fake.Combinators
import Fake


data GenderList =
   GenderListFemale
 | GenderListMale
 | GenderListBoth


class PickGenderList a where
  pickGenderList :: a -> GenderList

instance PickGenderList GenderList where
  pickGenderList = id

instance PickGenderList Gender where
  pickGenderList GenderFemale = GenderListFemale
  pickGenderList GenderMale = GenderListMale
  pickGenderList GenderUnspecified = GenderListBoth

data Gender =
    GenderFemale
  | GenderMale
  | GenderUnspecified
  deriving (Enum, Bounded)

data FullName = FullName {
    firstName :: Text
  , lastName :: Text
  } deriving (Eq, Show)

fullName :: FGen FullName
fullName = fakeFullNameForGender =<< fakeGender

maleName :: FGen FullName
maleName = fakeFullNameForGender GenderMale

femaleName :: FGen FullName
femaleName = fakeFullNameForGender GenderFemale

fakeFullNameForGender :: (PickGenderList a) => a -> FGen FullName
fakeFullNameForGender g = FullName <$>
  fakeNameForGender g <*>
  elementsV (lastNames sources)


fakeGender :: FGen Gender
fakeGender = fakeEnum

fakeNameForGender :: (PickGenderList a) => a -> FGen Text
fakeNameForGender g = do
  frequency [
     (40, useCommonNameList g)
   , (60, useFullNameList g)
   ]

useCommonNameList :: (PickGenderList a) => a -> FGen Text
useCommonNameList g = frequency . map (mapSnd elementsV) .  (flip ($) sources) =<<
    case pickGenderList g of
      GenderListFemale -> pure (commonFemaleNames)
      GenderListMale -> pure (commonMaleNames)
      GenderListBoth -> elements [commonFemaleNames, commonMaleNames]



useFullNameList :: (PickGenderList a) => a -> FGen Text
useFullNameList g = elementsV . (flip ($) sources) =<<
    case pickGenderList g of
      GenderListFemale -> pure (femaleNames)
      GenderListMale -> pure (maleNames)
      GenderListBoth -> elements [(femaleNames), (maleNames)]



elementsV :: Vector a -> FGen a
elementsV v = (v V.!) <$> fromRange (0, V.length v - 1)


vectorFromFile :: ByteString -> Vector Text
vectorFromFile = V.fromList . T.lines . decodeUtf8

mapSnd :: (b -> c) -> (a, b) -> (a, c)
mapSnd f (a, b) = (a, f b)

weightVectorForFrequency :: Vector Text -> [(Int, Vector Text)]
weightVectorForFrequency v =
  let chunks = chunksOf (max 1 (round $ ((fromIntegral (V.length v) :: Double) / 10) )) $ V.toList v
  in map (mapSnd  V.fromList) $ fst $ foldl' doChunk ([], length chunks) chunks
  where
    doChunk :: ([(Int, [Text])], Int) -> [Text] -> ([(Int, [Text])], Int)
    doChunk (freqs, l) ts =
      let newF = (l + 1) * 10 + length ts * 5
      in ((newF, ts):freqs, l-1)


data Sources = Sources {
   femaleNames       :: Vector Text
 , maleNames         :: Vector Text
 , commonFemaleNames :: [(Int, Vector Text)]
 , commonMaleNames   :: [(Int, Vector Text)]
 , lastNames         :: Vector Text
}

sources :: Sources
sources =
  let a = $(makeRelativeToProject "src/Fake/Provider/Person/data/femaleNames.csv" >>= embedFile)
      b = $(makeRelativeToProject "src/Fake/Provider/Person/data/maleNames.csv" >>= embedFile)
      c = $(makeRelativeToProject "src/Fake/Provider/Person/data/commonFemaleNames.csv" >>= embedFile)
      d = $(makeRelativeToProject "src/Fake/Provider/Person/data/commonMaleNames.csv" >>= embedFile)
      e = $(makeRelativeToProject "src/Fake/Provider/Person/data/lastNames.csv" >>= embedFile)
  in Sources {
      femaleNames       = vectorFromFile a
    , maleNames         = vectorFromFile b
    , commonFemaleNames = weightVectorForFrequency . vectorFromFile $ c
    , commonMaleNames   = weightVectorForFrequency . vectorFromFile $ d
    , lastNames         = vectorFromFile e
  }

