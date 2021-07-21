{-# LANGUAGE ExistentialQuantification, TemplateHaskell, FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# LANGUAGE Strict, OverloadedLists, OverloadedStrings #-}


module DataFrame
( someFunc
, DataFrame (..), HasRecords (..), HasKey (..) -- , HasIndex (..)
, getData, getRecord, toHashMap, fromHashMap, toJSON, fromJSON
, toCSV, fromCSV, info, jsonNormalize, headerToText


) where


import Data.Text (Text)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.HashMap.Strict as H
import qualified Data.HashSet as Hs

import Data.Hashable ( Hashable )

import Control.Lens ( makeFields )
-- import Control.Lens.TH

import Data.Time (Day, UTCTime)

someFunc :: IO ()
someFunc = putStrLn "someFunc"


data DF =
  DFMInt    (V.Vector (Maybe Int))      | DFInt     (U.Vector Int)      |
  DFMDouble (V.Vector (Maybe Double))   | DFDouble  (U.Vector Double)   |
  DFMText   (V.Vector (Maybe Text))     | DFText    (V.Vector Text)     |
  DFMUTC    (V.Vector (Maybe UTCTime))  | DFUTC     (V.Vector UTCTime)  |
  DFMDay    (V.Vector (Maybe Day))      | DFDay     (V.Vector Day)      |
  DFMCat    (V.Vector (Maybe (V.Vector Text)))                          |
  DFCat     (V.Vector (V.Vector Text))                                  |
  DFMBool   (V.Vector (Maybe Bool))     | DFBool    (U.Vector Bool)
  deriving  (Show)

data RData =
  RDataMInt     (Maybe Int)             | RDataInt     Int             |
  RDataMDouble  (Maybe Double)          | RDataDouble  Double          |
  RDataMText    (Maybe Text)            | RDataText    Text            |
  RDataMUTC     (Maybe UTCTime)         | RDataUTC     UTCTime         |
  RDataMDay     (Maybe Day)             | RDataDay     Day             |
  RDataMCat     (Maybe (V.Vector Text)) | RDataCat     (V.Vector Text) |
  RDataMBool    (Maybe Bool)            | RDataBool    Bool
  deriving      (Show)

type Record a = H.HashMap a RData

data DFIndex =
  DFIdInt   (H.HashMap Int Int)      |
  DFIdText  (H.HashMap Text Int)     |
  DFIdDay   (H.HashMap Day Int)      |
  DFIdTime  (H.HashMap UTCTime Int)
  deriving  (Show)

data DataFrame a b = (Hashable a, Hashable b) => DataFrame
  { dataFrameRecords    ::  H.HashMap a DF
  , dataFrameKey        ::  Maybe (H.HashMap b Int)
  -- , dataFrameIndex      :: H.HashMap a DFIndex
  }

makeFields ''DataFrame

instance Show (DataFrame a b) where
  show x = undefined

getData :: b -> a -> DataFrame a b -> Maybe RData
getData ky fd = undefined

getRecord :: b -> DataFrame a b -> Maybe (Record a)
getRecord ky = undefined

unsafeGetData :: b -> a -> DataFrame a b -> RData
unsafeGetData ky fd = undefined

unsafeGetRecord :: b -> DataFrame a b -> Record a
unsafeGetRecord ky = undefined

toHashMap :: DataFrame a b -> H.HashMap b (Record a)
toHashMap df = undefined

fromHashMap :: H.HashMap b (Record a) -> DataFrame a b
fromHashMap hs = undefined

toJSON :: DataFrame a b -> Text
toJSON df = undefined

fromJSON :: Text -> DataFrame a b
fromJSON sr = undefined

toCSV :: DataFrame a b -> Text
toCSV df = undefined

fromCSV :: Text -> DataFrame a b
fromCSV sr = undefined

info :: DataFrame a b -> Text
info df = undefined

vecToDF :: V.Vector RData -> Maybe DF
vecToDF rd = case rd V.! 0 of
  RDataInt _ -> V.mapM f rd >>= (Just . DFInt . U.convert) where
    f (RDataInt x) = Just x
    f _ = Nothing

  RDataMInt _ -> V.mapM f rd >>= (Just . DFMInt) where
    f (RDataMInt x) = Just x
    f _ = Nothing

  _ -> Nothing

jsonNormalize :: Text -> V.Vector Text -> V.Vector Text -> DataFrame a b
jsonNormalize dat recPath meta = undefined

headerToText :: DataFrame a b -> DataFrame a b
headerToText df = undefined

idxVecToDF :: U.Vector Int -> DataFrame a b -> DataFrame a b
idxVecToDF id df = undefined

keyVecToDF :: V.Vector b -> DataFrame a b -> Maybe (DataFrame a b)
keyVecToDF ky df = undefined

unsafeKeyVecToDF :: V.Vector b -> DataFrame a b -> DataFrame a b
unsafeKeyVecToDF ky df = undefined

dfFilter :: V.Vector a -> (V.Vector RData -> Bool) -> DataFrame a b -> DataFrame a b
dfFilter vc fn df = undefined

arrange :: Ord c => V.Vector a -> (V.Vector RData -> c) -> DataFrame a b -> DataFrame a b
arrange vc fn df = undefined

select :: Hs.HashSet a -> DataFrame a b -> Maybe (DataFrame a b)
select fl df = undefined

unsafeSelect :: Hs.HashSet a -> DataFrame a b -> DataFrame a b
unsafeSelect fl df = undefined

conditionSelect :: (a -> Bool) -> DataFrame a b -> DataFrame a b
conditionSelect fn df = undefined

rename :: a -> a -> DataFrame a b -> Maybe (DataFrame a b)
rename n0 n1 df = undefined

unsafeRename :: a -> a -> DataFrame a b -> DataFrame a b
unsafeRename n0 n1 df = undefined

mutate :: H.HashMap a (V.Vector a, V.Vector RData -> RData) -> DataFrame a b -> DataFrame a b
mutate xf df = undefined

transmute :: H.HashMap a (V.Vector a, V.Vector RData -> RData) -> DataFrame a b -> DataFrame a b
transmute xf df = undefined



