{-# LANGUAGE ExistentialQuantification, TemplateHaskell, FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# LANGUAGE Strict, OverloadedLists, OverloadedStrings #-}


module DataFrame
( someFunc
, DataFrame (..), HasRecords (..), HasKey (..) -- , HasIndex (..)
, findData, findRecord, toHashMap, fromHashMap, toJSON, fromJSON
, toCSV, fromCSV, info, jsonNormalize, headerToText


) where


import Data.Text (Text)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.HashMap.Strict as H

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

findData :: b -> a -> DataFrame a b -> RData
findData ky fd = undefined

findRecord :: b -> DataFrame a b -> Record a
findRecord ky = undefined 

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

jsonNormalize :: Text -> V.Vector Text -> V.Vector Text -> DataFrame a b
jsonNormalize dat recPath meta = undefined

headerToText :: DataFrame a b -> DataFrame a b
headerToText df = undefined 

