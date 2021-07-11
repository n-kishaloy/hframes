{-# LANGUAGE ExistentialQuantification, TemplateHaskell, FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# LANGUAGE Strict, OverloadedLists, OverloadedStrings #-}


module DataFrame
( someFunc
, DataFrame (..), HasRecords (..), HasIndex (..), HasHeader (..)


) where


import Data.Text (Text)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U

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
  DFMBool   (V.Vector (Maybe Bool))     | DFBool    (U.Vector Bool)
  deriving (Show)

data DFIndex =
  DFIdInt   (U.Vector Int)      | 
  DFIdText  (V.Vector Text)     | 
  DFIdDay   (V.Vector Day)      |
  DFIdTime  (V.Vector UTCTime)
  deriving (Show)

data DataFrame = DataFrame
  { dataFrameRecords    ::  V.Vector DF
  , dataFrameIndex      ::  Maybe DFIndex
  , dataFrameHeader     ::  Maybe DFIndex

  } deriving (Show) 

makeFields ''DataFrame

toJSON :: DataFrame -> Text 
toJSON df = undefined 

fromJSON :: Text -> DataFrame 
fromJSON sr = undefined 

jsonNormalize :: Text -> V.Vector Text -> V.Vector Text -> DataFrame
jsonNormalize dat recPath meta = undefined

headerToText :: DataFrame -> DataFrame
headerToText df = undefined 

