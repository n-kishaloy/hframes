{-|
Module      : hframes
Description : Implement DataFrames in Haskell
Copyright   : (c) 2021 Kishaloy Neogi
License     : MIT
Maintainer  : Kishaloy Neogi
Email       : nkishaloy@yahoo.com



You may see the github repository at <https://github.com/n-kishaloy/hframes>
-}


{-# LANGUAGE ExistentialQuantification, TemplateHaskell, FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# LANGUAGE Strict, OverloadedLists, OverloadedStrings #-}


module DataFrame
(


-- *How to use this library
-- |Add @hframes@ to build-depends and @import DataFrames@

-- |

-- *Documentation
  DataFrame (..), HasRecords (..), HasIndex (..), HasUniqueKey (..)
, getData, getRecord, toJSON, fromJSON
, toCSV, fromCSV, info, jsonNormalize, headerToText


) where


import Data.Text (Text)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.HashMap.Strict as H
import qualified Data.HashSet as Hs

import Data.Hashable ( Hashable )

import Control.Lens
import Data.Function ((&))
-- import Control.Lens.TH

import Data.Time (Day, UTCTime)

{-|@DF = Vector of each Datatype supported by DataFrames@

-}
data DFVec =
  DFMInt    (V.Vector (Maybe Int))      | DFInt     (U.Vector Int)      |
  DFMDouble (V.Vector (Maybe Double))   | DFDouble  (U.Vector Double)   |
  DFMText   (V.Vector (Maybe Text))     | DFText    (V.Vector Text)     |
  DFMUTC    (V.Vector (Maybe UTCTime))  | DFUTC     (V.Vector UTCTime)  |
  DFMDay    (V.Vector (Maybe Day))      | DFDay     (V.Vector Day)      |
  DFMCat    (V.Vector (Maybe Int))      | DFCat     (V.Vector Int)      |
  DFMBool   (V.Vector (Maybe Bool))     | DFBool    (U.Vector Bool)
  deriving  (Show)

data DType =
  DTypeMInt     (Maybe Int)             | DTypeInt     Int             |
  DTypeMDouble  (Maybe Double)          | DTypeDouble  Double          |
  DTypeMText    (Maybe Text)            | DTypeText    Text            |
  DTypeMUTC     (Maybe UTCTime)         | DTypeUTC     UTCTime         |
  DTypeMDay     (Maybe Day)             | DTypeDay     Day             |
  DTypeMCat     (Maybe Int)             | DTypeCat     Int             |
  DTypeMBool    (Maybe Bool)            | DTypeBool    Bool
  deriving      (Show)

type Record a = H.HashMap a DType

data DFIndex = 
  DFKeyInt    (H.HashMap Int (V.Vector Int))      |
  DFKeyText   (H.HashMap Text (V.Vector Int))     |
  DFKeyDay    (H.HashMap Day (V.Vector Int))      |
  DFKeyTime   (H.HashMap UTCTime (V.Vector Int))  |
  DFKeyBool   (H.HashMap Bool (V.Vector Int))

data UniqueIndex = 
  UniqueKeyInt    (H.HashMap Int Int)      |
  UniqueKeyText   (H.HashMap Text Int)     |
  UniqueKeyDay    (H.HashMap Day Int)      |
  UniqueKeyTime   (H.HashMap UTCTime Int)  |
  UniqueKeyBool   (H.HashMap Bool Int)

data DataFrame a = Hashable a => DataFrame
  { dataFrameRecords    ::  H.HashMap a DFVec
  , dataFrameIndex      ::  H.HashMap a DFIndex
  , dataFrameUniqueKey  ::  H.HashMap a UniqueIndex
  , dataFrameCategory   ::  H.HashMap a (H.HashMap Text Int, V.Vector Text)
  }

makeFields ''DataFrame

instance Show (DataFrame a) where
  show x = undefined

idxDFToDType :: DFVec -> Int -> Maybe DType
idxDFToDType (DFMInt v) ix    = (v V.!? ix) >>= Just . DTypeMInt
idxDFToDType (DFInt v) ix     = (v U.!? ix) >>= Just . DTypeInt
idxDFToDType (DFMDouble v) ix = (v V.!? ix) >>= Just . DTypeMDouble
idxDFToDType (DFDouble v) ix  = (v U.!? ix) >>= Just . DTypeDouble
idxDFToDType (DFMText v) ix   = (v V.!? ix) >>= Just . DTypeMText
idxDFToDType (DFText v) ix    = (v V.!? ix) >>= Just . DTypeText
idxDFToDType (DFMUTC v) ix    = (v V.!? ix) >>= Just . DTypeMUTC
idxDFToDType (DFUTC v) ix     = (v V.!? ix) >>= Just . DTypeUTC
idxDFToDType (DFMDay v) ix    = (v V.!? ix) >>= Just . DTypeMDay
idxDFToDType (DFDay v) ix     = (v V.!? ix) >>= Just . DTypeDay
idxDFToDType (DFMCat v) ix    = (v V.!? ix) >>= Just . DTypeMCat
idxDFToDType (DFCat v) ix     = (v V.!? ix) >>= Just . DTypeCat
idxDFToDType (DFMBool v) ix   = (v V.!? ix) >>= Just . DTypeMBool
idxDFToDType (DFBool v) ix    = (v U.!? ix) >>= Just . DTypeBool

unsafeIdxDFToDType :: DFVec -> Int -> DType
unsafeIdxDFToDType (DFMInt v) ix    = DTypeMInt     (V.unsafeIndex v ix) 
unsafeIdxDFToDType (DFInt v) ix     = DTypeInt      (U.unsafeIndex v ix) 
unsafeIdxDFToDType (DFMDouble v) ix = DTypeMDouble  (V.unsafeIndex v ix) 
unsafeIdxDFToDType (DFDouble v) ix  = DTypeDouble   (U.unsafeIndex v ix) 
unsafeIdxDFToDType (DFMText v) ix   = DTypeMText    (V.unsafeIndex v ix) 
unsafeIdxDFToDType (DFText v) ix    = DTypeText     (V.unsafeIndex v ix) 
unsafeIdxDFToDType (DFMUTC v) ix    = DTypeMUTC     (V.unsafeIndex v ix) 
unsafeIdxDFToDType (DFUTC v) ix     = DTypeUTC      (V.unsafeIndex v ix) 
unsafeIdxDFToDType (DFMDay v) ix    = DTypeMDay     (V.unsafeIndex v ix) 
unsafeIdxDFToDType (DFDay v) ix     = DTypeDay      (V.unsafeIndex v ix) 
unsafeIdxDFToDType (DFMCat v) ix    = DTypeMCat     (V.unsafeIndex v ix) 
unsafeIdxDFToDType (DFCat v) ix     = DTypeCat      (V.unsafeIndex v ix) 
unsafeIdxDFToDType (DFMBool v) ix   = DTypeMBool    (V.unsafeIndex v ix) 
unsafeIdxDFToDType (DFBool v) ix    = DTypeBool     (U.unsafeIndex v ix) 

-- |Get DType from key and field. In case key does not exist  
getData :: DataFrame a -> Int -> a -> Maybe DType
getData df ky fd = undefined -- p V.! z where
  -- q = H.lookup fd (df ^. records) 
  -- p = (df ^. key) >>= H.lookup ky

getRecord :: DataFrame a -> Int -> Maybe (Record a)
getRecord df ky = undefined

unsafeGetData :: DataFrame a -> Int -> a -> DType
unsafeGetData df ky fd = undefined

unsafeGetRecord :: DataFrame a -> Int -> Record a
unsafeGetRecord df ky = undefined

toJSON :: DataFrame a -> Text
toJSON df = undefined

fromJSON :: Text -> DataFrame a
fromJSON sr = undefined

toCSV :: DataFrame a -> Text
toCSV df = undefined

fromCSV :: Text -> DataFrame a
fromCSV sr = undefined

info :: DataFrame a -> Text
info df = undefined

vecToDFVec :: V.Vector DType -> Maybe DFVec
vecToDFVec rd = case rd V.! 0 of
  DTypeInt _ -> V.mapM f rd >>= (Just . DFInt . U.convert) where
    f (DTypeInt x) = Just x
    f _ = Nothing

  DTypeMInt _ -> V.mapM f rd >>= (Just . DFMInt) where
    f (DTypeMInt x) = Just x
    f _ = Nothing

  _ -> Nothing

dfToVec :: DFVec -> V.Vector DType
dfToVec df = undefined

addMaybeDFVec :: DFVec -> DFVec
addMaybeDFVec df = undefined

removeMaybeDFVec :: DFVec -> Maybe DFVec
removeMaybeDFVec df = undefined

unsafeRemoveMaybeDFVec :: DFVec -> DFVec
unsafeRemoveMaybeDFVec df = undefined

addMaybeDType :: DType -> DType
addMaybeDType rd = undefined

removeMaybeDType :: DType -> Maybe DType
removeMaybeDType rd = undefined

unsafeRemoveMaybeDType :: DType -> DType
unsafeRemoveMaybeDType rd = undefined

removeMaybeDataFrame :: V.Vector a -> DataFrame a -> Maybe (DataFrame a)
removeMaybeDataFrame vc df = undefined

jsonNormalize :: Text -> V.Vector Text -> V.Vector Text -> DataFrame a
jsonNormalize dat recPath meta = undefined

headerToText :: DataFrame a -> DataFrame a
headerToText df = undefined

idxVecToDF :: U.Vector Int -> DataFrame a -> DataFrame a
idxVecToDF id df = undefined

dfFilter :: V.Vector a -> (V.Vector DType -> Bool) -> DataFrame a -> DataFrame a
dfFilter vc fn df = undefined

arrange :: Ord c => V.Vector a -> (V.Vector DType -> c) -> DataFrame a -> DataFrame a
arrange vc fn df = undefined

select :: Hs.HashSet a -> DataFrame a -> Maybe (DataFrame a)
select fl df = undefined

unsafeSelect :: Hs.HashSet a -> DataFrame a -> DataFrame a
unsafeSelect fl df = undefined

conditionSelect :: (a -> Bool) -> DataFrame a -> DataFrame a
conditionSelect fn df = undefined

rename :: a -> a -> DataFrame a -> Maybe (DataFrame a)
rename n0 n1 df = undefined

unsafeRename :: a -> a -> DataFrame a -> DataFrame a
unsafeRename n0 n1 df = undefined

mutate :: H.HashMap a (V.Vector a, V.Vector DType -> DType) -> DataFrame a -> DataFrame a
mutate xf df = undefined

transmute :: H.HashMap a (V.Vector a, V.Vector DType -> DType) -> DataFrame a -> DataFrame a
transmute xf df = undefined



