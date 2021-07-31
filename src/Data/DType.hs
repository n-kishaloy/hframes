{-|
Module      : Data.DType
Description : Implement DataFrames in Haskell
Copyright   : (c) 2021 Kishaloy Neogi
License     : MIT
Maintainer  : Kishaloy Neogi
Email       : nkishaloy@yahoo.com

This module implements the Data.DType module where the base DType and the Record system


You may see the github repository at <https://github.com/n-kishaloy/hframes>
-}


{-# LANGUAGE ExistentialQuantification, TemplateHaskell, FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# LANGUAGE Strict, OverloadedLists, OverloadedStrings #-}


module Data.DType
(


-- *How to use this library
-- |Add @hframes@ to build-depends and @import DataFrames@

-- |

-- *Documentation
  DType (..), Record

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

data DType =
  DTypeMInt     (Maybe Int)             | DTypeInt     Int             |
  DTypeMDouble  (Maybe Double)          | DTypeDouble  Double          |
  DTypeMText    (Maybe Text)            | DTypeText    Text            |
  DTypeMUTC     (Maybe UTCTime)         | DTypeUTC     UTCTime         |
  DTypeMDay     (Maybe Day)             | DTypeDay     Day             |
  DTypeMCat     (Maybe Int)             | DTypeCat     Int             |
  DTypeMBool    (Maybe Bool)            | DTypeBool    Bool            |
  DTypeFunc     (DType -> DType)

type Record a = H.HashMap a DType

type RecordVec a = V.Vector (Record a)

addMaybeDType :: DType -> DType
addMaybeDType (DTypeInt     x)  = DTypeMInt    (Just x)
addMaybeDType (DTypeDouble  x)  = DTypeMDouble (Just x)
addMaybeDType (DTypeText    x)  = DTypeMText   (Just x)
addMaybeDType (DTypeUTC     x)  = DTypeMUTC    (Just x)
addMaybeDType (DTypeDay     x)  = DTypeMDay    (Just x)
addMaybeDType (DTypeCat     x)  = DTypeMCat    (Just x)
addMaybeDType (DTypeBool    x)  = DTypeMBool   (Just x)
addMaybeDType _                 = error "Wrong type"

removeMaybeDType :: DType -> Maybe DType
removeMaybeDType (DTypeMInt    (Just x))  = Just (DTypeInt    x)
removeMaybeDType (DTypeMDouble (Just x))  = Just (DTypeDouble x)
removeMaybeDType (DTypeMText   (Just x))  = Just (DTypeText   x)
removeMaybeDType (DTypeMUTC    (Just x))  = Just (DTypeUTC    x)
removeMaybeDType (DTypeMDay    (Just x))  = Just (DTypeDay    x)
removeMaybeDType (DTypeMCat    (Just x))  = Just (DTypeCat    x)
removeMaybeDType (DTypeMBool   (Just x))  = Just (DTypeBool   x)
removeMaybeDType _                        = Nothing 

unsafeRemoveMaybeDType :: DType -> DType
unsafeRemoveMaybeDType (DTypeMInt    (Just x))  = DTypeInt    x
unsafeRemoveMaybeDType (DTypeMDouble (Just x))  = DTypeDouble x
unsafeRemoveMaybeDType (DTypeMText   (Just x))  = DTypeText   x
unsafeRemoveMaybeDType (DTypeMUTC    (Just x))  = DTypeUTC    x
unsafeRemoveMaybeDType (DTypeMDay    (Just x))  = DTypeDay    x
unsafeRemoveMaybeDType (DTypeMCat    (Just x))  = DTypeCat    x
unsafeRemoveMaybeDType (DTypeMBool   (Just x))  = DTypeBool   x
unsafeRemoveMaybeDType _ = error "Nothing"


