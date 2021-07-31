{-|
Module      : Data.SpeadSheet
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


module Data.SpreadSheet
(


-- *How to use this library
-- |Add @hframes@ to build-depends and @import DataFrames@

-- |

-- *Documentation
  SpreadSheet, Cell (..), HasValue (..), HasDecimal (..), HasJustify (..)

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

import Data.DType ( DType, Record )

data Justify = Left | Center | Right

data Cell = Cell 
  { cellValue   :: DType
  , cellDecimal :: Int
  , cellJustify :: Justify
  }

makeFields ''Cell

type SpreadSheet = H.HashMap (Int, Int) Cell
