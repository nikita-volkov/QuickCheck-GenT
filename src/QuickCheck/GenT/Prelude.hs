module QuickCheck.GenT.Prelude
  ( module Exports,
    traceM,
  )
where

import Control.Applicative as Exports
import Control.Arrow as Exports hiding (left, right)
import Control.Category as Exports
import Control.Concurrent as Exports hiding (yield)
import Control.Exception as Exports hiding (tryJust)
import Control.Monad as Exports
import Control.Monad.IO.Class as Exports
import Control.Monad.Morph as Exports (MFunctor (..))
import Control.Monad.ST as Exports
import Control.Monad.Trans.Class as Exports
import Data.Data as Exports
import Data.Fixed as Exports
import Data.Foldable as Exports
import Data.IORef as Exports
import Data.Int as Exports
import Data.Ix as Exports
import Data.List as Exports hiding (all, and, any, concat, concatMap, elem, find, foldl, foldl', foldl1, foldr, foldr1, mapAccumL, mapAccumR, maximum, maximumBy, minimum, minimumBy, notElem, or, product, sum)
import Data.Maybe as Exports
import Data.Monoid as Exports
import Data.Ord as Exports (Down (..))
import Data.Ratio as Exports
import Data.STRef as Exports
import Data.String as Exports
import Data.Traversable as Exports hiding (for)
import Data.Tuple as Exports
import Data.Word as Exports
import Debug.Trace as Exports hiding (traceM)
import GHC.Exts as Exports (groupWith, sortWith)
import GHC.Generics as Exports (Generic)
import GHC.IO.Exception as Exports
import System.Exit as Exports
import System.IO as Exports (Handle, hClose)
import System.IO.Error as Exports
import System.IO.Unsafe as Exports
import System.Mem.StableName as Exports
import System.Timeout as Exports
import Text.Read as Exports (readEither, readMaybe)
import Unsafe.Coerce as Exports
import Prelude as Exports hiding (FilePath, all, and, any, concat, concatMap, elem, foldl, foldl1, foldr, foldr1, id, mapM, mapM_, maximum, minimum, notElem, or, product, sequence, sequence_, sum, (.))

traceM :: (Monad m) => String -> m ()
traceM s = trace s $ return ()
