{-# LANGUAGE DataKinds                                #-}

module MNIST where
import           Control.DeepSeq
import           Control.Exception.Base
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.State
import           Data.Bitraversable
import           Data.IDX
import           Data.Traversable
import           Data.Tuple
import qualified Data.Vector                         as V
import qualified Data.Vector.Generic                 as VG
import qualified Data.Vector.Unboxed                 as VU
import qualified Numeric.LinearAlgebra               as HM  -- non-backprop hmatrix
import qualified Numeric.LinearAlgebra.Data          as HMD
import qualified Numeric.LinearAlgebra.Static        as HMS -- hmatrix with type-checked operations :D

loadMNIST
   :: FilePath
   -> FilePath
   -> IO (Maybe [(HMS.L 28 28, HMS.R 10)])
loadMNIST fpI fpL = runMaybeT $ do
   i <- MaybeT          $ decodeIDXFile       fpI
   l <- MaybeT          $ decodeIDXLabelsFile fpL
   d <- MaybeT . return $ labeledIntData l i
   r <- MaybeT . return $ for d (bitraverse mkImage mkLabel . swap)
   liftIO . evaluate $ force r
      where
         mkImage :: VU.Vector Int -> Maybe (HMS.L 28 28)
         mkImage = HMS.create . HMD.reshape 28 . VG.convert . VG.map (\i -> fromIntegral i / 255)
         mkLabel :: Int -> Maybe (HMS.R 10)
         mkLabel n = HMS.create $ HM.build 10 (\i -> if round i == n then 1 else 0)

