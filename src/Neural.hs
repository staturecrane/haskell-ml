module Neural where

import qualified Numeric.LinearAlgebra               as HM  -- non-backprop hmatrix
import qualified Numeric.LinearAlgebra.Data          as HMD
import qualified Numeric.LinearAlgebra.Static        as HMS -- hmatrix with type-checked operations :D

linear weights bias x = (HMS.mul x weights) + bias
relu weights = map (\x -> max 0 x) (HM.toLists weights)
