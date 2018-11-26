module GroupAnalysis.Generator (generateGroup) where

import Data.List (nub)

import GroupAnalysis.Functions (fixedPoint, outer)
import GroupAnalysis.Matrix (matMultMod)

generateGroup :: Real a => a -> [[[a]]] -> [[[a]]]
generateGroup modulus = fixedPoint (\ a -> nub $ concat $ outer (matMultMod modulus) a a)
