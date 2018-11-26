module GroupAnalysis.Matrix ( matScalarMult
                            , matMult
                            , matMultMod
                            , matPow
                            , matPowMod
                            , matSub
                            , matSubMod
                            , matAdd
                            , matAddMod
                            , identity
                            , identityMod
                            ) where

import Data.List (transpose)
import Data.Fixed (mod')

applyMod :: Real a => a -> [[a]] -> [[a]]
applyMod modulus = (map . map) (flip mod' modulus)

matScalarMult :: Num a => [[a]] -> a -> [[a]]
matScalarMult m s = (map . map) (* s) m

matMult :: Num a => [[a]] -> [[a]] -> [[a]]
matMult a b = [[sum $ zipWith (*) ar bc | bc <- (transpose b)] | ar <- a]

matMultMod :: Real a => a -> [[a]] -> [[a]] -> [[a]]
matMultMod modulus a b = applyMod modulus $ matMult a b

matPow :: Num a => [[a]] -> Int -> [[a]]
matPow m p = iterate (matMult m) m !! max 0 p

matPowMod :: Real a => a -> [[a]] -> Int -> [[a]]
matPowMod modulus m p = iterate (matMultMod modulus m) m !! max 0 p

matSub :: Real a => [[a]] -> [[a]] -> [[a]]
matSub m1 m2 = (zipWith . zipWith) (-) m1 m2

matSubMod :: Real a => a -> [[a]] -> [[a]] -> [[a]]
matSubMod modulus m1 m2 = applyMod modulus $ matSub m1 m2

matAdd :: Real a => [[a]] -> [[a]] -> [[a]]
matAdd m1 m2 = (zipWith . zipWith) (+) m1 m2

matAddMod :: Real a => a -> [[a]] -> [[a]] -> [[a]]
matAddMod modulus m1 m2 = applyMod modulus $ matAdd m1 m2

identity :: Num a => Int -> [[a]]
identity dimensions = [[fromIntegral $ fromEnum $ i == j | j <- [0 .. l]] | i <- [0 .. l]]
    where l = dimensions - 1

identityMod :: Num a => Int -> a -> [[a]]
identityMod dimensions modulus = (identity dimensions) `matScalarMult` (modulus - 1)
