module Spiral (spiral) where

import Data.Matrix


spiral :: Int -> [[Int]]
spiral i = toLists $ matrix i i matrix_
    where 
        matrix_ (r,c)
            | r==1 = c
            | c==i = r+i-1
            | r==i = 3*i-c-1
            | c==1 = 4*i-r-2
            | otherwise = (4*i-4) +
                getElem (r-1) (c-1) 
                (fromLists ( spiral (i-2)))