module Saizeriya where

import Data.List     (maximumBy)
import Data.Function (on)

data Menu =
     Menu { kcal :: KCal
          , cost :: Cost
          , name :: Name }
            deriving ( Show )

type Cost = Int
type KCal = Int
type Name = String

m1 = Menu { kcal = 3
          , cost = 2
          , name = "1" }

m2 = Menu { kcal = 2
          , cost = 1
          , name = "2" }

m3 = Menu { kcal = 6
          , cost = 3
          , name = "3" }

m4 = Menu { kcal = 1
          , cost = 2
          , name = "4" }

m5 = Menu { kcal = 3
          , cost = 1
          , name = "5" }

sampleMenu = [m1, m2, m3, m4, m5]

sumCost :: [Menu] -> Cost
sumCost =  sum . map cost

sumKCal :: [Menu] -> KCal
sumKCal =  sum . map kcal

saizeriyaDP        :: Cost -> [Menu] -> [Menu]
saizeriyaDP mxKCal =  snd . last . foldl step initial
                      where initial     = replicate (mxKCal + 1) empty
                            empty       = (0, [])
                            step pm m = zipWith chooseKCal
                                                included
                                                notIncluded
                                          where included        = shift ++
                                                                  map addMenu pm
                                                notIncluded     = pm
                                                addMenu (k, ms) = ( k + kcal m
                                                                  , m : ms )
                                                chooseKCal a b  = maximumBy (compare `on` fst)
                                                                            [a, b]
                                                shift           = replicate (cost m)
                                                                            empty

