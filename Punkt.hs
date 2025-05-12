module Punkt where
    data Punkt = Punkt Int Int 

    instance Show Punkt where
        show = showPunkt

    showPunkt (Punkt x y) = "(" ++ show x ++ "|" ++ show y ++ ")"
