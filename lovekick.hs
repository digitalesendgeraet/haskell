import Data.Char
add a b = a + b
letter2Num :: Char -> Int
letter2Num a = ord a
 
num2Letter :: Int -> Char
num2Letter a = chr a
 
caesar_ :: Char -> Int -> Char
caesar_ a b = num2Letter ((letter2Num a) + b)
 
caesar :: [Char] -> Int -> [Char]
caesar [] b = []
caesar (x:xs) b = (caesar_ x b): caesar xs b
 
laenge :: [Char] -> Int
laenge [] = 0
laenge (x:xs) = 1 + laenge xs
 
produkt :: Int -> Int -> Int -> Int
produkt a b c = a*b*c

fak :: Int -> Int
fak n |n>1 = n*(fak (n-1))
      |otherwise = n