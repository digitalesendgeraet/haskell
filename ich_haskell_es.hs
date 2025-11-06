-- Methoden spezifizieren und  implementieren
module Set (Set) -- new, isEmpty, isIn, insert, remove, isIn, cardinality)
where
data Set a = Empty | K a (Set a)

new  		   :: Set a
isEmpty      :: Set a -> Bool
isIn	       :: Eq a => a -> Set a -> Bool
insert	   :: Eq a => a -> Set a -> Set a
remove	   :: Eq a => a -> Set a -> Set a
cardinality  :: Eq a => Set a -> Set a -> Bool

instance (Show a) => Show (Set a) where
    show = showSet
showSet aSet = "{" ++ showM aSet ++ "}"
showM Empty  = ""
showM (K a Empty) = show a
showM (K a b) = show a ++ "," ++ showM b

new = Empty

isEmpty Empty = True
isEmpty (K a b) = False

isIn x Empty = False
isIn x (K a b)
    | a == x = True
    | otherwise = isIn b

insert x Empty = (K a Empty)
insert x (K a b) 
    | isIn x (K a b) == True = K a b
    | otherwise == K x (K a b)

remove x Empty = Empty
remove x (K a b)
    | a == x = remove x b
    | otherwise = K a (remove x b)

--Testmengen
m1= K 3 Empty 
m2 = K 7 (K 9 (K 4 m1))
m3 = K 5 (K 9 m2) -- ist keine Menge, denn der Konstruktor prueft nicht, ob das Element schon vorhanden


