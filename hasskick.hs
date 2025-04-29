import Data.List

insid a (x:xs) |a==x = True
               |a!=x = insid a (xs)
insid a [] = False

verschBuchst (x:xs)|insid x [] =    