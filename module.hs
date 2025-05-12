module Stack (new, )
 
 
where
   
    data Stack a = Empty | K a (Stack a)
 
    new :: Stack a
    top :: Stack a => a

 
    new = Empty
    top (K a b) = a 
 
 
 
    instance (Show a) => Show (Stack a) where
                        show = showStack
 
    showStack Empty   = " "
    showStack (K a b) = show a ++ " " ++ showStack b    
                                                       
 
 
 
    --Testumgebung
    stack1 = K 4 (K 3 (K 2 (K 1 new)))
    stack2 = K 'a' (K 'b' (K 'c' (K 'd' new)))
 