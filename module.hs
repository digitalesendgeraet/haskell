module Stack (new, isEmpty, push, pop, top)
 
where
    data Stack a = Empty | S a (Stack a) -- Konstruktor S
 
 
    new = Empty
 
    isEmpty Empty = True
    isEmpty _ = False
 
    push Empty el = (S el new)
    push stack el = (S el stack)
 
    pop Empty = error "Empty"
    pop (S a b) = b
 
    top Empty = error "Empty"
    top (S a b) = a
 
   
    instance (Show a) => Show (Stack a) where
                        show = showStack -- Show
 
    showStack Empty   = " "
    showStack (S a b) = show a ++ " " ++ showStack b
 
    --Testumgeung
 
    stack1 = S 4 (S 3 (S 2 (S 1 new)))
    stack2 = S 4 (S 3 (S 2 (S 1 new)))