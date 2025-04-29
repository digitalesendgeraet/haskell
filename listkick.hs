module Liste (new, head') 
    -- Schnittstelle: Welche Operationen sind implementiert, toBeCompleted!
    -- Zum Beispiel: tail', length', index', reverse', sort'

where
    data Liste a = Empty | K a (Liste a) -- rekursive Definition der Liste und ihres Konstruktors K

    -- Typsignaturen der ADT-Operationen
    new :: Liste a
    head' :: Liste a -> a
    length' :: Liste a -> Int


  
    


    -- toBeCompleted!


    -- die zugehoerigen Definitionen
    new = Empty

    head' (K a b) = a
    head' Empty = error "Leere Liste"
    length' Empty = 0
    lenght' (K a b) = 1 + b 

 

    -- Das Modul ist eine Instanz der Typklasse Show, brauchen wir, um unsere Listen anzeigen zu lassen.
    -- Hier soll nichts geaendert werden!!!
    instance (Show a) => Show (Liste a) where
                        show = showListe -- die show-Funktion der bekannten Datentypen wird ueberschrieben

    showListe Empty   = " "
    showListe (K a b) = show a ++ " " ++ showListe b    -- dafuer wird die bekannte show-Funktion 
                                                        -- der Datentypen (Int, Char, ...) genutzt



    --Testumgebung
    liste1 = K 4 (K 3 (K 2 (K 1 new)))
    liste2 = K 'a' (K 'b' (K 'c' (K 'd' new)))
    --liste3 = insert 4 (insert 2 (insert 6 (insert 5 (insert 1 new))))
    liste3 = new