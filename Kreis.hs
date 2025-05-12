module Kreis where
    import Punkt
    data Kreis = Kreis Punkt Int

    instance Show Kreis where
        show = showKreis

    showKreis (Kreis a b) = (showPunkt a ++ ", " ++ show b)