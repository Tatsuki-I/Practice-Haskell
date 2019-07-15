module Scale where

data Scale = C Sign
           | D Sign
           | E Sign
           | F Sign
           | G Sign
           | A Sign
           | B Sign
             deriving ( Eq )

data Sign = None
          | Sharp
          | Flat
            deriving ( Eq )

instance Show Sign where
    show None  = ""
    show Sharp = "#"
    show Flat  = "b"

instance Show Scale where
    show (C s) = f s "C"
    show (D s) = f s "D"
    show (E s) = f s "E"
    show (F s) = f s "F"
    show (G s) = f s "G"
    show (A s) = f s "A"
    show (B s) = f s "B"

f s = if s /= None
         then (++ " " ++ show s)
         else id


