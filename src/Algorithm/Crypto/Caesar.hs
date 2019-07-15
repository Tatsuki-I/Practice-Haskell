module Algorithm.Crypto.Caesar where

import Data.Char (ord, chr)

caesar n =  map (f n)
            where f _ ' ' = ' '
                  f n c   =  chr (mod ((ord c + n) - ord 'a') 26 + ord 'a')

rot13 = caesar 13
