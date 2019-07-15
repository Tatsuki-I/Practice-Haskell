module Bitcoin.Types where

-- リトルエンディアンで表されるWord32
{-
newtype Word32le = Word32le Word32
                   deriving ( Show
                            , Eq
                            , Ord
                            , Enum
                            , Bounded
                            , Num
                            , Real
                            , Integral )
                            -}

--instance Binary Word32le where
--    put (Word32le x) = putWord32le x
--    get = Word32le <$> getWord32lw
