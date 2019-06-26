module Unoverflowable where

newtype Unflowable a = Enther (a String)

--safeDiv :: Fractional a => Unflowable a -> Unflowable a -> Unflowable a
safeDiv _ (Right 0) = Left "Error: Divide by Zero."
safeDiv (Left _) _ = Left "Error: Not a number."
safeDiv _ (Left _) = Left "Error: Not a number."
--safeDiv a b = do either () ()


