module Algorithm.State 
    ( State (..)
    ) where

class State a where
    nextStates :: a -> [a]
    isFinal    :: a -> Bool
