module Main where

import Algorithm.State
import Algorithm.Search

instance State Integer where
    isFinal i = i == 11
    nextStates i
        | i < 11    = [i * 2, i + 1]
        | otherwise = []

data IterableState s = IterableState [s] s

instance State s => State (IterableState s) where
    isFinal (IterableState _ a) = isFinal a
    nextStates (IterableState ps a) =
        let qs = ps ++ [a]
        in map (IterableState qs) $ nextStates a

instance Show s => Show (IterableState s) where
    show (IterableState ps a) = show (ps ++ [a])

initState :: State s => s -> IterableState s
initState s = IterableState [] s

search :: Integer -> Maybe (IterableState Integer)
search a = breadthFirst $ initState a

main :: IO ()
main = print $ search 1
