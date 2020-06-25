module Algorithm.Search
    ( breadthFirst
    , depthFirst
    ) where

import Data.Maybe
import Algorithm.State

depthFirst :: State a => a -> Maybe a
depthFirst s
    | isFinal s = Just s
    | otherwise = findFirst . map depthFirst $ nextStates s
        where
            findFirst :: [Maybe b] -> Maybe b
            findFirst (Just a:_)   = Just a
            findFirst (Nothing:as) = findFirst as
            findFirst []           = Nothing

breadthFirst :: State a => a -> Maybe a
breadthFirst a = breadthFirst' [a]
    where
        breadthFirst' :: State a => [a] -> Maybe a
        breadthFirst' (s:ss)
            | isFinal s = Just s
            | otherwise = breadthFirst' (ss ++ nextStates s)
        breadthFirst' [] = Nothing