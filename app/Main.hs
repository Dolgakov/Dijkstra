module Main where

import Dijkstra

main :: IO ()
main = let 
    adj = fromLists [
        [(1,1), (2,5)],
        [(0,1), (2,2), (3,4)],
        [(0,5), (1,2), (3,4)],
        [(1,4), (2,4)]]

    source = 2
    in print $ findPaths adj source 
