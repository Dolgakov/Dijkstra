module Dijkstra (
    Vertex,
    fromLists,
    dijkstra,
    findPaths
    ) where

import Data.Vector ((!), (//), Vector, foldr', fromList)
import qualified Data.Vector as V
import qualified Data.Set as S
import Data.List (sortBy)
import Data.Maybe (fromJust)

data Vertex = Vertex {
    label :: Int,
    weight :: Float
}

type AdjList = Vector (Vector Vertex)
type Distance = Vector Float
type Previous = Vector (Maybe Label)
inf = 1 / 0
type Label = Int
type Weight = Float
type ShortestPath = [Label]

-- [[(Label, Weight)]] -> AdjList
fromLists :: [[(Label, Weight)]] -> AdjList
fromLists xs = fromList (fmap fromList ((fmap . fmap) (\(l,w) -> Vertex l w) xs))

for = flip fmap

findPaths :: AdjList -> Label -> [Maybe (ShortestPath, Weight)]
findPaths adj s = let (dist, prev) = dijkstra adj s 
    in for [0..(V.length adj - 1)] $ \d -> do
        p <- prev ! d
        return $ ((loop prev [] d), dist ! d)
            where 
                loop prev_  path vert 
                    | vert == s = s : path
                    | otherwise = loop prev_ (vert : path) (next vert)
                        where 
                            next v = fromJust $ prev_ ! v


dijkstra :: AdjList -> Label -> (Distance, Previous)
dijkstra adj source = let
    vertNum = V.length adj
    vertSet = S.fromList [0 .. fromIntegral (vertNum - 1)] 
    dist = V.replicate vertNum inf
    prev = V.replicate vertNum Nothing
    
    dist' = dist // [(source, 0)]

    in loop vertSet adj dist' prev

loop :: S.Set Label -> AdjList -> Distance -> Previous -> (Distance, Previous)
loop set adj dist prev
    | S.null set = (dist, prev)
    | otherwise = let
        u = inSetMinDist set dist
        set' = S.delete u set
        neighbors = adj ! u
        (dist', prev') = foldr' (\vertex (distTemp, prevTemp) -> 
            relax u vertex (distTemp, prevTemp)) (dist, prev) neighbors
        in loop set' adj dist' prev'

inSetMinDist :: S.Set Label -> Distance -> Label
inSetMinDist s dist = let
    l = [(dist ! i, i) | i <- (S.toList s)]
    -- sortBy :: (a -> a -> Ordering) -> [a] -> [a]
    (d, vert) = head (sortBy comp l)
    comp (a, _) (a', _) = compare a a'
    in vert

relax :: Label -> Vertex -> (Distance, Previous) -> (Distance, Previous)
relax u (Vertex label weight) (dist, prev) = let
    alt = dist ! u + weight
    oldDistToVertex = dist ! label
    in if alt < oldDistToVertex 
        then (dist // [(label, alt)], prev // [(label, Just u)])
        else (dist, prev)
