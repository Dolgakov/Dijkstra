module Dijkstra (
    Vertex,
    fromLists,
    dijkstra
    ) where

import Data.Vector ((!), (//), Vector, foldr', fromList, ifoldr')
import qualified Data.Vector as V
import qualified Data.Set as S
import Data.List (sortBy)

data Vertex = Vertex {
    label :: Int,
    weight :: Float
}

type AdjList = Vector (Vector Vertex)
type Distance = Vector Float
inf = 1 / 0
type Label = Int

-- [[(Label, Weight)]] -> AdjList
fromLists :: [[(Int, Float)]] -> AdjList
fromLists xs = fromList (fmap fromList ((fmap . fmap) (\(l,w) -> Vertex l w) xs))

dijkstra :: AdjList -> Label -> Int -> Distance
dijkstra adj source vertNum = let
    vertSet = S.fromList [0 .. fromIntegral (vertNum - 1)] 
    dist = V.replicate vertNum inf
    dist' = dist // [(source, 0)]
    in loop vertSet adj dist'

loop :: S.Set Label -> AdjList -> Distance -> Distance
loop set adj dist
    | S.null set = dist
    | otherwise = let
        u = inSetMinDist set dist
        set' = S.delete u set
        neighbors = adj ! u
        dist' = foldr' (\vertex distTemp -> relax u vertex distTemp) dist neighbors
        in loop set' adj dist'

inSetMinDist :: S.Set Label -> Distance -> Label
inSetMinDist s dist = let
    l = [(dist ! i, i) | i <- (S.toList s)]
    -- sortBy :: (a -> a -> Ordering) -> [a] -> [a]
    (d, vert) = head (sortBy comp l)
    comp (a, _) (a', _) = compare a a'
    in vert

relax :: Label -> Vertex -> Distance -> Distance
relax u (Vertex label weight) dist = let
    alt = dist ! u + weight
    oldDistToVertex = dist ! label
    in if alt < oldDistToVertex 
        then dist // [(label, alt)]
        else dist
