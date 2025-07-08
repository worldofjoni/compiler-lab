module Compile.Dataflow.Coloring (interferenceGraph, color, Graph, Coloring, Color) where

import Compile.Dataflow.Liveness
import Compile.IR (BBFunc (funcBlocks), lines)
import Data.List (sort)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, fromJust, mapMaybe)
import Data.PSQueue (Binding ((:->)))
import qualified Data.PSQueue as PSQ
import qualified Data.Set as Set

type Graph t = Map.Map t [t]

type Coloring t = Map.Map t Color

type Color = Int

interferenceGraph :: (Ord t) => LivenessFunc t -> Graph t
interferenceGraph lFunc = Set.toList <$> foldr addNeighbors emptyGraph adjacencies
  where
    emptyGraph = Map.empty
    interferences = map snd . concatMap Compile.IR.lines $ Map.elems (funcBlocks lFunc)
    adjacencies = [(x, Set.delete x xs) | xs <- interferences, x <- Set.toList xs]
    addNeighbors :: (Ord t) => (t, Set.Set t) -> Map.Map t (Set.Set t) -> Map.Map t (Set.Set t)
    addNeighbors = uncurry $ Map.insertWith Set.union

color :: (Ord t) => Graph t -> Coloring t
color g = greedyColor g (simplicialElimiationOrder g)

greedyColor :: (Ord t) => Graph t -> [t] -> Coloring t
greedyColor g = foldl (\c u -> Map.insert u (smallestFreeColor c u) c) Map.empty
  where
    neighborhoodColors c u = mapMaybe (`Map.lookup` c) $ neighbors u g
    smallestFreeColor c u = smallestNotIn $ neighborhoodColors c u

simplicialElimiationOrder :: (Ord t) => Graph t -> [t]
simplicialElimiationOrder g = maximumCardinalitySearch g pq
  where
    pq = PSQ.fromList [v :-> 0 | v <- Map.keys g]

maximumCardinalitySearch :: (Ord t) => Graph t -> PSQ.PSQ t Int -> [t]
-- since PSQ is a min prio queue we store weights w as (-w)
maximumCardinalitySearch g pq =
  case PSQ.findMin pq of
    Nothing -> []
    (Just (v :-> _)) -> v : maximumCardinalitySearch g pq'
      where
        pq' = adjustAll (subtract 1) (neighbors v g) . PSQ.deleteMin $ pq

adjustAll :: (Ord p, Ord k) => (p -> p) -> [k] -> PSQ.PSQ k p -> PSQ.PSQ k p
adjustAll f = chain . map (PSQ.adjust f)
  where
    chain = foldl (.) id :: [a -> a] -> a -> a

smallestNotIn :: [Color] -> Color
smallestNotIn = foldl (\acc c -> if c == acc then acc + 1 else acc) 1 . sort

neighbors :: (Ord t) => t -> Graph t -> [t]
neighbors v g = fromJust $ Map.lookup v g
