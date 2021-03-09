{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Lens
import Data.Graph.Inductive.Graph hiding ((&))
import qualified Data.Graph.Inductive.Graph as G ((&))
import Data.Graph.Inductive.PatriciaTree
import Data.List (find, findIndex)
import Data.List.Extra (elemIndex)
import Data.Vector (Vector, (!), (//))
import qualified Data.Vector as V
import Graphics.Gloss hiding (Vector)
import Graphics.Gloss.Interface.IO.Game hiding (Vector)
import Numeric.AD
import Numeric.Optimization.Algorithms.HagerZhang05.AD
import System.IO.Unsafe (unsafePerformIO)

-- Math Stuff

type Linkage = Gr Bool Float

type Positions a = Vector a

type P2 a = (a, a)

numNodes :: (Graph gr) => gr a b -> Int
numNodes = noNodes

numEdges :: (Graph gr) => gr a b -> Int
numEdges = size

μ :: (Num a) => P2 a -> P2 a -> a
μ (x0, y0) (x1, y1) = (x0 - x1) ^ 2 + (y0 - y1) ^ 2

nodePos :: Node -> Positions a -> P2 a
nodePos i ps = (ps ! (2 * i), ps ! (2 * i + 1))

setNodePos :: Node -> Positions a -> P2 a -> Positions a
setNodePos i ps (x, y) = ps // [(2 * i, x), (2 * i + 1, y)]

np :: Node -> Lens' (Positions a) (P2 a)
np i = lens (nodePos i) (setNodePos i)

data OverlayScheme = OverlayScheme
  { oldPos :: Positions Float,
    posMap :: Vector (Maybe Int)
  }

makeOverlayScheme :: Linkage -> Positions Float -> (OverlayScheme, Vector Float)
makeOverlayScheme l ps = (OverlayScheme ps pm, x)
  where
    n = numNodes l
    idxs = fst <$> filter (not . snd) (labNodes l)
    x = V.fromList $ do
      i <- idxs
      let (p1, p2) = nodePos i ps
      [p1, p2]
    pm = V.fromList $ map (`elemIndex` idxs) [0 .. n -1]

buildPositions :: OverlayScheme -> Vector Float -> Positions Float
buildPositions os x = V.fromList $ do
  let n = V.length (oldPos os) `div` 2
  i <- [0 .. n - 1]
  let (p1, p2) = nodePos' os i x
  [p1, p2]

nodePos' :: (RealFloat a) => OverlayScheme -> Node -> Vector a -> P2 a
nodePos' os i x = case posMap os ! i of
  Just j -> nodePos j x
  Nothing -> over both realToFrac $ nodePos i (oldPos os)

distToNode :: (RealFloat a) => Node -> Point -> OverlayScheme -> Vector a -> a
distToNode i p os x = μ (over both realToFrac p) (nodePos' os i x)

edgeConstraint :: (RealFloat a) => OverlayScheme -> LEdge Float -> Vector a -> a
edgeConstraint os (n, m, d) x = μ (nodePos' os n x) (nodePos' os m x) - realToFrac (d ^ 2)

edgeConstraints :: (RealFloat a) => OverlayScheme -> Linkage -> [Vector a -> a]
edgeConstraints os = map (edgeConstraint os) . labEdges

(·) :: (Num a) => Vector a -> Vector a -> a
v · w = V.foldl' (+) 0 $ V.zipWith (*) v w

magnitude :: (Num a) => Vector a -> a
magnitude v = v · v

lagrange :: (RealFloat a) => Node -> Point -> Linkage -> OverlayScheme -> Vector a -> a
lagrange n p l os x = distToNode n p os x + (cvals · λs)
  where
    cvals = V.fromList $ map ($ x) (edgeConstraints os l)
    λs = V.slice (V.length x - numEdges l) (numEdges l) x

objective :: (RealFloat a) => Node -> Point -> Linkage -> OverlayScheme -> Vector a -> a
objective n p l os = magnitude . grad (lagrange n p l os)

sensibleOptimize :: Traversable f => (forall a. RealFloat a => f a -> a) -> f Double -> f Double
sensibleOptimize g init = unsafePerformIO $ do
  (found, _, _) <- optimize (defaultParameters {printFinal = False}) 0.01 init g
  return found

adjustLinkage :: Node -> Point -> Linkage -> Positions Float -> Positions Float
adjustLinkage n p l ps = buildPositions os $ realToFrac <$> opt init
  where
    (os, initMovPs) = makeOverlayScheme l ps
    opt = sensibleOptimize (objective n p l os)
    init = fmap realToFrac initMovPs V.++ V.replicate (numEdges l) 0

-- Graphics Stuff

data World = World
  { _graph :: Linkage,
    _clicked :: Maybe Node,
    _pos :: Positions Float
  }

makeLenses ''World

radius = 40

drawNode :: Positions Float -> Node -> Picture
drawNode ps i = translate x y (circle radius)
  where
    (x, y) = ps ^. np i

drawEdge :: Positions Float -> Edge -> Picture
drawEdge ps (x, y) = line [ps ^. np x, ps ^. np y]

drawWorld :: World -> Picture
drawWorld w = pictures $ (drawNode ps <$> nodes g) ++ (drawEdge ps <$> edges g)
  where
    ps = w ^. pos
    g = w ^. graph

updateWorld :: Event -> World -> World
updateWorld (EventKey (MouseButton LeftButton) Down _ p) w =
  w & clicked .~ foundNode
  where
    underCursor = (< radius ^ 2) . μ p . (w ^.) . (pos .) . np
    foundNode = find underCursor $ nodes (w ^. graph)
updateWorld (EventKey (MouseButton LeftButton) Up _ p) w = w & clicked .~ Nothing
updateWorld (EventMotion p) w = case w ^. clicked of
  Just node -> w & pos %~ adjustLinkage node p (w ^. graph)
  Nothing -> w
updateWorld _ w = w

stepWorld :: Float -> World -> World
stepWorld = const id

quadWorld :: World
quadWorld =
  World
    { _graph = mkGraph [(0, True), (1, True), (2, False), (3, False)] [(0, 1, 300), (1, 2, 200), (2, 3, 300), (3, 0, 150)],
      _clicked = Nothing,
      _pos = V.fromList [0, 0, 0, 300, 200, 0, 0, 0]
    }

peaucellierWorld =
  World
    { _graph =
        mkGraph
          [(0, True), (1, True), (2, False), (3, False), (4, False), (5, False)]
          [(1, 2, 100), (2, 5, 100), (2, 4, 100), (5, 3, 100), (4, 3, 100), (0, 5, 280), (0, 4, 280)],
      _clicked = Nothing,
      _pos = V.fromList [0, 0, 100, 0, 200, 0, 342, 0, 271, 50, 271, -50]
    }

main :: IO ()
main = play (InWindow "Linkage" (400, 400) (0, 0)) white 5 peaucellierWorld drawWorld updateWorld stepWorld
