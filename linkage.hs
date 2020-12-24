{-# LANGUAGE TemplateHaskell #-}

module Main where

import Data.List (find)
import Data.Graph.Inductive.Graph hiding ((&))
import qualified Data.Graph.Inductive.Graph as G ((&))
import Data.Graph.Inductive.PatriciaTree
import Data.Vector.Unboxed (Vector, (!), (//))
import qualified Data.Vector.Unboxed as V

import Graphics.Gloss hiding (Vector)
import Graphics.Gloss.Interface.IO.Game hiding (Vector)

import Control.Lens

-- Math Stuff

type Linkage = Gr Bool Float
  
type Positions = Vector Float

μ :: Point -> Point -> Float
μ (x0,y0) (x1,y1) = (x0 - x1)^2 + (y0 - y1)^2
  
pointCoord :: Positions -> Node -> Point
pointCoord ps i = (ps ! (2*i), ps ! (2*i+1))

setPointCoord :: Node -> Point -> Positions -> Positions
setPointCoord i (x,y) ps = ps // [(2*i,x),(2*i+1,y)]

f :: Node -> Point -> Positions -> Float
f node p ps = μ p (pointCoord ps node)

edgeConstraint :: LEdge Float -> Positions -> Float
edgeConstraint (n,m,d) ps = μ (pointCoord ps n) (pointCoord ps m) - d

gs :: Linkage -> [Positions -> Float]
gs = map edgeConstraint . labEdges

(·) :: (Num a, V.Unbox a) => Vector a -> Vector a -> a
v · w = V.foldl' (+) 0 $ V.zipWith (*) v w

lagrange :: Node -> Point -> Linkage -> Positions -> Vector Float -> Float
lagrange n p l ps λs = (f n p ps) + (cvals · λs)
  where cvals = V.fromList $ map ($ ps) (gs l)

-- Graphics Stuff
  
data World = World {
    _graph :: Linkage
  , _clicked :: Maybe Node
  , _pos :: Positions
  }
  
makeLenses ''World

radius = 40

drawNode :: Positions -> Node -> Picture
drawNode ps i = translate x y (circle radius)
  where (x,y) = pointCoord ps i

drawEdge :: Positions -> Edge -> Picture
drawEdge ps (x,y) = line [p,q]
  where
    p = pointCoord ps x
    q = pointCoord ps y

drawWorld :: World -> Picture
drawWorld w = pictures $ (drawNode ps <$> (nodes g)) ++ (drawEdge ps <$> (edges g))
  where
    ps = w ^. pos
    g = w ^. graph


updateWorld :: Event -> World -> World
updateWorld (EventKey (MouseButton LeftButton) Down _ p) w =
  w & clicked .~ foundNode
  where
    underCursor = (< radius^2) . (μ p) . (pointCoord $ w ^. pos)
    foundNode = find underCursor $ nodes (w ^. graph)
updateWorld (EventKey (MouseButton LeftButton) Up _ p) w = w & clicked .~ Nothing
updateWorld (EventMotion p) w = case w ^. clicked of
  Just node -> w & pos %~ (setPointCoord node p)
  Nothing -> w
updateWorld _ w = w

stepWorld :: Float -> World -> World
stepWorld = const id

initWorld :: World
initWorld = World {
    _graph = mkGraph [(0,False),(1,False),(2,False)] [(0,1,0),(1,2,0),(2,0,0)]
  , _clicked = Nothing
  , _pos = V.fromList [0,0,0,0,0,0]
  }

main :: IO ()
main = play (InWindow "Linkage" (400,400) (0,0)) white 20 initWorld drawWorld updateWorld stepWorld
