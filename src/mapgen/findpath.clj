(ns mapgen.findpath
  (:use [data.grid2d :only (width height)])
  (:import (org.newdawn.slick.util.pathfinding TileBasedMap AStarPathFinder Mover Path)))

(defn- get-pathposi-seq [^Path thepath]
  (map
    #(let [step (.getStep thepath %)]
       [(.getX step)
        (.getY step)])
    (range (.getLength thepath))))

(defn find-path [start target grid pathblocked?]
  {:pre [(not (pathblocked? grid start))
         (not (pathblocked? grid target))]}
  (let [tilebasedmap (reify TileBasedMap
                       (getWidthInTiles [this] (width grid))
                       (getHeightInTiles [this] (height grid))
                       (pathFinderVisited [this x y] false)
                       (getCost [this context tx ty] (float 1))
                       (blocked [this context tx ty] (boolean (pathblocked? grid [tx ty]))))
        astarpathfinder (let [maxsearchdist (* (width grid) (height grid))
                              allowdiagmovement false]
                          (proxy [AStarPathFinder] [tilebasedmap maxsearchdist allowdiagmovement]))

        mover (reify Mover)
        [sx sy] start
        [tx ty] target]
    (get-pathposi-seq (.findPath astarpathfinder mover sx sy tx ty))))

