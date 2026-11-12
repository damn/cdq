(ns mapgen.populate
  (:require
    [mapgen.utils :refer [wall-at?]]
    [cdq.core :refer [height width]]
    [game.utils.random :refer [high-weighted-rand-nth]]
    [mapgen.cellular :refer [flood-fill]])
  (:import (org.newdawn.slick.util.pathfinding TileBasedMap AStarPathFinder Mover Path)))

;;; findpath (inlined)

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


;;; populate

(defn get-rand-end-posi [grid start]
  (let [[_ _ labeled-ordered] (flood-fill grid start)]
    ; rest because start-posi should not marked as end-posi
    (rand-nth (high-weighted-rand-nth (rest labeled-ordered)))))

; TODO beim pfad genau nebeneinander die stuff-posis..
(defn get-populated-grid-posis [grid start-posi no-stuff]
  (let [end-posi (get-rand-end-posi grid start-posi)
        path-posis (set ; for checking.. if contains?
                     (find-path start-posi end-posi grid wall-at?))
        [_ _ labeled-ordered] (flood-fill grid path-posis)
        stuff-posis (loop [labeled-ordered (rest labeled-ordered) ; dont put stuff directly on the path (first element)
                           stuff-cnt no-stuff
                           stuff-posis []]
                      (if (or (zero? stuff-cnt) (empty? labeled-ordered))
                        stuff-posis
                        (let [stuff-position (rand-nth (high-weighted-rand-nth labeled-ordered))
                              [_ posis _] (flood-fill grid stuff-position :steps 24) ; TODO flood-fill with 8 instead of 4 neighbours?
                              need-to-be-removed (set posis)
                              labeled-ordered (remove empty? (map (fn [posis]
                                                                    (remove #(contains? need-to-be-removed %) posis))
                                                                  labeled-ordered))]
                          (when (contains? path-posis stuff-position)
                            ; triggered at the end of a loop!
                            (println "stuff-position on path-posi - should not be !!"))
                          (recur labeled-ordered
                                 (dec stuff-cnt)
                                 (conj stuff-posis stuff-position)))))]
    {:end end-posi
     :path-posis path-posis
     :stuff-posis stuff-posis}))
