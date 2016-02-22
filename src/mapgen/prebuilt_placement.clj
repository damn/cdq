(ns mapgen.prebuilt-placement
  (:require [data.grid2d :as grid])
  (:use [game.utils.geom :only (tiles-inside-rect)]
        [mapgen.utils :only (wall-at?)])
  (:import data.grid2d.VectorGrid))

(defn- is-5x5-walls-space? [center grid]
  (let [tiles (tiles-inside-rect [center 2 2])] ; room half width&height = 2 => 5x5
    (when (every? #(wall-at? grid %) tiles)
      tiles)))

(defn- flipy [data]
  (map #(vec (reverse %)) data))

(defn- rotate-grid-90-degrees [grid]
  (VectorGrid. (vec (reverse (apply map vector (.data ^VectorGrid grid))))))

(defn- rotate-grid-180-degrees [grid]
  (VectorGrid. (vec (reverse (flipy (.data ^VectorGrid grid))))))

(defn- rotate-grid-270-degrees [grid]
  (VectorGrid. (vec (flipy (apply map vector (.data ^VectorGrid grid))))))

(comment
  (let [grid (grid/VectorGrid.[[1 4 7]
                               [2 5 8]
                               [3 6 9]])]
    (grid/print-grid grid :print-cell print)
    (println)
    (grid/print-grid (rotate-grid-90-degrees grid) :print-cell print)
    (println)
    (grid/print-grid (rotate-grid-180-degrees grid) :print-cell print)
    (println)
    (grid/print-grid (rotate-grid-270-degrees grid) :print-cell print)))

(defn is-5x5-walls-entrance?
  "returns false or a vector of [5x5-tiles open-entrance-tile center-tile rotatefn]"
  [[x y] grid]
  (let [a [x (- y 3)]
        b [x (+ y 3)]
        c [(- x 3) y]
        d [(+ x 3) y]]
    (if (wall-at? grid [x y])
      false
      (if-let [tiles (is-5x5-walls-space? a grid)]    ; 1. konfiguration => a => so wie das tiled file => eingang nach oben
        [tiles [x (dec y)] a identity]
        (if-let [tiles (is-5x5-walls-space? b grid)]
          [tiles [x (inc y)] b rotate-grid-180-degrees]
          (if-let [tiles (is-5x5-walls-space? c grid)]
            [tiles [(dec x) y] c rotate-grid-270-degrees]
            (if-let [tiles (is-5x5-walls-space? d grid)]
              [tiles [(inc x) y] d rotate-grid-90-degrees]
              false)))))))
