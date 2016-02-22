(ns mapgen.cave
  (:require [data.grid2d :as grid]
            [utils.core :refer (assoc-ks)]
            [game.utils.random :as rand]))

;Cave Algorithmus.
;http://properundead.com/2009/03/cave-generator.html
;http://properundead.com/2009/07/procedural-generation-3-cave-source.html
;http://forums.tigsource.com/index.php?topic=5174.0

(defn- create-order [random]
  (rand/sshuffle (range 4) random))

(defn- get-in-order [v order]
  (map #(get v %) order))

(def ^:private current-order (atom nil))

(def ^:private turn-ratio 0.5)

(defn- create-rand-4-neighbour-posis [posi n random] ; TODO does more than 1 thing
  (when (< (rand/srand random) turn-ratio)
    (reset! current-order (create-order random)))
  (take n
        (get-in-order (grid/get-4-neighbour-positions posi) @current-order)))

(defn- get-default-adj-num [open-paths random]
  (if (= open-paths 1)
    (case (int (rand/srand-int 4 random))
      0 1
      1 1
      2 1
      3 2
      1)
    (case (int (rand/srand-int 4 random))
      0 0
      1 1
      2 1
      3 2
      1)))

(defn- get-thin-adj-num [open-paths random]
  (if (= open-paths 1)
    1
    (case (int (rand/srand-int 7 random))
      0 0
      1 2
      1)))

(defn- get-wide-adj-num [open-paths random]
  (if (= open-paths 1)
    (case (int (rand/srand-int 3 random))
      0 1
      2)
    (case (int (rand/srand-int 4 random))
      0 1
      1 2
      2 3
      3 4
      1)))

(def ^:private get-adj-num
  {:wide get-wide-adj-num
   :thin get-thin-adj-num    ; h�hle mit breite 1 �berall nur -> turn-ratio verringern besser
   :default get-default-adj-num}) ; etwas breiter als 1 aber immernoch zu d�nn f�r m ein game -> turn-ratio verringern besser

; gute ergebnisse: :wide / 500-4000 max-cells / turn-ratio 0.5
; besser 150x150 anstatt 100x100 w h
; TODO glaubich einziger unterschied noch: openpaths wird bei jeder cell neu berechnet?
; TODO max-tries wenn er nie �ber min-cells kommt? -> im let dazu definieren vlt max 30 sekunden -> in tries umgerechnet??
; use defnks
(defn cave-gridgen [random min-cells max-cells adjnum-type]
  (reset! current-order (create-order random))
  (let [start-posi [0 0]
        start-grid (assoc {} start-posi :ground) ; grid of posis to :ground or no entry for walls
        finished (fn [grid end-posi cell-cnt]
                   ;(println "Reached cells: " cell-cnt) ; TODO cell-cnt stimmt net genau
                   (if (< cell-cnt min-cells)
                     (cave-gridgen random min-cells max-cells adjnum-type) ; recur?
                     (let [[grid convert] (grid/mapgrid->vectorgrid grid #(if (nil? %) :wall :ground))]
                       {:grid  grid
                        :start (convert start-posi)
                        :end   (convert end-posi)})))]
    (loop [posi-seq [start-posi]
           grid start-grid
           cell-cnt 0]
      (if (>= cell-cnt max-cells)
        (finished grid (last posi-seq) cell-cnt)
        (let [try-carve-posis (create-rand-4-neighbour-posis
                                (last posi-seq)
                                ((get-adj-num adjnum-type) (count posi-seq) random)
                                random)
              carve-posis (filter #(nil? (get grid %)) try-carve-posis)
              new-pos-seq (concat (drop-last posi-seq) carve-posis)]
          (if (not-empty new-pos-seq)
            (recur
              new-pos-seq
              (if (seq carve-posis)
                (assoc-ks grid carve-posis :ground)
                grid)
              (+ cell-cnt (count carve-posis)))
            (finished grid (last posi-seq) cell-cnt)))))))
