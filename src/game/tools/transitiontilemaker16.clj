(ns game.tools.transitiontilemaker16
  (:use data.grid2d
        utils.core
        (engine core render)
        game.utils.random
        game.tools.tiledmap-grid-convert))

(let [idxvalues-order [[1 0] [-1 0] [0 1] [0 -1]]]
  (assert (= (get-4-neighbour-positions [0 0]) idxvalues-order)))

(comment
  ; Values for every neighbour:
  {          [0 -1] 1
   [-1 0]  8          [1 0] 2
             [0 1] 4 })

; so the idxvalues-order corresponds to the following values for a neighbour tile:
(def- idxvalues [2 8 4 1])

(defn- get-transition-tile-idxvalue [count-neighbour? grid posi]
  (apply +
         (map-indexed (fn [idx p] (if (count-neighbour? p (get grid p))
                                    (get idxvalues idx)
                                    0))
                      (get-4-neighbour-positions posi))))

; => gives value between 0 and 15
; => sprite-idx for every value is defined in a map
; => count :walls

(def- idxvalue-localids
  {0 0,
   1 22,
   2 7,
   3 23,
   4 1,
   5 15,
   6 16,
   7 21,
   8 10,
   9 24,
   10 9,
   11 2,
   12 17,
   13 14,
   14 3,
   15 8})

(defn- calc-localid [grid posi value]
  (case value
    :undefined nil
    :ground 43
    :wall (let [idxvalue (get-transition-tile-idxvalue (fn [posi value] (= :wall value)) grid posi)]
            (assert ((set (keys idxvalue-localids)) idxvalue))
            (get idxvalue-localids idxvalue))))

(let [spritesheet-width 7]

  (defn calc-sprite-posi [grid posi value]
    (when-let [localid (calc-localid grid posi value)]
      (convert-to-spriteposi localid spritesheet-width))))

;;

(def- firstgid 1)

(defn- calc-gid [grid posi value]
  (when-let [localid (calc-localid grid posi value)]
    (+ firstgid localid)))

;(use '(mapgen nad cave utils cellular))
;(start-slick-basicgame
;  :init (fn [container]
;          (let [grid (cellular-automata-gridgen 100
;                                                100
;                                                :fillprob 64
;                                                :random (java.util.Random.)
;                                                :generations 5
;                                                :wall-borders true)
;                grid (-> grid connect-regions fix-not-allowed-diagonals undefined-value-behind-walls)]
;            (make-tiledmap-file 16
;                                (resource "gauntletly.tmx")
;                                grid
;                                calc-gid
;                                [firstgid "gauntletly.tsx"]))
;          (println "SUCCESS!")
;          (System/exit 0)))

