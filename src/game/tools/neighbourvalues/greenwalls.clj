(ns game.tools.neighbourvalues.greenwalls
  (:use data.grid2d
        utils.core
        (engine core render)
        game.utils.random
        (game.tools transitiontilemaker tiledmap-grid-convert)))

; needs a grid scaled x2 at least and nil cells behind other walls; also remove NADS & single cells

(def- greenwalls-firstgid 1)

(def- neighbourvalues
  {:topandleft 0
   :top 1
   :topandright 2
   :bottomright 3
   :bottomleft 4
   :left 6
   :right 8
   :bottom 13
   :topright 9
   :topleft 10
   :leftandbottom 12
   :rightandbottom 14})

(def- walkable-lid 16)
(def- walkableopt-lids [5 11 15 17])

(defn- calc-localid [grid posi value]
  (case value
    :undefined nil
    :ground (if-chance 10 (rand-nth walkableopt-lids) walkable-lid)
    :wall (get-transition-tile-value posi grid
            :neighbourvalues neighbourvalues
            :default-value -1   ; already set to :undefined by postprocessing...
            :count-neighbour? (fn [posi value] (= :ground value)))))

(defn- calc-gid [grid posi value]
  (when-let [localid (calc-localid grid posi value)]
    (+ greenwalls-firstgid localid)))

(let [spritesheet-width 6]
  (defn calc-sprite-posi [grid posi value]
    (when-let [localid (calc-localid grid posi value)]
      (convert-to-spriteposi localid spritesheet-width))))

(defpreload tilesheet (spritesheet "maps/test/greenwalls.png" 48 48))

;(use '(mapgen nad cave utils cellular))
;(start-slick-basicgame
;  :init (fn [container]
;          (let [grid (cellular-automata-gridgen 50 50
;                                                :fillprob 64
;                                                :random (java.util.Random.)
;                                                :generations 5
;                                                :wall-borders true)
;                grid (-> grid connect-regions fix-not-allowed-diagonals
;                       ;fill-single-cells
;                       ;(scalegrid 2)
;                       ;undefined-value-behind-walls
;                       )]
;            (make-tiledmap-file 48
;                                (resource "cave_walls.tmx")
;                                grid
;                                calc-gid
;                                [greenwalls-firstgid "greenwalls.tsx"]))
;          (System/exit 0)))



