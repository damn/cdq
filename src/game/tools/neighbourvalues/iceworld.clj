(ns game.tools.neighbourvalues.iceworld
  (:use data.grid2d
        utils.core
        (engine core render)
        game.utils.random
        (game.tools transitiontilemaker tiledmap-grid-convert)))

; needs a grid scaled x2 at least and nil cells behind other walls; also remove NADS & single cells

; potential field does not reach monsters on nearby islands seperated with water
; -> they just stand there and can be killed easily
; even if they have a path to player they are likely to lose against teleport&ranged island hopping

(def- iceworld-firstgid 1)

(def- neighbourvalues
  {:topandleft 3
   :top 11
   :topandright 4
   :bottomright 0
   :bottomleft 2
   :left 7
   :right 5
   :bottom 1
   :topright 10
   :topleft 12
   :leftandbottom 8
   :rightandbottom 9})

(def- fullwall-localid 13)
(def- walkable-localid 6)

(defn- calc-localid [grid posi value]
  (case value
    :undefined fullwall-localid
    :ground walkable-localid
    :wall (get-transition-tile-value
            posi
            grid
            :neighbourvalues neighbourvalues
            :default-value fullwall-localid
            :count-neighbour? (fn [p value] (= :ground value)))))

(defn- calc-gid [grid posi value]
  (+ iceworld-firstgid
     (calc-localid grid posi value)))

(let [spritesheet-width 5]
  (defn calc-sprite-posi [grid posi value]
    (convert-to-spriteposi (calc-localid grid posi value) spritesheet-width)))

(defpreload tilesheet (spritesheet "maps/iceworld.png" 48 48))

(def blockprops
  {:undefined #{:ground}
   :wall #{:ground}
   :ground #{}})

(use '(mapgen nad cave utils))

(comment
  (make-tiledmap-file 48
                      (resource "cave_iceworld.tmx")
                      (-> (cave-gridgen (java.util.Random.) 25 5000 :wide)
                          :grid
                          fix-not-allowed-diagonals
                          fill-single-cells
                          (scalegrid 2)
                          undefined-value-behind-walls)
                      calc-gid
                      [iceworld-firstgid "iceworld.tsx"])
  (println "SUCCESS")
  (System/exit 0))



; 16x16 TEST

;(defpreload tilesheet (spritesheet "maps/test/dirt16x16likeiceworldblocks.png" 16 16))
;(use '(mapgen nad cave utils))
;(do
;  (make-tiledmap-file 16
;                      (resource "cave_dirt.tmx")
;                      (-> (cave-gridgen (java.util.Random.) 25 5000 :wide)
;                        :grid
;                        fix-not-allowed-diagonals
;                        fill-single-cells
;                        (scalegrid 2)
;                        undefined-value-behind-walls)
;                      calc-gid
;                      [iceworld-firstgid "dirt.tsx"])
;  (println "SUCCESS")
;  (System/exit 0))

