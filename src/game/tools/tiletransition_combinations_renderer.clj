(ns game.tools.tiletransition-combinations-renderer
  (:require [engine.render :as color])
  (:use (engine core render)
        [data.grid2d :only (get-8-neighbour-positions)]
        [utils.core :only (mapvals boolperm)]))

(def posis (get-8-neighbour-positions [0 0]))

(def all-combinations (map #(zipmap posis %) (boolperm 8)))

(def four-adjacent-ground {[0 -1] false [0 1] false [-1 0] false [1 0] false})
(def topright-nads {[0 -1] false [1 -1] true [1 0] false})
(def topleft-nads {[-1 -1] true [0 -1] false [-1 0] false})
(def bottomright-nads {[1 1] true [1 0] false [0 1] false})
(def bottomleft-nads {[-1 1] true [-1 0] false [0 1] false})

; scaled x2
; jeder weisse der NICHT diagonal ist
; braucht einen nachbarn mindestens er kann also nicht alleine stehen
; -> für jeden der false ist schaue:
; falls diagonal okay


(defn checkit [combination check]
  (every? #(= (get combination (% 0)) (% 1)) check))

(def not-allowed-singles (filter #(checkit % four-adjacent-ground) all-combinations))
(def bottomleft-nads-combinations (filter #(checkit % bottomleft-nads) all-combinations))

; 96
(def all-not-allowed-combinations (filter #(or
                                             (checkit % four-adjacent-ground)
                                             (checkit % topright-nads)
                                             (checkit % topleft-nads)
                                             (checkit % bottomright-nads)
                                             (checkit % bottomleft-nads)) all-combinations))

; 160
(def all-allowed-combinations (remove #(or
                                         (checkit % four-adjacent-ground)
                                         (checkit % topright-nads)
                                         (checkit % topleft-nads)
                                         (checkit % bottomright-nads)
                                         (checkit % bottomleft-nads)) all-combinations))

(def combinations all-allowed-combinations)

(def scale 13)

(defn render-combination [g sx sy c]
  (fill-rect g (+ sx scale) (+ sy scale) scale scale color/black)
  (doseq [[[posx posy] v] (map #(vector (mapv inc (% 0)) (% 1)) c)]
    (fill-rect g
               (+ sx (* posx scale))
               (+ sy (* posy scale))
               scale
               scale
               (if v color/black color/white))))

(def screenw 800)

(defn render [g]
  (let [sx 5
        sy 12
        x (atom 0)
        y (atom 0)
        step (* scale 4)]
    (dorun
      (map-indexed
        (fn [idx combination]
          (let [xrender (+ sx (* @x step))
                yrender (+ sy (* @y step))]
            (render-readable-text g xrender (- yrender 15) :background false color/black idx)
            (render-combination g xrender yrender combination))
          (if (> (+ sx (* (+ @x 2) step)) screenw)
            (do (reset! x 0) (swap! y inc))
            (swap! x inc)))
        combinations))))

(start-slick-basicgame
  :width screenw
  :height 600
  :render (fn [container g]
            (fill-rect g 0 0 800 600 color/gray)
            (render g)))






