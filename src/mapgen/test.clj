(ns mapgen.test
  (:require [engine.render :as color])
  (:use data.grid2d
        utils.core
        (engine core input render)
        game.utils.random
        (mapgen cave spawn-spaces cellular nad utils populate))
  (:import java.util.Random))

(def- grid-colors {:wall      (rgbcolor :r 0.2 :g 0.2)
                   :ground    (rgbcolor :r 0.5 :g 0.5)
                   :room      color/white
                   :undefined color/black})

(defn- load-grid-on-image [grid scale]
  (let [image (create-empty-image (* scale (width grid))
                                  (* scale (height grid)))
        g (.getGraphics image)]
    (doseq [[[x y] value] grid]
      (fill-rect g (* scale x) (* scale y) scale scale
                 (if (keyword? value) (grid-colors value) value)))
    (.flush g)
    image))

(def- current-grid (atom nil))

(defn- colorize-nads []
  (let [nads (get-nads @current-grid)]
    (println "found " (count nads) " NADs." )
    (mark-nads @current-grid nads color/red)))

(def- gridw 50)
(def- gridh 50)
(def- scale 6) ; cell pixel-width/height

(def- imagew (* scale gridw))
(def- imageh (* scale gridh))
(def- border 25)
(def- screenw 1000)
(def- screenh 800)

(def- current-seed (atom (create-seed)))

(defn- generate-cellular []
  (let [randseed (reset! current-seed (create-seed))]
    (cellular-automata-gridgen gridw gridh
                               :fillprob 64; (rand-int-between 60 70) ; TODO from randseed!
                               :random (Random. randseed)
                               :generations 5 ; (rand-int-between 3 5) ; TODO from randseed!
                               :wall-borders true)))

(defn- generate-cave []
  (let [randseed (reset! current-seed (create-seed))]
    (:grid (cave-gridgen (Random. randseed) 25 1000 :wide))))

(def- current-image (atom nil))

(defn- update-grid [grid]
  (when @current-image
    (.destroy @current-image))
  (reset! current-image
          (load-grid-on-image (reset! current-grid grid)
                              scale)))

(defn- draw-spawn-field-grid [g x y w h]
  (let [cellw (* scale 13) ; spawn space field size --> als var in spawn space?
        cellh (* scale 10)
        gridw (/ w cellw)
        gridh (/ h cellh)]
    (draw-grid g x y gridw gridh cellw cellh)))

(defn- rand-color []
  (rgbcolor :r (rand)
              :g (rand)
              :b (rand)))

(defn- get-mouse-tile-pos []
  (let [[x y] (get-mouse-pos)
        image-startx (- (/ screenw 2)
                        (/ (.getWidth @current-image) 2))
        image-starty (-
                       (/ screenh 2)
                       (/ (.getHeight @current-image) 2))
        scale (fn [n] (int (/ n scale)))]
    [(scale (- x image-startx))
     (scale (- y image-starty))]))

(defn- flood-fill-grid []
  (let [mouse-tile-pos (get-mouse-tile-pos)
        [grid labeled-posis _] (flood-fill @current-grid mouse-tile-pos :label (rand-color))]
    grid))

(defn- color-flood-fill-heatmap [grid start]
  (let [[_ _ labeled-ordered] (flood-fill grid start)
        agrid (atom grid)
        labeled-ordered (rest labeled-ordered) ; not at start..
        cnt (count labeled-ordered)
        alphastep (/ 1 cnt)]
    (dorun
      (map-indexed (fn [idx posis]
                     (swap! agrid assoc-ks posis (rgbcolor :r 1 :a (* idx alphastep))))
                   labeled-ordered))
    @agrid))

(defn- flood-fill-distances-heatmap []
  (color-flood-fill-heatmap @current-grid (get-mouse-tile-pos)))

(use '[mapgen.findpath :only (find-path)])

(defn- flood-fill-from-start-to-end-path []
  (let [start-posi (get-mouse-tile-pos)
        end-posi (get-rand-end-posi @current-grid start-posi)
        pathposis (find-path start-posi end-posi @current-grid wall-at?)]
    (color-flood-fill-heatmap (swap! current-grid assoc-ks pathposis color/white) pathposis)))

(defn- populate []
  (let [{:keys [end path-posis stuff-posis]} (get-populated-grid-posis @current-grid (get-mouse-tile-pos) 20)]
    (-> @current-grid
      (assoc-ks path-posis color/white)
      (assoc-ks stuff-posis color/red))))

(use 'mapgen.prebuilt-placement)

(defn- show-possible-room-locations []
  (let [[room entrance] (rand-nth
                          (remove false?
                                  (map #(is-5x5-walls-entrance? % @current-grid)
                                       (posis @current-grid))))]
    (-> @current-grid
      (assoc-ks room color/red)
      (assoc-ks [entrance] color/blue)))
;  (transform @current-grid
;                        (fn [posi value]
;                          (if (is-5x5-walls-entrance? posi value @current-grid)
;                            color/red
;                            value)))
  )

(defn- show-random-end-posis "very slow" []
  (let [mouse-tile-pos (get-mouse-tile-pos)
        random-end-posis (repeatedly 100 #(get-rand-end-posi @current-grid mouse-tile-pos))]
    (-> @current-grid
      (assoc mouse-tile-pos color/blue)
      (assoc-ks random-end-posis color/red))))

(defn- colorize-grid [grid]
  (reduce #(assoc-ks %1 %2 (rand-color)) grid (calculate-regions grid)))

(defn- color-regions-grid [] (colorize-grid @current-grid))

(defn- postprocess []
  (-> @current-grid
    connect-regions
    fix-not-allowed-diagonals
    fill-single-cells
    ;undefined-value-behind-walls
    ;(scalegrid 2)
    ))
;    mark-spawn-spaces

(def- key-commands
  `{:A generate-cave
    :B populate
    :C show-possible-room-locations
    :E show-random-end-posis
    :S generate-cellular
    :F flood-fill-grid
    :Z flood-fill-from-start-to-end-path
    :G flood-fill-distances-heatmap
    :R color-regions-grid
    :P postprocess
    :D colorize-nads})

(start-slick-basicgame
  :width screenw
  :height screenh
  :init (fn [container]
          (update-grid (generate-cave)))
  :update (fn [container delta]
            (doseq [[k command] key-commands
                    :let [f @(find-var command)]]
              (when (is-key-pressed? k)
                (update-grid (f)))))
  :render (fn [container g]
            (fill-rect g 0 0 screenw screenh color/darkGray)
            (.drawCentered @current-image (/ screenw 2) (/ screenh 2)) ; TODO
            (set-color g color/white)
            (draw-string g 0 0 (get-mouse-tile-pos))
            ;(draw-spawn-field-grid g (/ border 2) (/ border 2) w h)
            ))

