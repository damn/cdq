(ns game.utils.raycast
  (:require [engine.render :as color])
  (:use
    utils.core
    (engine input render)
    (game settings)
    (game.components core)
    (game.utils geom tilemap)
    (game.maps cell-grid data))
  (:import game.utils.RayCaster))

; mit jcell-grid weil performanter als andere probierte l�sung: tilebasedmap zugriff auf get-cell bei jedem call to blocked.
; 10x so schnell mit jcell-grid
; Annahme: cell-grid �ndert net w�hrend einer ray-blocked? anfrage; deswegen auch jcell-grid

(defn ray-blocked?
  ([start-x start-y target-x target-y]
    (RayCaster/rayBlocked start-x start-y target-x target-y (get-map-w) (get-map-h) (get-jcell-grid)))
  ([p1 p2]
    (let [[x1 y1] (vec-posi p1)
          [x2 y2] (vec-posi p2)]
      (ray-blocked? x1 y1 x2 y2))))

(defn ray-maxsteps
  [start-x start-y vector-x vector-y max-steps]
  (let [erg (RayCaster/castMaxSteps start-x start-y vector-x vector-y (get-map-w) (get-map-h) (get-jcell-grid) max-steps)]
    (if (= -1 erg) :not-blocked erg)))

(defn- create-double-ray-endpositions
  "path-w in tiles."
  [start-x,start-y,target-x,target-y,path-w]
  {:pre [(< path-w 0.98)]} ; wieso 0.98??
  (let [path-w (+ path-w 0.02) ;etwas gr�sser damit z.b. projektil nicht an ecken anst�sst
        v (direction-vector [start-x start-y] [target-y target-y])
        [normal1 normal2] (get-normal-vectors v)
        normal1 (scale normal1 (/ path-w 2))
        normal2 (scale normal2 (/ path-w 2))
        start1  (add (vector2f start-x  start-y)  normal1)
        start2  (add (vector2f start-x  start-y)  normal2)
        target1 (add (vector2f target-x target-y) normal1)
        target2 (add (vector2f target-x target-y) normal2)]
    [start1,target1,start2,target2]))

(defn is-path-blocked?
  "path-w in tiles. casts two rays."
  ([start-x,start-y,target-x,target-y,path-w]
    (let [[start1,target1,start2,target2] (create-double-ray-endpositions start-x start-y target-x target-y path-w)]
      (is-path-blocked? start1 target1 start2 target2)))
  ([start1,target1,start2,target2]
    (or
      (ray-blocked? start1 target1)
      (ray-blocked? start2 target2))))

(defn- ray-steplist
  [start-x start-y target-x target-y]
  (seq
    (RayCaster/castSteplist start-x start-y target-x target-y (get-map-w) (get-map-h) (get-jcell-grid))))

; STEPLIST TEST

(def current-steplist (atom nil))

(defn steplist-contains? [tilex tiley]
  (some
    (fn [[x y]]
      (and (= x tilex) (= y tiley)))
    @current-steplist))

(defn render-line-middle-to-mouse [g color]
  (set-color g color)
  (let [[x y] (get-mouse-pos)]
    (draw-line g half-screen-w half-screen-h x y)))

(defn update-test-raycast-steplist []
  (let [[start-x start-y] (get-position player-body)
        [target-x target-y] (get-mouse-tile-pos)
        steplist (map
                   (fn [step]
                     [(.x step) (.y step)])
                   (ray-steplist start-x start-y target-x target-y))]
    (reset! current-steplist steplist)))

;; MAXSTEPS TEST

(def current-steps (atom nil))

(defn update-test-raycast-maxsteps []
  (let [[start-x start-y] (get-position player-body)
        [target-x target-y] (get-mouse-tile-pos)
        vector-x (- target-x start-x)
        vector-y (- target-y start-y)
        maxsteps 10
        steps (ray-maxsteps start-x start-y vector-x vector-y maxsteps)]
    (reset! current-steps steps)))

(defn draw-test-raycast [g]
  (let [[start-x start-y] (get-position player-body)
        [target-x target-y] (get-mouse-tile-pos)
        color (if (ray-blocked? start-x start-y target-x target-y)
                color/red
                color/green)]
    (render-line-middle-to-mouse g color)))

; PATH BLOCKED TEST

(defn draw-test-path-blocked [g]
  (let [[start-x start-y] (get-position player-body)
        [target-x target-y] (get-mouse-tile-pos)
        [start1 target1 start2 target2] (mapv vec-posi
                                               (create-double-ray-endpositions start-x start-y target-x target-y 0.4))
        [start1screenx,start1screeny] (screenpos-of-tilepos start1)
        [target1screenx,target1screeny] (screenpos-of-tilepos target1)
        [start2screenx,start2screeny] (screenpos-of-tilepos start2)
        [target2screenx,target2screeny] (screenpos-of-tilepos target2)]
    (set-color g (if (is-path-blocked? start1 target1 start2 target2) color/red color/green))
    (doto g
      (draw-line start1screenx start1screeny target1screenx target1screeny)
      (draw-line start2screenx start2screeny target2screenx target2screeny))))


