(ns game.tests.lightning
  (:use
    [utils.core :only (mapvals)]
    [engine.render :only (render-readable-text)]
    (game.components
      [core :only (get-component get-position player-body)])
    (game.utils
      [raycast :only (ray-blocked?)]
      [lightning :only (image-corners)]
      [tilemap :only (get-mouse-tile-pos mouse-int-tile-pos)]))
  (:import org.newdawn.slick.Image))

(defn- get-corner-name [n] ; assert 0 , 1,2,3 = top-left.etc
  (case (int n)
    0 "TOP_LEFT"
    1 "TOP_RIGHT"
    2 "BOTTOM_RIGHT"
    3 "BOTTOM_LEFT"))

(defn get-cache []
  (let [[x y] (mouse-int-tile-pos)
        ipos [(+ x 0.5) (+ y 0.5)]
        m (mapvals
            (get (:lightmap (get-component player-body :light)) ipos)
            #(if % :LIGHT))]
    (zipmap (map get-corner-name (keys m)) (vals m))))

(defn- get-corner-position [x y half-w half-h corner]
  (let [x (float x) y (float y) half-w (float half-w) half-h (float half-h)]
    (cond
      (= corner Image/TOP_LEFT)     [(- x half-w) (- y half-h)]
      (= corner Image/TOP_RIGHT)    [(+ x half-w) (- y half-h)]
      (= corner Image/BOTTOM_LEFT)  [(- x half-w) (+ y half-h)]
      (= corner Image/BOTTOM_RIGHT) [(+ x half-w) (+ y half-h)])))

; (mouse-int-tile-pos) = [x y]
; get-corner-position [x y half-w half-h corner]
; f�r jede ecke des current tile posis zeige posi und den dazugeh�rigen namen untereinander (map-indexed und y+30*i oder font-height)
(defn render-mouse-tile-blocked-corners [g xstart ystart ypuffer]
  (let [[x y] (mouse-int-tile-pos)
        ix (+ x 0.5)
        iy (+ y 0.5)
        half-w 0.5
        half-h 0.5
        light-posi (get-position player-body)]
    (dorun
      (map-indexed
        (fn [idx corner]
          (let [posi-vec (get-corner-position ix iy half-w half-h corner)]
            (render-readable-text g xstart (+ ystart (* idx ypuffer))
              (str (get-corner-name corner) " " posi-vec
                " LIGHT? "
                (not (ray-blocked? light-posi posi-vec))))))
        image-corners))))


