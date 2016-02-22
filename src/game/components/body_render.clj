(ns game.components.body-render
  (:require [game.debug-settings :as debug]
            [game.utils.geom :as geom]
            [engine.render :as color])
  (:use utils.core
        [game.utils.tilemap :only (screenpos-of-tilepos)]
        (engine core input render)
        (game settings mouseoverbody)
        (game.components core body destructible render ingame-loop)
        (game.components.skills core melee)
        game.utils.lightning)
  (:import (org.newdawn.slick Color Image)))

(defn- melee-debug-info [g p body]
  (when-let [meleecomp (get-skill body :melee)]
    (let [target-body (get-entity (:target-body-id meleecomp))
          start-attack-puffer (in-pixel (+ start-melee-puffer (get-half-width body)))
          hit-puffer (in-pixel (+ melee-puffer (get-half-width body)))]
      (when target-body
        (set-color g (if (body-in-melee-range? body target-body false) color/red color/green))
        (draw-shape g (geom/circle p start-attack-puffer))
        (draw-shape g (geom/circle p hit-puffer))))))

(defn- render-body-debug-info [g [x y] body]
  (when (and @debug-mode debug/show-body-debug-info)
    ;(render-readable-text g x y :shift false (str (get-id body)))
    ;(melee-debug-info g [x y] body)
;    (when-let [c (get-skill body :melee)]
;      (render-readable-text g x y :shift false (str (:state c))))
    (when-let [rangedcomp (get-skill body :ranged)]
      (render-readable-text g x y :shift false (str (:state rangedcomp))))))

(defn- render-body-bounds [g body [x y] color]
  (let [h-width (get-half-pxw body)
        h-height (get-half-pxh body)
        x (- x h-width)
        y (- y h-height)]
    (draw-rect g x y (* 2 h-width) (* 2 h-height) color)))

(def- hpbar-colors
  {:green     (rgbcolor :g 1 :darker 0.2)
   :darkgreen (rgbcolor :g 1 :darker 0.5)
   :yellow    (rgbcolor :r 1 :g 1 :darker 0.5)
   :red       (rgbcolor :r 1 :darker 0.5)})

(defn- get-color-for-ratio [ratio]
  (let [ratio (float ratio)
        color (cond
                (> ratio 0.75) :green
                (> ratio 0.5)  :darkgreen
                (> ratio 0.25) :yellow
                :else          :red)]
    (color hpbar-colors)))

(defn render-bar
  ([g x y w h ratio color]
    (fill-rect g x y w h color/black)
    (fill-rect g (inc x) (inc y) (* ratio (- w 2)) (- h 2) color))
  ([g x y w h current-val max-val color]
    (render-bar g x y w h (get-ratio current-val max-val) color)))

(def- body-info-bars-h 3)

(defn- render-body-info-bar [g body [x y] ratio color ybuffer]
  (let [half-w (int (get-half-pxw body))
        half-h (int (get-half-pxh body))
        h body-info-bars-h]
    (render-bar g (- x half-w) (- y half-h ybuffer) (* 2 half-w) h ratio color)))

(defn- render-hp-bar [g body render-position]
  (let [hp (get-hp body)
        ratio (get-ratio hp)
        color (get-color-for-ratio ratio)
        color (if @active-lightning
                (.darker ^Color color
                  (let [image (or
                                (:image (get-component body :image-render))
                                (get-frame (current-animation body)))
                        intensity (let [color (.getCornerColor ^Image image Image/TOP_LEFT)]
                                    (/ (+ (.r color) (.g color) (.b color)) 3))]
                    (- 1 intensity)))
                color)]
    (render-body-info-bar g body render-position ratio color body-info-bars-h)))

(defn- render-attacking-bar [g body skill render-position]
  (render-body-info-bar g body render-position (ratio (:attack-counter skill)) color/blue (* 2 body-info-bars-h)))

(defn body-renderfn [g body component {x 0 y 1 :as render-position}]
  (let [tile-posi (get-position body)]
    (when @debug-mode
      (when debug/show-body-bounds
        (render-body-bounds g body render-position color/white))
      (render-body-debug-info g render-position body))
    (when (not (is-player? body))
      (when (destructible? body)
        (render-hp-bar g body render-position))
      (let [skillmanager (get-component body :skillmanager)
            active-type (:active-type skillmanager)]
        (when (and skillmanager
                   (#{:healing :monster-nova} active-type)
                   (is-attacking? skillmanager))
          (render-attacking-bar g body (get-active-skill skillmanager) render-position)))
      (when-let [component (find-first #(and (:show-cast-bar %) (is-attacking? %)) (get-components body))]
        (render-attacking-bar g body component render-position)))))

(intern 'game.components.body 'body-info-renderfn body-renderfn)

;;;


(defpreload ^:private outlines {:green {:topleft (create-image "outline/tl.png" :scale 0.5)
                                        :topright (create-image "outline/tr.png" :scale 0.5)
                                        :bottomleft (create-image "outline/bl.png" :scale 0.5)
                                        :bottomright (create-image "outline/br.png" :scale 0.5)}
                                :red {:topleft (create-image "outline/tl_r.png" :scale 0.5)
                                      :topright (create-image "outline/tr_r.png" :scale 0.5)
                                      :bottomleft (create-image "outline/bl_r.png" :scale 0.5)
                                      :bottomright (create-image "outline/br_r.png" :scale 0.5)}})

(def body-outline-height 2)

; The corner is at pixel x4 y4 where the body outline corner is
(defn- draw-outline-shape [image x y]
  (draw-image image (- x 2) (- y 2)))

(defn- render-body-outline [g body [x y] color]
  (let [h-width (get-half-pxw body)
        h-height (get-half-pxh body)
        leftx (- x h-width)
        rightx (+ x h-width)
        topy (- y h-height)
        bottomy (+ y h-height)]
    (draw-outline-shape (:topleft (color outlines)) leftx topy)
    (draw-outline-shape (:topright (color outlines)) rightx topy)
    (draw-outline-shape (:bottomleft (color outlines)) leftx bottomy)
    (draw-outline-shape (:bottomright (color outlines)) rightx bottomy)))

(ingame-loop-comp :body-outline
  (rendering :below-gui  [g c]
    (when-let [body (get-mouseover-body)]
      (when (:mouseover-outline (get-component body :body))
        (let [p (screenpos-of-tilepos (get-position body))]
          (render-body-outline g body p
                               (if (and (not (is-player? body))
                                        (destructible? body))
                                 :red
                                 :green)))))))


; OR: current rendered image of body
; => make outline image => draw over it!? .. costs 20-30 ms to make that img would neet to cache it..!!

