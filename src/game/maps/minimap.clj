(ns game.maps.minimap
  (:require [engine.render :as color]
            [game.utils.geom :as geom])
  (:use clojure.set
        data.grid2d
        utils.core
        (engine core render)
        game.settings
        game.components.core
        (game.maps data cell-grid camera))
  (:import (org.newdawn.slick Graphics Image)))

(defn- get-image  [] (:minimap-image  (get-current-map-data)))
(defn- get-bodies [] (:minimap-bodies (get-current-map-data)))

(let [alpha 0.9]
  (def- colors
    (mapvals
      {:wall   [:r 1 :g 1 :b 1 :a 1]
       :ground [:r 0.5 :g 0.5 :b 0.5 :a alpha]
       :water  [:r 0.6 :g 0.6 :b 0.6 :a alpha]}
      #(apply rgbcolor %))))
; where is unexplored color defined?
; wall color chosen as light so you can spot frontiers between ground and unexplored quicker

(defn create-minimap-image [w h] (Image. (int w) (int h) Image/FILTER_NEAREST))

(defn- draw-on-image [posicolorseq]
  (let [g (.getGraphics ^Image (get-image))] ; TODO .getGraphics expensive?
    (doseq [[[x y] color] posicolorseq]
      (draw-rect g x y 0 0 color))
    (.flush g)))

(defn show-on-minimap [color] ; TODO defcomponent?
  (create-comp :minimap-icon
               {:color color
                :depends [:body]}))

(def- scale 1)

(defn- render-bodies [g x y start-leftx start-topy width-in-tiles height-in-tiles]
  (doseq [body (reset! (get-bodies) (select exists? @(get-bodies))) ; swap?
          :let [{bodyx 0 bodyy 1 :as body-posi} (int-posi (get-position body))
                body-render-x (+ x (* scale (- bodyx start-leftx)))
                body-render-y (+ y (* scale (- bodyy start-topy)))]
          :when (geom/point-in-rect? body-posi start-leftx start-topy width-in-tiles height-in-tiles)]
    (fill-rect g (- body-render-x 0.5) (- body-render-y 0.5) 2 2 (:color (get-component body :minimap-icon)))))

(defn- cut-sub-image
  "out of image borders the image repeats itself, which shows on small maps, so dont create a sub-image which shows parts outside of image bounds"
  [^Image image start-leftx start-topy width-in-tiles height-in-tiles shift-right shift-down]
  (let [[imgw imgh] (get-dimensions image)
        subimage-leftx (max 0 start-leftx)
        subimage-topy (max 0 start-topy)
        subimage-width (- width-in-tiles shift-right)
        subimage-height (- height-in-tiles shift-down)
        subimage-width (if (> (+ subimage-leftx subimage-width)
                           imgw)
                         (- imgw subimage-leftx)
                         subimage-width)
        subimage-height (if (> (+ subimage-topy subimage-height)
                               imgh)
                          (- imgh subimage-topy)
                      subimage-height)]
    (get-sub-image image subimage-leftx subimage-topy subimage-width subimage-height)))

(defn render-minimap [g]
  (let [puffer 2
        x puffer
        y puffer
        half-w-tiles (int (/ (- half-screen-w puffer) scale))
        half-h-tiles (int (/ (- half-screen-h puffer) scale))
        width-in-tiles (* 2 half-w-tiles)
        height-in-tiles (* 2 half-h-tiles)
        px-width (* scale width-in-tiles)
        px-height (* scale height-in-tiles)
        [camerax cameray] (int-posi (get-camera-position))
        start-leftx (- camerax half-w-tiles)
        start-topy (- cameray half-h-tiles)
        shift-right (if (neg? start-leftx) (Math/abs (float start-leftx)) 0)
        shift-down (if (neg? start-topy) (Math/abs (float start-topy)) 0)
        subimg (cut-sub-image (get-image) start-leftx start-topy width-in-tiles height-in-tiles shift-right shift-down)
        subimage (get-scaled-copy subimg scale)]
    (fill-rect g x y px-width px-height (rgbcolor :r 1 :g 1 :b 1 :a 0.3))
    (draw-image subimage (+ x (* scale shift-right)) (+ y (* scale shift-down)))
    (draw-rect g x y px-width px-height color/gray)
    (render-bodies g x y start-leftx start-topy width-in-tiles height-in-tiles)))

(defn update-minimap [sensed-tiles]
  (let [sensed-cells (get-cells sensed-tiles)]
    (swap! (get-bodies) union (set (filter #(get-component % :minimap-icon) (get-bodies-from-cells sensed-cells))))
    (draw-on-image (for [{:keys [posi blocks]} (map deref (remove nil? sensed-cells))]
                     [posi (case blocks
                             #{:air :ground} (:wall colors)
                             #{:ground} (:water colors)
                             #{} (:ground colors))]))))





