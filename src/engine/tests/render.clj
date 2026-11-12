(ns engine.tests.render
  (:require
    [engine.core :refer [start-slick-basicgame]]
    [engine.render.color :as color]
    [engine.render.image :refer [create-image]]
    [engine.render.graphics :refer [fill-rect render-readable-text]])
  (:import (org.newdawn.slick Image Color)))

(def image)

; fillrect 2/2 weil beim rotieren von images schärfe verloren geht
(defn- run-pixels-x [^Image image g xrange yrange]
  (dorun (map
    (fn [y] ;doseq
      (some
        (fn [x]
          (when (pos? (.getAlpha (.getColor image x y)))
            (fill-rect g x y 2 2 color/white)
            true))
        xrange))
    yrange)))

; fillrect 2/2 weil beim rotieren von images schärfe verloren geht
(defn- run-pixels-y [^Image image g xrange yrange]
  (dorun (map
    (fn [x] ;doseq
      (some
        (fn [y]
          (when (pos? (.getAlpha (.getColor image x y)))
            (fill-rect g x y 2 2 color/white)
            true))
        yrange))
    xrange)))

(defn- create-selection-image [^Image original]
  (let [w (.getWidth original)
        h (.getHeight original)
        selection-image (Image. w h)
        g (.getGraphics selection-image)]
    (run-pixels-x original g (range w) (range h))
    (run-pixels-x original g (reverse (range w)) (range h))
    (run-pixels-y original g (range w) (range h))
    (run-pixels-y original g (range w) (reverse (range h)))
    (.flush g)
    selection-image))

(defn render [container g]
  (.draw image 50 50)
  ; image filter color, white = normal colors; black = black outline
  ; .drawFlash & filterColor => differentoutline colors?
  (.draw image (float 150) (float 50) Color/blue)
  (.draw image (float 200) (float 50) Color/white)
  (.draw (create-selection-image image) (float 200) (float 50) Color/green)
  (.draw image (float 250) (float 50) Color/black)
  (render-readable-text g 200 180 :background false (apply str (map char (range 32 91))))
  (render-readable-text g 200 200 :background true
    Color/white "Loading...0%"
    Color/green "Loading...0%"
    Color/yellow "HELLO WORLD\nnewlineyellow"
    Color/white "I LOVE YOU"
    123
    nil
    Color/green "NOBODY!"
    nil
    nil
    1234
    5
    \o
    3/2
    Color/magenta "blablabla \n this is a text \n and it continues\n\n"
    Color/white "forever\nand"
    "ever"))

(defn start-test []
  (start-slick-basicgame
    :init (fn [container]
            (def image (create-image "opponents/storage1.png")))
    :render (fn [container g]
              (fill-rect g 0 0 800 600 Color/blue)
              (render container g))))

(start-test)
