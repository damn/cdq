(ns utils.selection-image
  (:require [engine.render :as color])
  (:use
    [engine.render :only (fill-rect)]
    [utils.core :only (runmap)])
  (:import org.newdawn.slick.Image))

; fillrect 2/2 weil beim rotieren von images sch�rfe verloren geht
(defn run-pixels-x [^Image image g xrange yrange]
  (runmap
    (fn [y] ;doseq
      (some
        (fn [x]
          (when (pos? (.getAlpha (.getColor image x y)))
            (fill-rect g x y 2 2 color/white)
            true))
        xrange))
    yrange))

; fillrect 2/2 weil beim rotieren von images sch�rfe verloren geht
(defn run-pixels-y [^Image image g xrange yrange]
  (runmap
    (fn [x] ;doseq
      (some
        (fn [y]
          (when (pos? (.getAlpha (.getColor image x y)))
            (fill-rect g x y 2 2 color/white)
            true))
        yrange))
    xrange))

(defn create-selection-image [^Image original]
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