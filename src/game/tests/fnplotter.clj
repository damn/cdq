(ns game.tests.fnplotter
  (:require [engine.render :as color])
  (:use (engine core render)
        game.utils.random))

(def width 400)
(def height width)
(def leftx 200)
(def topy 100)
(def bottomy (+ topy height))

(defn- high-weighted2 [x]
  (/ (* 2 x) (+ 1 x)))

(def points (map (fn [x] [x (high-weighted x)]) (range 0 1 0.01)))

(def points (map (fn [x] [x (high-weighted x)]) (repeatedly 50 rand)))

(start-slick-basicgame
  :width 800
  :height 600
  :render (fn [container g]
            (fill-rect g 0 0 800 600 color/black)
            (doseq [[x y] points]
              (draw-rect g (+ leftx (* x width)) (- bottomy (* y height)) 0 0 color/red))
            (draw-rect g leftx topy width height color/white)))

