(ns engine.tests.render
  (:use
    (engine core render))
  (:import (org.newdawn.slick Image Color)))

(def image)

(use 'utils.selection-image)

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