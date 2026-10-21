(ns engine.render.color
  (:import (org.newdawn.slick Graphics Color)))

(def transparent Color/transparent)
(def white Color/white)
(def yellow Color/yellow)
(def red Color/red)
(def blue Color/blue)
(def green Color/green)
(def black Color/black)
(def gray Color/gray)
(def cyan Color/cyan)
(def darkGray Color/darkGray)
(def lightGray Color/lightGray)
(def pink Color/pink)
(def orange Color/orange)
(def magenta Color/magenta)

(defn rgbcolor [& {:keys [r g b a darker brighter]
                   :or {r 0 g 0 b 0 a 1 darker 0 brighter 0}}]
  (-> (Color. (float r) (float g) (float b) (float a))
      (.darker darker)
      (.brighter brighter)))

(defn set-color [^Graphics g color]
  (.setColor g color))

(def transparent-black (rgbcolor :a 0.8))
