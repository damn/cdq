(ns engine.render.color
  (:import (org.newdawn.slick Graphics Color)))

(defmacro defslick2dcolors [& slick-static-colors]
  `(do
     ~@(map #(list 'def % (symbol (str "Color/" %))) slick-static-colors)))

(defslick2dcolors
  transparent
  white
  yellow
  red
  blue
  green
  black
  gray
  cyan
  darkGray
  lightGray
  pink
  orange
  magenta)

(defn rgbcolor [& {:keys [r g b a darker brighter]
                   :or {r 0 g 0 b 0 a 1 darker 0 brighter 0}}]
  (-> (Color. (float r) (float g) (float b) (float a))
      (.darker darker)
      (.brighter brighter)))

(defmacro defcolor [namesym & args]
  `(def ~namesym (rgbcolor ~@args)))

(defn set-color [^Graphics g color]
  (.setColor g color))

(defcolor transparent-black :a 0.8)
