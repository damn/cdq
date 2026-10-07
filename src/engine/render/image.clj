(ns engine.render.image
  (:import (org.newdawn.slick Graphics Color Image)))

(defn get-dimensions [^Image image]
  [(.getWidth image)
   (.getHeight image)])

(defn draw-image
  ([image x y]
    (.draw ^Image image x y))
  ([image ^Float x ^Float y ^Float w ^Float h]
    (.draw ^Image image x y w h)))

(defn render-centered-image [image [x y]]
  (.drawCentered ^Image image x y))

(defn render-rotated-centered-image [^Graphics g image angle {x 0 y 1 :as position}]
  (.rotate g x y angle)
  (render-centered-image image position)
  (.rotate g x y  (- angle)))

(defn get-sub-image [^Image i x y w h]
  (.getSubImage i x y w h))

(defn get-scaled-copy
  ([^Image i value] (.getScaledCopy i value))
  ([^Image i w h]   (.getScaledCopy i w h)))

(defn create-image
  "org.newdawn.slick.Image data is cached(?) so if the same image path is loaded x2
  the second time it loads from the cached data much faster."
  [file & {:keys [transparent scale]}]
  ; Image constructor: String ref, boolean flipped, int f, Color transparent
  ; set this filter so there is no antialiasing effect when images are not at a perfect pixel position or rotated
  ; set in constructor not later in .setFilter so the image will not be loaded (deferred loading)
  (let [image (if transparent
                (Image. (str file) false Image/FILTER_NEAREST ^Color transparent)
                (Image. (str file) false Image/FILTER_NEAREST))]

    (cond
      (vector? scale) (apply get-scaled-copy image scale)
      (number? scale) (get-scaled-copy image scale)
      :else image)))

(defn create-empty-image [w h]
  (Image. (int w) (int h)))
