(ns game.tools.image-center-pxdist
  (:require [game.utils.geom :as geom])
  (:use (engine core input render)))

(def posis (atom []))

(def center [200 200])

(defn get-center-dist []
  (let [[x y] center
        [mx my] (get-mouse-pos)]
    [(- mx x) (- my y)]))

(start-slick-basicgame
  :init (fn [container]
          (def image (create-image "opponents/station/station01.png")))

  :update (fn [container delta]
            (update-mousebutton-state)
            (when (is-leftm-pressed?)
              (println (swap! posis conj (get-center-dist)))))

  :render (fn [container g]
            (let [shape (geom/rectangle 0 0
                                        (.getWidth image)
                                        (.getHeight image))]
              (.drawCentered image x y) ; TODO
              (render-centered-shape g shape center)
              (render-readable-text g 50 50
                (str "Dist to image center: " (get-center-dist) "\nposis: " @posis)))))
