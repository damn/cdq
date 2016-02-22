(ns engine.tests.copy-image
  (:use (engine core render))
  (:import (org.newdawn.slick Image Color)))

; maybe compress the data so savegame is more readable? 1ASssF9g8aAgsGDGe

(defpreload ^:private sheet-frames (map #(get-scaled-copy % 2)
                                        (spritesheet-frames "effects/expbig.png" 40 40)))
(declare frankenstein)

(defn render [container g]
  (let [img (nth sheet-frames 7)]
    (.draw img 50 50)
    (defonce frankenstein (recreate-image (save-image-data img)))
    (.draw frankenstein 150 50))
  (let [img (get-scaled-copy (nth sheet-frames 8) 0.5)]
    (.draw img 50 200)
    (.draw (recreate-image (save-image-data img)) 150 200)
    )
  )

(comment
  (shorter (save-image-data (nth sheet-frames 7)))
  {:ref "effects/expbig.png", :sub-bounds [0 40 40 40], :bounds [80 80]}
  )

(defn start-test []
  (start-slick-basicgame
    :init (fn [container])
    :render (fn [container g]
              (fill-rect g 0 0 800 600 Color/blue)
              (render container g))))

;(start-test)
