(ns game.media
  (:use
    (engine [core :only (defpreload)]
            [render :only (spritesheet get-sprite)])))

(defpreload ^:private sheet (spritesheet "items/items.png" 16 16))

(defn get-itemsprite [p]
  (get-sprite sheet p))








