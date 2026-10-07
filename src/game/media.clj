(ns game.media
  (:require
    [engine.core :refer [defpreload]]
    [engine.render.assets :refer [get-sprite spritesheet]]))

(defpreload ^:private sheet (spritesheet "items/items.png" 16 16))

(defn get-itemsprite [p]
  (get-sprite sheet p))








