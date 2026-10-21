(ns game.media
  (:require
    [engine.render.assets :refer [get-sprite spritesheet]]))

(alter-var-root #'engine.core/fns conj
  (fn []
    (def ^:private sheet (spritesheet "items/items.png" 16 16))))

(defn get-itemsprite [p]
  (get-sprite sheet p))








