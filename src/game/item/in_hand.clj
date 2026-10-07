(ns game.item.in-hand
  (:require
    [engine.core :refer [set-mouse-cursor]]
    [engine.render.image :refer [get-scaled-copy]]
    [game.settings :refer [screen-scale]]
    [game.mouse-cursor :refer [reset-default-mouse-cursor]]))

(def item-in-hand (atom nil))

(defn is-item-in-hand? [] @item-in-hand)

(defn set-item-in-hand [item]
  (reset! item-in-hand item)
  (set-mouse-cursor (get-scaled-copy (:image item) screen-scale) (* 8 screen-scale) (* 8 screen-scale)))

(defn empty-item-in-hand []
  (reset! item-in-hand nil)
  (reset-default-mouse-cursor))
