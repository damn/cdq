(ns game.entity.chest
  (:require [engine.render :as color]
            game.utils.lightning)
  (:use (engine core render)
        game.media
        (game.components core position body pressable render)
        (game.item instance instance-impl)
        (game.maps data minimap)))

(defentity ^:private create-chest* [position item-name]
  (position-component position)
  (create-body :solid true
               :dimensions [16 16]
               :mouseover-outline true)
  ;(game.utils.lightning/light-component :intensity 0.7 :radius 2)
  (pressable-component ""
                       (fn [this-body]
                         (play-sound "bfxr_chestopen.wav")
                         (add-to-removelist this-body)
                         (if item-name
                           (create-item-body position item-name)
                           (create-rand-item position :max-lvl (:rand-item-max-lvl (get-current-map-data))))))
  (show-on-minimap color/magenta)
  (create-comp :always-in-sight)
  (image-render-component (get-itemsprite [1 4])))

(defn create-chest [position & {item-name :item-name}]
  (create-chest* position item-name))
