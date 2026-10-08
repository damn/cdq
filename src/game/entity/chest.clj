(ns game.entity.chest
  (:require
    [engine.render.color :as color]
    game.utils.lightning
    [engine.core :refer [play-sound]]
    [game.media :refer [get-itemsprite]]
    [game.components.core :refer [add-to-removelist create-entity]]
    [game.components.position :refer [position-component]]
    [game.components.body :refer [create-body]]
    [game.components.pressable :refer [pressable-component]]
    [game.components.render :refer [image-render-component]]
    [game.item.instance :refer [create-item-body]]
    [game.item.instance-impl :refer [create-rand-item]]
    [game.maps.data :refer [get-current-map-data]]
    [game.maps.minimap :refer [show-on-minimap]]))

(defn- create-chest* [position item-name]
  (create-entity
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
    {:type :always-in-sight}
    (image-render-component (get-itemsprite [1 4]))))

(defn create-chest [position & {item-name :item-name}]
  (create-chest* position item-name))
