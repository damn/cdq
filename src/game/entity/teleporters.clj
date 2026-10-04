(ns game.entity.teleporters
  (:require
    [engine.render :as color :refer [create-animation get-dimensions get-frame spritesheet-frames]]
    [game.maps.data :refer [current-map do-in-map get-pretty-name]]
    utils.core
    [engine.core :refer [play-sound]]
    game.media
    game.settings
    game.utils.lightning
    [game.maps.minimap :refer [show-on-minimap]]
    [game.maps.mapchange :refer [queue-map-change]]
    [game.components.core :refer [create-comp create-entity create-entity-no-init defentity]]
    [game.components.position :refer [position-component]]
    [game.components.body :refer [create-body]]
    [game.components.render :refer [single-animation-component]]
    game.components.misc
    [game.components.pressable :refer [pressable-component]]))

(defentity create-teleporter
  [:position :target-map :target-posi :animation
   :opt :do-after-use :save-game]
  (position-component position)
  (create-body :solid false
               :dimensions (get-dimensions (get-frame animation))
               :mouseover-outline true)
  ;(light-component :intensity 0.8 :radius 2)
  (create-comp :always-in-sight)
  (show-on-minimap color/blue)
  (pressable-component
    (str "Teleport to " (get-pretty-name target-map))
    (fn [this-body]
      (play-sound "bfxr_teleport.wav")
      (queue-map-change target-posi target-map save-game)
      (when do-after-use (do-after-use))))
  (single-animation-component animation :order :is-ground))

(defn static-teleporter
  [& {[start-map start-posi] :from
      [target-map target-posi] :to
      save-game :save-game}]
  (do-in-map start-map
    (create-teleporter
      :position start-posi
      :target-map target-map
      :target-posi target-posi
      :animation (create-animation (spritesheet-frames "teleporter/teleporter.png" 20 10) :frame-duration 100 :looping true)
      :save-game save-game)))

(defn connect-static-teleporters [& {start :from target :to}]
  (static-teleporter :from start :to target)
  (static-teleporter :from target :to start))

