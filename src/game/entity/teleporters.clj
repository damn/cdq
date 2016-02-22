(ns game.entity.teleporters
  (:require [engine.render :as color])
  (:use
    utils.core
    (engine core render)
    (game media settings)
    (game.utils lightning)
    (game.maps
      [data :only (do-in-map current-map get-pretty-name)]
      minimap mapchange)
    (game.components core position body render misc pressable)))

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

