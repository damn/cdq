(ns game.entity.teleporters
  (:require
    [cdq.core :refer [blue]]
    [cdq.core :refer [get-dimensions]]
    [cdq.core :refer [create-animation get-animation-frame]]
    [cdq.core :refer [spritesheet-frames]]
    [cdq.core :refer [current-map do-in-map get-pretty-name]]
    [cdq.core :refer [play-sound]]
    cdq.core
    [cdq.core :refer [show-on-minimap]]
    [game.maps.mapchange :refer [queue-map-change]]
    [cdq.core :refer [create-entity]]
    [cdq.core :refer [position-component]]
    [cdq.core :refer [create-body]]
    [cdq.core :refer [single-animation-component]]
    [cdq.core :refer [pressable-component]]))

(defn create-teleporter
  [& {:keys [position target-map target-posi animation do-after-use save-game]}]
  (create-entity
    (position-component position)
    (create-body :solid false
                 :dimensions (get-dimensions (get-animation-frame animation))
                 :mouseover-outline true)
    ;(light-component :intensity 0.8 :radius 2)
    {:type :always-in-sight}
    (show-on-minimap blue)
    (pressable-component
      (str "Teleport to " (get-pretty-name target-map))
      (fn [this-body]
        (play-sound "bfxr_teleport.wav")
        (queue-map-change target-posi target-map save-game)
        (when do-after-use (do-after-use))))
    (single-animation-component animation :order :is-ground)))

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

