(ns game.entity.door
  (:require
    [engine.render.color :as color]
    [engine.core :refer [play-sound]]
    [game.maps.minimap :refer [show-on-minimap]]
        [game.components.core :refer [add-to-removelist create-entity]]
    [game.components.position :refer [position-component]]
    [game.components.body :refer [create-body]]
    [game.components.pressable :refer [pressable-component]]
    [game.components.render :refer [image-render-component]]
    [game.maps.cell-grid :refer [cell-blocks-changed-update-listeners change-cell-blocks get-cell]]))

; another way of handling the open door was to change the image&mouseover-outline to false&remove component :pressable
; but it had still a :body component which blocks mouseoverentities that lie below it because there is always only 1 mouseoverbody at each position
; ... or remove the :body component too?
(defn- make-open-door [p image]
  (create-entity
    (position-component p)
    {:type :always-in-sight}
    (image-render-component image :order :air)))

(defn make-door [p closed-image open-image] ; make-closed-door ?
  (create-entity
    (position-component p)
    (create-body :solid false
                 :dimensions [16 16]
                 :mouseover-outline true)
    {:type :always-in-sight}
    (show-on-minimap color/green)
    (image-render-component closed-image :order :air)
    {:type :clicked :is? false}
    (pressable-component ""
                         (fn [entity]
                           (when-not (:is? (:clicked @entity))
                             (swap! entity assoc-in [:clicked :is?] true)
                             (play-sound "ReversyH-Nick_Ros-105.wav")
                             (add-to-removelist entity)
                             (make-open-door p open-image)
                             (change-cell-blocks (get-cell p) #{})
                             (cell-blocks-changed-update-listeners))))))


