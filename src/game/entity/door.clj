(ns game.entity.door
  (:require
    [engine.render :as color]
    [engine.core :refer [play-sound]]
    [game.maps.minimap :refer [show-on-minimap]]
    [utils.core :refer [assoc-in!]]
    [game.components.core :refer [add-to-removelist create-comp create-entity create-entity-no-init defentity get-component]]
    [game.components.position :refer [position-component]]
    [game.components.body :refer [create-body]]
    [game.components.pressable :refer [pressable-component]]
    [game.components.render :refer [image-render-component]]
    [game.maps.cell-grid :refer [cell-blocks-changed-update-listeners change-cell-blocks get-cell]]))

; another way of handling the open door was to change the image&mouseover-outline to false&remove component :pressable
; but it had still a :body component which blocks mouseoverentities that lie below it because there is always only 1 mouseoverbody at each position
; ... or remove the :body component too?
(defentity ^:private make-open-door [p image]
  (position-component p)
  (create-comp :always-in-sight)
  (image-render-component image :order :air))

(defentity make-door [p closed-image open-image] ; make-closed-door ?
  (position-component p)
  (create-body :solid false
               :dimensions [16 16]
               :mouseover-outline true)
  (create-comp :always-in-sight)
  (show-on-minimap color/green)
  (image-render-component closed-image :order :air)
  (create-comp :clicked {:is? false})
  (pressable-component ""
                       (fn [entity]
                         (when-not (:is? (get-component entity :clicked))
                           (assoc-in! entity [:clicked :is?] true)
                           (play-sound "ReversyH-Nick_Ros-105.wav")
                           (add-to-removelist entity)
                           (make-open-door p open-image)
                           (change-cell-blocks (get-cell p) #{})
                           (cell-blocks-changed-update-listeners)))))


