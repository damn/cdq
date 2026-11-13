(ns game.monster.defmonster
  (:require
    [cdq.core :refer [create-entity]]
    [cdq.core :refer [position-component]]
    [cdq.core :refer [create-body]]
    [cdq.core :refer [monster-destructible]]
    [cdq.core :refer [sleeping-component]]
    [cdq.core :refer [tile-height tile-width]]))

(defn create-monster
  "use defmonster not this for defining monsters"
  [position monster-type {hp :hp armor :armor :as props} & components] ; monster-type not used?! TODO was used for (:is-boss) or something like that
  (apply create-entity
         (position-component position)
         (create-body :solid true
                      :side :monster
                      :pxw (:pxw props)
                      :pxh (:pxh props)
                      :mouseover-outline true)
         (monster-destructible hp armor)
         (sleeping-component)
         components))

(def monsters {})

(defn get-monster-properties [type]
  (or (get monsters type)
      (throw (Error. (str "Could not find monster: " type)))))

(defmacro defmonster [monster-type props & components]
  `(let [props# ~props
         type# ~(keyword monster-type)]
     (alter-var-root #'monsters assoc type#
       {:create (fn [position#] (create-monster position# type# props# ~@components))
        :half-w (/ (:pxw props#) tile-width 2)
        :half-h (/ (:pxh props#) tile-height 2)
        :movement-type :ground})))



