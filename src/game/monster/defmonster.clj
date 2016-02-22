(ns game.monster.defmonster
  (:use
    utils.core
    [engine.render :only (create-image)]
    (game.components core position body destructible sleeping)
    (game settings)))

(defn monsterresrc [path] (str "opponents/" path))

(def monsterimage (comp create-image monsterresrc))

(defn assoc-w-and-h [props]
  (if-let [pxsize (:pxsize props)]
    (-> props (dissoc :pxsize) (assoc :pxw pxsize :pxh pxsize))
    props))

(defn create-monster
  "use defmonster not this for defining monsters (defmonster calls assoc-w-and-h!)"
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

; TODO mach doch einfach ne map wie bei skills von type zu den monster ...
; dann auch distinct types wählen wichtig ...
; find prefixed var irgendwie komisch ...
(def- defmonster-prefix "monster-")

(defn get-monster-properties [type] ; use ns-resolve 'game.monster.monsters (name monster-type) ? NO NEED FOR PREFIX CHECK? but then ...?
  (find-prefixed-var
    :namespace 'game.monster.monsters
    :prefix defmonster-prefix
    :prefixed-type type))

(defmacro defmonster [monster-type props & components]
  `(let [props# (assoc-w-and-h ~props)
         type# ~(keyword monster-type)]
     (def ~(symbol (str defmonster-prefix monster-type))
       {:create (fn [position#] (create-monster position# type# props# ~@components))
        :half-w (/ (:pxw props#) tile-width 2)
        :half-h (/ (:pxh props#) tile-height 2)
        :movement-type :ground})))



