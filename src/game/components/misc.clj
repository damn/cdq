(ns game.components.misc
  (:require [engine.core :refer [make-counter]])
  (:use utils.core
        game.utils.geom
        game.components.core))

(defcomponent delete-after-duration [duration & {:keys [duration-over]}]
  (active [delta c entity]
    (when (update-counter! entity delta c)
      (add-to-removelist entity)
      (when duration-over
        (duration-over entity))))
  {:counter (make-counter duration)
   :serialize [:counter]})

;;

(defn set-rotation-angle [entity angle] ; TODO has rotation component
  (assoc-in! entity [:rotation :angle] angle))

(defn- rotate-to-vector [body v]
  (set-rotation-angle body (get-angle-from-vector v)))

(defn rotate-to-body [a b]
  (rotate-to-vector a (entity-direction-vector a b)))

(defn rotate-to-player [body]
  (rotate-to-body body player-body))

(defn rotate-to-mouse [body]
  (rotate-to-vector body (get-vector-to-mouse-coords)))

; bodies mit verschiedener w/h lieber nicht rotieren da die body-collision shape nicht mit rotiert.
; also rotation nur bei bodies mit gleicher w/h da sie dann in ihrer collision shape drinbleiben
(defcomponent rotation []
  {:init #(assert (= (get-half-width %) (get-half-height %)))
   :moved rotate-to-vector
   :angle 0})

;;

(defn- regenerate [data delta percent-reg-per-second]
  (increase-min-max-val data
                        (->
                          percent-reg-per-second
                          (/ 100)         ; percent -> multiplier
                          (* (:max data)) ; in 1 second
                          (/ 1000)        ; in 1 ms
                          (* delta))))

(defn regeneration-component [ctype ks percent-reg-per-second]
  (create-comp ctype
               (active [delta component entity]
                 (update-in! entity ks regenerate delta (:reg-per-second component)))
               {:reg-per-second percent-reg-per-second}))

(defn hp-regen-component [percent-reg-per-second]
  (regeneration-component :hp-regen [:destructible :hp] percent-reg-per-second))

(defn mana-regen-component [percent-reg-per-second]
  (regeneration-component :mana-regen [:skillmanager :mana] percent-reg-per-second))
