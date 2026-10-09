(ns game.components.misc
  (:require
    [engine.core :refer [make-counter]]
    [utils.coll :refer [safe-merge]]
    [utils.numbers :refer [increase-min-max-val]]
    [game.utils.geom :refer [direction-vector get-angle-from-vector get-vector-to-mouse-coords]]
    [game.components.core :refer [active add-to-removelist player-body update-counter!]]))

(defn delete-after-duration-component [duration & {:keys [duration-over]}]
  (safe-merge
    {:type :delete-after-duration
     :counter (make-counter duration)
     :serialize [:counter]}
    (active [delta c entity]
      (when (update-counter! entity delta c)
        (add-to-removelist entity)
        (when duration-over
          (duration-over entity))))))

;;

(defn set-rotation-angle [entity angle] ; TODO has rotation component
  (swap! entity assoc-in [:rotation :angle] angle))

(defn- rotate-to-vector [body v]
  (set-rotation-angle body (get-angle-from-vector v)))

(defn rotate-to-body [a b]
  (rotate-to-vector a (direction-vector (:value (:position @a)) (:value (:position @b)))))

(defn rotate-to-player [body]
  (rotate-to-body body player-body))

(defn rotate-to-mouse [body]
  (rotate-to-vector body (get-vector-to-mouse-coords)))

; bodies mit verschiedener w/h lieber nicht rotieren da die body-collision shape nicht mit rotiert.
; also rotation nur bei bodies mit gleicher w/h da sie dann in ihrer collision shape drinbleiben
(defn rotation-component []
  {:type :rotation
   :init #(assert (= (:half-width (:body @%)) (:half-height (:body @%))))
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
  (merge {:type ctype
          :reg-per-second percent-reg-per-second}
         (active [delta component entity]
           (swap! entity update-in ks regenerate delta (:reg-per-second component)))))

(defn hp-regen-component [percent-reg-per-second]
  (regeneration-component :hp-regen [:destructible :hp] percent-reg-per-second))

(defn mana-regen-component [percent-reg-per-second]
  (regeneration-component :mana-regen [:skillmanager :mana] percent-reg-per-second))
