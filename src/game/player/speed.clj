(ns game.player.speed
  (:require [cdq.core :refer [in-tiles-per-ms]]))

(def player-move-speed 70) ; px/s

(def ^:private animation-types
  {:movement  [:moving]
   :attacking [:sword :sword-small :sword-big :fist]
   :casting   [:casting :meditation]})

(defn inc-speed [component value types]
  {:pre [(contains? animation-types types)]}
  (reduce #(update-in %1 [%2 :speed] + value)
          component
          (get animation-types types)))

(defn incr-attack-speed  [entity-hash value]
  (update-in entity-hash [:animation] inc-speed value :attacking))

(defn incr-casting-speed [entity-hash value]
  (update-in entity-hash [:animation] inc-speed value :casting))

(defn incr-move-speed [entity-hash modifier]
  (-> entity-hash
      (update-in [:animation] inc-speed modifier :movement)
      (update-in [:movement :speed] + (* (in-tiles-per-ms player-move-speed) modifier))))
