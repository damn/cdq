(ns game.player.speed
  (:require [game.components.movement :refer [in-tiles-per-ms]]
            (game.player [animation :as animation]
                         [settings  :as settings])))

(defn incr-attack-speed  [entity-hash value]
  (update-in entity-hash [:animation] animation/inc-speed value :attacking))

(defn incr-casting-speed [entity-hash value]
  (update-in entity-hash [:animation] animation/inc-speed value :casting))

(defn incr-move-speed [entity-hash modifier]
  (-> entity-hash
      (update-in [:animation] animation/inc-speed modifier :movement)
      (update-in [:movement :speed] + (* (in-tiles-per-ms settings/player-move-speed) modifier))))
