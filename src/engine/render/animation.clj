(ns engine.render.animation
  (:require
    [engine.core :refer [Updateable]]
    [engine.render.image :refer [render-centered-image]]))

(defprotocol Animation
  (is-stopped?  [_])
  (restart      [_])
  (get-duration [_])
  (get-frame    [_]))

(defrecord ImmutableAnimation [frames frame-duration looping speed cnt maxcnt]
  Updateable
  (update [this delta]
    (let [newcnt (+ cnt (* speed delta))]
      (assoc this :cnt (cond (< newcnt maxcnt) newcnt
                             looping           (min maxcnt (- newcnt maxcnt))
                             :else             maxcnt))))
  Animation
  (is-stopped? [_]
    (and (not looping) (= cnt maxcnt)))
  (restart [this]
    (assoc this :cnt 0))
  (get-duration [_]
    maxcnt)
  (get-frame [this]
    ; int because speed can make delta a float value.
    (get frames (int (quot (dec cnt) frame-duration)))))

(def default-frame-duration 33)

(defn create-animation [frames & {:keys [frame-duration looping]
                                  :or {frame-duration default-frame-duration looping false}}]
  (map->ImmutableAnimation
    {:frames (vec frames)
     :frame-duration frame-duration
     :looping looping
     :speed 1
     :cnt 0
     :maxcnt (* (count frames) frame-duration)}))

(defn render-centered-animation [animation position]
  (render-centered-image (get-frame animation) position))
