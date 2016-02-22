(ns game.components.skills.utils
  (:require [engine.render :as color])
  (:use
    utils.core
    [engine.render :only (render-centered-image create-image)]
    game.mouseoverbody
    (game.components core render misc
                     [position :only (position-component)])
    (game.utils geom
      [msg-to-player :only (show-msg-to-player)]
      [raycast :only (ray-blocked?)])
    game.components.skills.core))

(defentity ^:private cross [position image]
  (position-component position)
  (create-comp :always-in-sight) ; because used where not in sight f.e.
  (create-comp :render
   (render-on-map :top-level [g _ c render-posi]
    (render-centered-image image render-posi)))
  (delete-after-duration-component 1000))

(def ^:private old-cross (atom nil))

(defn- not-allowed-position-effect [position]
  (when (and @old-cross (exists? @old-cross))
    (add-to-removelist @old-cross))
  (reset! old-cross
          (cross position
                 (create-image "effects/forbidden.png" :transparent color/white :scale [32 32]))))

(defn check-line-of-sight [entity _]
  (let [target (get-skill-use-mouse-tile-pos)]
    (if (ray-blocked? (get-position entity) target)
      (do
        (show-msg-to-player "No line of sight to target!")
        (not-allowed-position-effect target)
        false)
      true)))

;;

(defn get-player-ranged-vector []
  (if-let [mouseover-body @saved-mouseover-body]
    (entity-direction-vector player-body mouseover-body)
    (get-vector-to-mouse-coords (get-skill-use-mouse-pos))))


