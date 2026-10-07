(ns game.components.skills.utils
  (:require
    [engine.render :as color :refer [create-image render-centered-image]]
    [game.components.position :refer [position-component]]
    [game.utils.msg-to-player :refer [show-msg-to-player]]
    [game.utils.raycast :refer [ray-blocked?]]
    utils.core
    [game.mouseoverbody :refer [saved-mouseover-body]]
    [game.components.core :refer [add-to-removelist create-entity create-entity-no-init defentity exists? get-position player-body]]
    [game.components.render :refer [render-on-map]]
    [game.components.misc :refer [delete-after-duration-component]]
    [game.utils.geom :refer [entity-direction-vector get-vector-to-mouse-coords]]
    [game.components.skills.core :refer [get-skill-use-mouse-pos get-skill-use-mouse-tile-pos]]))

(defentity ^:private cross [position image]
  (position-component position)
  {:type :always-in-sight} ; because used where not in sight f.e.
  (merge {:type :render}
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


