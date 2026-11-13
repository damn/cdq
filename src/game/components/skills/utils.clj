(ns game.components.skills.utils
  (:require
    [cdq.core :refer [direction-vector get-vector-to-mouse-coords]]
    [cdq.core :refer [white]]
    [cdq.core :refer [create-image render-centered-image]]
    [cdq.core :refer [position-component]]
    [game.utils.msg-to-player :refer [show-msg-to-player]]
    [cdq.core :refer [ray-blocked?]]
    [game.mouseoverbody :refer [saved-mouseover-body]]
    [cdq.core :refer [add-to-removelist create-entity id-entity-map player-body]]
    [game.components.render :refer [render-on-map]]
    [cdq.core :refer [delete-after-duration-component]]
    [game.components.skills.core :refer [get-skill-use-mouse-pos get-skill-use-mouse-tile-pos]]))

(defn- cross [position image]
  (create-entity
    (position-component position)
    {:type :always-in-sight} ; because used where not in sight f.e.
    (merge {:type :render}
           (render-on-map :top-level [g _ c render-posi]
             (render-centered-image image render-posi)))
    (delete-after-duration-component 1000)))

(def ^:private old-cross (atom nil))

(defn- not-allowed-position-effect [position]
  (when (and @old-cross (get @id-entity-map (:id (meta @old-cross))))
    (add-to-removelist @old-cross))
  (reset! old-cross
          (cross position
                 (create-image "effects/forbidden.png" :transparent white :scale [32 32]))))

(defn check-line-of-sight [entity _]
  (let [target (get-skill-use-mouse-tile-pos)]
    (if (ray-blocked? (:value (:position @entity)) target)
      (do
        (show-msg-to-player "No line of sight to target!")
        (not-allowed-position-effect target)
        false)
      true)))

;;

(defn get-player-ranged-vector []
  (if-let [mouseover-body @saved-mouseover-body]
    (direction-vector (:value (:position @player-body)) (:value (:position @mouseover-body)))
    (get-vector-to-mouse-coords (get-skill-use-mouse-pos))))

