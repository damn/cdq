(ns game.components.movement.ai.homing
  (:use
    utils.core
    game.utils.geom
    (game.components core movement)))

(defn- move-and-rotate-to-target-control
  [projectile {:keys [target-body current-angle rotationspeed]} delta]
  (vector-from-angle
    (if-not (exists? target-body)
      current-angle
      (let [angle-to-target (get-angle-to-position (get-position projectile) (get-position target-body))
            adjusted-angle (rotate-angle-to-angle current-angle angle-to-target rotationspeed delta)]
        (assoc-in! projectile [:movement :current-angle] adjusted-angle)
        adjusted-angle))))

(defn create-homing-movement [speed target-body start-angle rotationspeed move-type]
  (movement-component
    {:control-update move-and-rotate-to-target-control
     :target-body target-body
     :current-angle start-angle
     :rotationspeed rotationspeed}
    speed
    move-type))

