(ns game.entity.projectile
  (:use
    [utils.core :only (xor)]
    (engine core render)
    (game.components core position [body :only (create-body)] render misc)))

(defpreload ^:private projectile-hits-wall-frames (folder-frames "effects/ember/"))

(defn- plop [position]
  (animation-entity
    :position position
    :animation (create-animation projectile-hits-wall-frames)))

; separate movement and projectile-collision ?
(defentity fire-projectile
  [:startbody :px-size :animation :side :hits-side :movement :hit-effects
   :opt :piercing :maxrange :maxtime]
  {:pre [(xor maxrange maxtime)]}
  (position-component (get-position startbody))
  (create-body :solid false
               :side side
               :pxw px-size
               :pxh px-size)
  movement
  (create-comp :projectile-collision
               {:piercing piercing
                :hits-side hits-side
                :hit-effects hit-effects
                :already-hit-bodies #{}
                :hits-wall-effect (fn [posi]
                                    (play-sound "bfxr_projectile_wallhit.wav")
                                    (plop posi))})
  (single-animation-component animation :order :air :apply-light false)
  (delete-after-duration-component (or maxtime (/ maxrange (:speed movement)))
                                   ;:duration-over (comp plop get-position)
                                   ))

