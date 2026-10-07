(ns game.entity.projectile
  (:require
    [utils.core :refer [xor]]
    [game.components.body :refer [create-body]]
    [engine.core :refer [defpreload play-sound]]
    [engine.render.animation :refer [create-animation]]
    [engine.render.assets :refer [folder-frames]]
    [game.components.core :refer [create-entity create-entity-no-init defentity get-position]]
    [game.components.position :refer [position-component]]
    [game.components.render :refer [animation-entity single-animation-component]]
    [game.components.misc :refer [delete-after-duration-component]]))

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
  {:type :projectile-collision
   :piercing piercing
   :hits-side hits-side
   :hit-effects hit-effects
   :already-hit-bodies #{}
   :hits-wall-effect (fn [posi]
                       (play-sound "bfxr_projectile_wallhit.wav")
                       (plop posi))}
  (single-animation-component animation :order :air :apply-light false)
  (delete-after-duration-component (or maxtime (/ maxrange (:speed movement)))
                                   ;:duration-over (comp plop get-position)
                                   ))

