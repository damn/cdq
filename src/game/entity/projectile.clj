(ns game.entity.projectile
  (:require
    [cdq.core :refer [xor]]
    [game.components.body :refer [create-body]]
    [cdq.core :refer [play-sound]]
    [cdq.core :refer [create-animation]]
    [cdq.core :refer [folder-frames]]
    [cdq.core :refer [create-entity]]
    [cdq.core :refer [position-component]]
    [game.components.render :refer [animation-entity single-animation-component]]
    [cdq.core :refer [delete-after-duration-component]]))

(alter-var-root #'cdq.core/init-fns conj
  (fn []
    (def ^:private projectile-hits-wall-frames (folder-frames "effects/ember/"))))

(defn- plop [position]
  (animation-entity
    :position position
    :animation (create-animation projectile-hits-wall-frames)))

; separate movement and projectile-collision ?
(defn fire-projectile
  [& {:keys [startbody px-size animation side hits-side movement hit-effects
             piercing maxrange maxtime]}]
  {:pre [(xor maxrange maxtime)]}
  (create-entity
    (position-component (:value (:position @startbody)))
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
                                     ;:duration-over (comp plop #(:value (:position @%)))
                                     )))

