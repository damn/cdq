(ns game.player.skills.psi-bolt
  (:require
    [engine.core :refer [defpreload play-sound]]
    [engine.render :refer [create-animation folder-frames]]
    [utils.core :refer [runmap variance-val-str]]
    [game.settings :refer [in-tiles]]
    [game.components.body-effects-impl :refer [consume-psi-charges current-psi-charges]]
    [game.components.destructible :refer [deal-dmg get-destructible-bodies]]
    [game.components.render :refer [animation-entity]]
    [game.components.skills.core :refer [get-skill-use-mouse-tile-pos]]
    [game.components.skills.melee :refer [get-current-player-melee-dmg]]
    [game.components.skills.utils :refer [check-line-of-sight]]
    [game.utils.random :refer [rand-int-between]]
    [game.player.skill.learnable :refer [deflearnable-skill]]))

(def ^:private psi-bolt-pxradius 17)
(defpreload ^:private psi-bolt-frames (folder-frames "effects/psibolt/"))

(defn- do-skill-psi-bolt [entity component]
  (let [cnt (consume-psi-charges entity)
        radius (in-tiles psi-bolt-pxradius)
        dmg (*
              (+ 0.5 (* cnt 0.5))
              (rand-int-between (get-current-player-melee-dmg)))
        posi (get-skill-use-mouse-tile-pos)
        hits (get-destructible-bodies posi radius :monster)]
    (play-sound "bfxr_psibolt.wav")
    (animation-entity
      :animation (create-animation psi-bolt-frames)
      :position posi)
    (runmap #(deal-dmg dmg %) hits)))

(deflearnable-skill psi-bolt
  :manacost 15
  :menu-posi [1 2]
  :mousebutton :right
  :icon "icons/psibolt.png"
  :info "Finisher-Skill
PSI-Explosion

0 Charges - 50% melee weapon damage
1 Charge  - 100% melee weapon damage
2 Charges - 150% melee weapon damage
3 Charges - 200% melee weapon damage"
  {:animation :casting
   :show-info-for [:cost]
   :check-usable check-line-of-sight
   :do-skill do-skill-psi-bolt
   :dmg-info (fn [entity skill]
               (let [cnt (current-psi-charges entity)
                     [mn mx] (get-current-player-melee-dmg)
                     modifier (+ 0.5 (* cnt 0.5))]
                 (variance-val-str
                   [(* modifier mn) (* modifier mx)])))})
