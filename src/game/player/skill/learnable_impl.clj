(ns game.player.skill.learnable-impl
  (:require [engine.render :as color])
  (:use
    utils.core
    (engine core render)
    (game settings mouseoverbody)
    (game.components core position body misc render destructible body-effects-impl movement)
    (game.entity nova projectile)
    game.components.movement.ai.homing
    (game.components.skills core melee utils)
    game.player.skill.learnable
    (game.utils raycast random geom
      [lightning :only (light-component)]
      [tilemap :only (get-mouse-tile-pos)])))

(defn- dmg-info-player-spell [skill]
  (variance-val-str
    (calc-effective-spell-dmg
      (:dmg skill)
      (:percent-modify-spell (get-component player-body :item-boni)))))

(defpreload ^:private projectile-frames (folder-frames "effects/energyball/"))

(deflearnable-skill player-ranged
  :manacost 2
  :menu-posi [0 0]
  :mousebutton :both
  :icon "icons/ranged.png"
  :info "Fires a projectile"
  {:dmg [5 7]
   :dmg-info dmg-info-player-spell
   :show-info-for [:cost :dmg]
   :animation :casting
   :do-skill (fn [entity component]
               (fire-projectile
                 :startbody entity
                 :px-size 8
                 :animation (create-animation projectile-frames :looping true)
                 :side :player
                 :hits-side :monster
                 :movement (projectile-movement-component (get-player-ranged-vector) 160)
                 :hit-effects [(dmg-effect (:dmg component) :is-player-spell true)
                               (stun-collision-effect 100 200)]
                 :piercing false
                 :maxrange 8))})

;(deflearnable-skill meditation
;  :manacost 0
;  :menu-posi [2 2]
;  :mousebutton :right
;  :icon "icons/character.png"
;  :info "Regenerate your mana with meditation."
;  {:animation :meditation
;   :do-skill (fn [entity component]
;               (when (lower-than-max? (get-mana entity))
;                 (show-gains-mana-effect entity (min 100 (rest-to-max (get-mana entity))))
;                 (update-in! entity [:skillmanager :mana] increase-min-max-val 100)))})


(defn- psi-charge-hit-effect [_]
  (let [{:keys [attack-bonus movement-bonus]} (:psi-charger learnable-skills)]
    (add-psi-charge player-body attack-bonus movement-bonus)))

(deflearnable-skill psi-charger
  :manacost 12
  :menu-posi [1 0]
  :mousebutton :both
  :icon "icons/psi_charger.png"
  :info
  "Melee Attack where every successful hit
gives you a PSI-Charge. PSI-Charges give
you a passive bonus and can be used with
finisher-skills.

1 Charge  - +20% Movement-Speed
            +13% Attack-Speed
2 Charges - +40% Movement-Speed
            +27% Attack-Speed
3 Charges - +60% Movement-Speed
            +40% Attack-Speed"
  {:attack-bonus (/ 0.4 max-psi-charges)
   :movement-bonus (/ 0.6 max-psi-charges)
   :show-info-for [:cost]}
  (player-melee-props [psi-charge-hit-effect]))

(defpreload ^:private nova-frames (folder-frames "effects/nova/"))

(deflearnable-skill nova
  :manacost 10
  :menu-posi [2 0]
  :mousebutton :right
  :icon "icons/nova.png"
  :info "Fires a nova."
  {:dmg [6 9]
   :dmg-info dmg-info-player-spell
   :radius 3
   :show-info-for [:cost :dmg]
   :animation :casting
   :do-skill (fn [entity {:keys [radius dmg] :as component}]
               (nova-effect
                 :position (get-position entity)
                 :duration 200
                 :maxradius radius
                 :affects-side :monster
                 :dmg dmg
                 :is-player-spell true
                 :animation (create-animation nova-frames)))})

(def- curse-infostr "Curse\nOnly one curse is active at a time\n")

(defpreload ^:private curse-frames (map #(get-scaled-copy % 0.3) (folder-frames "effects/curse/")))

(defn- create-curse-effect [[x y]]
  (play-sound "bfxr_curse.wav")
  (animation-entity
    :position [x (- y (in-tiles (/ 80 3)))]
    :animation (create-animation curse-frames)))

(deflearnable-skill bullettime-field
  :manacost 50
  :menu-posi [0 1]
  :mousebutton :both
  :icon "icons/bullettime.png"
  :info (str curse-infostr "Slows down monsters by 60%")
  {:radius 1.5
   :seconds 20
   :show-info-for [:cost :seconds]
   :animation :casting
   :check-usable check-line-of-sight
   :do-skill (fn [_ {:keys [radius seconds]}]
               (let [posi (get-skill-use-mouse-tile-pos)]
                 (create-curse-effect posi)
                 ;(create-circle-render-effect posi radius (rgbcolor :b 0.6 :a 0.7) 250)
                 (create-bullettime-effects posi radius seconds)))})

(deflearnable-skill aoe-armor-reduce
  :manacost 50
  :menu-posi [0 2]
  :mousebutton :both
  :icon "icons/armorreduce.png"
  :info (str curse-infostr "Reduces armor by 50%")
  {:radius 1.5
   :seconds 20
   :show-info-for [:cost :seconds]
   :animation :casting
   :check-usable check-line-of-sight
   :do-skill (fn [_ {:keys [radius seconds]}]
               (let [posi (get-skill-use-mouse-tile-pos)]
                 (create-curse-effect posi)
                 ;(create-circle-render-effect posi radius (rgbcolor :b 0.6 :a 0.7) 250)
                 (aoe-armor-reducer posi radius 50 seconds)))})

(deflearnable-skill stomp
  :manacost 10
  :menu-posi [1 1]
  :mousebutton :right
  :icon "icons/stomp.png"
  :info "Finisher-Skill
Stuns and deals damage

0 Charges - 1s stun duration
1 Charge  - 1.3s stun duration
            20% melee weapon damage
2 Charges - 1.6s stun duration
            40% melee weapon damage
3 Charges - 2s stun duration
            60% melee weapon damage"
  {:dmg-info (fn [skill]
               (let [cnt (current-psi-charges player-body)
                     [mn mx] (get-current-player-melee-dmg)
                     modifier (* cnt (:dmg-modifier skill))]
                 (variance-val-str
                   [(* modifier mn) (* modifier mx)])))
   :radius 2
   :dmg-modifier 0.2
   :animation :casting
   :show-info-for [:cost]
   :do-skill (fn [entity {:keys [radius dmg-modifier] :as component}]
               (let [posi (get-position entity)
                     cnt (consume-psi-charges entity)
                     duration (+ 1000 (* cnt 333))
                     dmg (* cnt dmg-modifier (rand-int-between (get-current-player-melee-dmg)))
                     hits (get-destructible-bodies posi radius :monster)]
                 (play-sound "explode-erco.wav")
                 (animation-entity
                   :position posi
                   :animation (folder-animation :folder "effects/stomp/" :looping false))
;                 (create-circle-render-effect posi radius (rgbcolor :b 0.6 :a 0.7) 250)
                 (runmap #(stun % duration) hits)
                 (runmap #(deal-dmg dmg %) hits)))})

(def- psi-bolt-pxradius 17)
(defpreload ^:private psi-bolt-frames (folder-frames "effects/psibolt/"))

(defn- do-skill-psi-bolt [entity component]
  (let [cnt (consume-psi-charges entity)
        radius (in-tiles psi-bolt-pxradius)
        dmg (*
              (+ 0.5 (* cnt 0.5))
              (rand-int-between (get-current-player-melee-dmg)))
        posi (get-skill-use-mouse-tile-pos)
        hits (get-destructible-bodies posi radius :monster)]
;    (create-circle-render-effect posi radius (rgbcolor :b 0.6 :a 0.7) 250)
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

(defn- teleport-animation [posi]
  (animation-entity
    :animation (create-animation (spritesheet-frames "effects/blue_teleport.png" 17 17) :frame-duration 100)
    :order :on-ground
    :position posi))
; cooler lightning effect if the animation is at the last position => why did I cut that?

(comment
  (deflearnable-skill teleport
    :manacost 15
    :menu-posi [2 1]
    :mousebutton :right
    :icon "icons/teleport.png"
    :info "Teleports you to the target destination."
    {:show-info-for [:cost]
     :animation :casting
     :check-usable (fn [entity _]
                     (not (blocked-location? (get-mouse-tile-pos) entity (get-movement-type entity))))
     :do-skill (fn [entity component]
                 (play-sound "bfxr_playerteleport.wav")
                 (teleport-animation (get-skill-use-mouse-tile-pos))
                 (teleport entity (get-skill-use-mouse-tile-pos)))}))


