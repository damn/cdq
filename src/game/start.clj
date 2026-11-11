(ns game.start
  (:import (java.awt GridLayout Dimension)
           (javax.swing JPanel JFrame JButton ButtonGroup JRadioButton)
           (java.awt.event KeyEvent ActionListener)
           (org.newdawn.slick Image Color))
  (:require
    [engine.render.image :as g :refer [create-image get-scaled-copy]]
    [engine.render.animation :refer [create-animation]]
    [engine.render.assets :refer [folder-animation folder-frames spritesheet-frames]]
    [engine.render.color :as color :refer [green orange red white]]
    [engine.statebasedgame :refer [init-state-based-game state-based-game]]
    [engine.settings :refer [jar-file?]]
    [engine.core :refer [create-and-start-app-game-container create-sound fullscreen-supported? make-counter play-sound]]
    [utils.core :as utils :refer [translate-to-tile-middle]]
    [utils.numbers :refer [get-ratio lower-than-max? variance-val-str]]
    [utils.coll :refer [safe-merge]]
    [game.session :as sess]
    [game.settings :refer [get-setting in-tiles init-config! screen-height screen-scale screen-width tile-height tile-width]]
    [game.components.core :refer [active add-to-removelist id-entity-map is-player? player-body update-counter!]]
    [game.components.body :refer [blocked-location? bodies-in-range? circle-collides? get-bodies-at-position get-dist-to-player teleport]]
    [game.components.misc :refer [hp-regen-component rotate-to-player rotation-component]]
    [game.components.render :refer [animation-component animation-entity create-line-render-effect create-lines-render-effect image-render-component single-animation-component]]
    [game.components.destructible :refer [calc-effective-spell-dmg deal-dmg explosion-frames get-destructible-bodies get-hp is-dead? set-hp-to-max]]
    [game.components.body-effects-impl :refer [add-psi-charge aoe-armor-reducer consume-psi-charges create-bullettime-effects current-psi-charges dmg-effect max-psi-charges slowdown-effect stun stun-collision-effect]]
    [game.components.active :refer [blocks-component]]
    [game.components.shield :refer [shield-component]]
    [game.components.movement :refer [movement-component projectile-movement-component]]
    [game.components.movement.ai.potential-field :refer [path-to-player-movement potential-field-player-following]]
    [game.components.skills.core :refer [enough-mana? get-skill get-skill-use-mouse-tile-pos is-attacking? is-ready? is-usable? skillmanager-component skillmanager-skill standalone-skill]]
    [game.components.skills.melee :refer [get-current-player-melee-dmg melee-weapon monster-melee-component monster-melee-props player-melee-props]]
    [game.components.sleeping :refer [wake-up]]
    [game.entity.projectile :refer [fire-projectile]]
    [game.entity.nova :refer [nova-effect]]
    [game.item.instance :refer [create-item-body]]
    [game.item.instance-impl :refer [create-rand-item]]
    [game.maps.data :refer [get-current-map-data]]
    [game.maps.minimap :refer [show-on-minimap]]
    [game.utils.geom :refer [direction-vector get-angle-to-position get-touched-tiles in-range? normalise rotate-angle-to-angle vector-from-angle vector2f]]
    [game.utils.random :refer [get-rand-weighted-item if-chance percent-chance rand-int-between]]
    [game.utils.raycast :refer [is-path-blocked? ray-blocked?]]
    [game.monster.defmonster :refer [defmonster get-monster-properties]]
    [game.monster.spawn :refer [try-spawn]]
    game.components.body-render
    game.components.burrow
    game.maps.add
    game.media
    game.entity.teleporters
    [game.components.skills.utils :refer [check-line-of-sight get-player-ranged-vector]]
    [game.player.skill.learnable :refer [deflearnable-skill]]
    [game.state.deferred-preload :refer [deferred-preload-gamestate]]
    [game.state.mainmenu :refer [mainmenu-gamestate]]
    [game.state.loading :refer [loading-gamestate]]
    [game.state.ingame :refer [ingame-gamestate]]
    [game.state.options :refer [options-gamestate]]
    [game.state.minimap :refer [minimap-gamestate]])
  (:gen-class))

;;; monsters.common

;;; movement.ai.homing

(defn- move-and-rotate-to-target-control
  [projectile {:keys [target-body current-angle rotationspeed]} delta]
  (vector-from-angle
    (if-not (get @id-entity-map (:id (meta target-body)))
      current-angle
      (let [angle-to-target (get-angle-to-position (:value (:position @projectile)) (:value (:position @target-body)))
            adjusted-angle (rotate-angle-to-angle current-angle angle-to-target rotationspeed delta)]
        (swap! projectile assoc-in [:movement :current-angle] adjusted-angle)
        adjusted-angle))))

(defn create-homing-movement [speed target-body start-angle rotationspeed move-type]
  (movement-component
    {:control-update move-and-rotate-to-target-control
     :target-body target-body
     :current-angle start-angle
     :rotationspeed rotationspeed}
    speed
    move-type))

;;; movement.ai.ranged-monster

(defn- runaway [body component]
  (cond
    (not (is-usable? body (get-skill body :ranged)))
    (potential-field-player-following body)

    (bodies-in-range? body player-body (:runaway-dist-sqrd component))
    (direction-vector (:value (:position @player-body)) (:value (:position @body)))

    :else
    nil))

(defn- randomise [body _]
  (cond
    (not (is-usable? body (get-skill body :ranged)))
    (potential-field-player-following body)

    :else
    (normalise
      (vector2f
        (if-chance 50 (rand) (- (rand)))
        (if-chance 50 (rand) (- (rand)))))))

(defn- duration-counter-control
  [body {:keys [current-movement-vector create-vectorfn counter] :as component} delta]
  (if (or (nil? current-movement-vector)
          (update-counter! body delta component))
    (let [v (create-vectorfn body component)]
      (swap! body assoc-in [:movement :current-movement-vector] v)
      v)
    current-movement-vector))

(defn ranged-runaway-movement-comp
  ([speed runaway-dist-in-tiles move-type]
    (movement-component
      {:control-update duration-counter-control
       :create-vectorfn runaway
       :counter (make-counter 300)
       :current-movement-vector nil
       :runaway-dist-sqrd (Math/pow runaway-dist-in-tiles 2)}
      speed
      move-type)))

(defn ranged-randomly-moving-comp
  [speed duration move-type]
  (movement-component
    {:control-update duration-counter-control
     :create-vectorfn randomise
     :counter (make-counter duration)
     :current-movement-vector nil}
    speed
    move-type))

; hat nix mit ranged_monster zu tun -> tu woanders hin
; wenn hp wieder max ist (also geheilt wurden -> finished!)
(defn- update-lowhp-runaway [body {:keys [running-away counter] :as c} delta]
  (let [finished (and running-away (update-counter! body delta c))]
    (when finished
      (swap! body assoc-in [:movement :running-away] false))
    (if (and running-away (not finished))
      (direction-vector (:value (:position @player-body)) (:value (:position @body)))
      (potential-field-player-following body))))

(defn lowhp-runaway-movement [speed]
  (movement-component {:control-update update-lowhp-runaway
                       :running-away false
                       :counter (make-counter 0)}
                      speed
                      :ground))

(defn rand-when-low-hp [body]
  (->> body get-hp get-ratio (- 1) (* 100) percent-chance))

(defn lowhp-dealt-dmg-trigger [body lethal]
  (let [move-comp (:movement @body)]
    (when (and (not lethal)
               (not (:running-away move-comp))
               (rand-when-low-hp body))
      (swap! body update-in [:movement]
                  #(-> %
                       (assoc-in [:counter :maxcnt] (* (rand-int-between 3 10) 1000))
                       (assoc-in [:running-away] true))))))

;;; shared monster helpers

(defn- path-to-player-blocked?
  [[sx sy] projectile-pxsize]
  (let [[tx ty] (:value (:position @player-body))
        path-w (in-tiles projectile-pxsize)]
    (is-path-blocked? sx sy tx ty path-w)))

(alter-var-root #'engine.core/fns conj
  (fn []
    (def ^:private redball-frames (folder-frames "effects/red_ball/"))))

(let [maxrange 10
      maxrange-squared (Math/pow maxrange 2)
      pxsize 7]
  (defn redball-projectile-skill-props []
    {:show-cast-bar false
     :check-usable (fn [entity component]
                     (and (not (path-to-player-blocked? (:value (:position @entity)) pxsize))
                          (bodies-in-range? entity player-body maxrange-squared)))
     :do-skill (fn [entity component]
                 (fire-projectile
                   :startbody entity
                   :px-size pxsize
                   :animation (create-animation redball-frames :looping true)
                   :side :monster
                   :hits-side :player
                   :movement (projectile-movement-component (direction-vector (:value (:position @entity)) (:value (:position @player-body))) 80)
                   :hit-effects [(dmg-effect [3 5])
                                 (stun-collision-effect 10 150)]
                   :maxrange maxrange))}))

(defn ranged-component
  [& {:keys [cooldown attacktime state-blocks]
      :or {state-blocks {:attacking :movement}}}]
  (standalone-skill
    :stype :ranged
    :cooldown cooldown
    :attacktime attacktime
    :state-blocks state-blocks
    :props (redball-projectile-skill-props)))

(alter-var-root #'engine.core/fns conj
  (fn []
    (def ^:private monsterdie-frames (folder-frames "effects/monsterexplosion/"))))

(defn monster-die-effect [body]
  (animation-entity
    :animation (create-animation monsterdie-frames)
    :position (:value (:position @body))))

(def ^:private monster-drop-table
  {{"Grenade" 1
    "Battle-Drugs" 1} 2
   {
    ;"Mana-Potion" 2
    "Heal-Potion" 4
    "Big-Mana-Potion" 1
    "Big-Heal-Potion" 4} 9})

(defn default-monster-death [body & {sound :sound :or {sound true}}]
  (monster-die-effect body) ; "effect" was ist das? sound/animation oder was
  (when sound
    (play-sound "bfxr_defaultmonsterdeath.wav"))
  (let [position (:value (:position @body))]
    (if-chance 20 ; TODO when-chance
      (let [item-name (get-rand-weighted-item
                        (get-rand-weighted-item monster-drop-table))]
        (create-item-body position item-name)))
    (if-chance 6
      (create-rand-item position :max-lvl (:rand-item-max-lvl (get-current-map-data))))))

(defn death-trigger [f] {:type :death-trigger :destruct f})

(defn default-death-trigger [] (death-trigger default-monster-death))

(alter-var-root #'engine.core/fns conj
  (fn []
    (def ^:private bigger-explosion-frames (spritesheet-frames "effects/explosn.png" 20 20))))
(alter-var-root #'engine.core/fns conj
  (fn []
    (def ^:private big-explosion-frames (spritesheet-frames "effects/expbig.png" 40 40))))

(defn rand-posis-hit-effect
  "pixel distance from center of boss for explosions"
  [body hit-posis & {big-explosion :big-explosion}]
  (doseq [[x y] (take
                  (int (/ (count hit-posis) 2))
                  (shuffle hit-posis))
          :let [vx (/ x tile-width)
                vy (/ y tile-height)
                [x y] (:value (:position @body))
                explosion-posi [(+ x vx) (+ y vy)]]]
    (animation-entity
      :position explosion-posi
      :animation (create-animation (if big-explosion
                                     big-explosion-frames
                                     (if-chance 50 explosion-frames bigger-explosion-frames))))))

(defn big-body-hit-effect
  [hit-posis]
  {:type :hit-effect
   :trigger (fn [body] (rand-posis-hit-effect body hit-posis))})
(defn get-free-posis [body position half-w half-h]
  (remove #(blocked-location? % body)
          (map translate-to-tile-middle
               (get-touched-tiles position half-w half-h))))

(defn monsterteleport-animation [position]
  (animation-entity
    :animation (create-animation (spritesheet-frames "effects/red_teleport.png" 17 17) :frame-duration 100)
    :order :on-ground
    :position position))

; TODO mach irgendwo in SICHTBAREN cells von player
; nur dann möglich wenn er eine findet
; set-hp-to-max macht noch ne animation hätt ich net erwartet
; wenn lethal dmg dann hier healing aber trotzdem death -> aber sonst fast unkillbar wohl...
(defn teleport-and-heal-when-low-hp [body lethal]
  (when (and (not lethal)
             (rand-when-low-hp body)
             (zero? (rand-int 5))) ; not too strong ... only 1 in 5
    (play-sound "bfxr_monstercast.wav")
    (let [free-posis (get-free-posis body (:value (:position @body)) 6 3)]
      (when (seq free-posis)
        (let [posi (rand-nth free-posis)]
          (teleport body posi)
          (monsterteleport-animation posi)))
      (set-hp-to-max body)))) ; danach hp-to-max damit an neuer posi +hp string steht

(defn normal-monster-melee []
  (monster-melee-component :cooldown 500
                           :attacktime 250
                           :hit-sound (create-sound "slash.wav")
                           :target-id (:id (meta player-body))))

(def heal-radius 6)

(defn get-healable-monsters-around [healer]
  (remove #(= % healer)
          (get-destructible-bodies (:value (:position @healer)) heal-radius :monster)))

;;; monsters

(defmonster ranged {:hp 0.6 :armor 5 :pxw 15 :pxh 15}
  (default-death-trigger)
  (ranged-runaway-movement-comp 24 (rand-int-between 2 6) :ground)
  (rotation-component)
  (image-render-component (create-image "opponents/core_raider.png"))
  (ranged-component :cooldown (rand-int-between 2000 2500) :attacktime 500))

(defn- monsters-need-healing? [healer]
  (some #(lower-than-max? (get-hp %))
        (get-healable-monsters-around healer)))

(defn- choose-active-healer [entity skillmanager delta]
  (let [skills (:skills skillmanager)
        heal-spell (:healing skills)
        ranged-weapon (:ranged skills)]
    (if (and (update-counter! entity delta skillmanager)
             (enough-mana? heal-spell skillmanager)
             (is-ready? heal-spell)
             (monsters-need-healing? entity)
             (zero? (rand-int 5)))
      :healing
      :ranged)))

(defn- heal-nearby-monsters
  "returns the healed monsters"
  [healer]
  (doall (remove nil?
                 (map #(when (lower-than-max? (get-hp %))
                         (set-hp-to-max %) %)
                      (get-healable-monsters-around healer)))))

(defn- create-healer-skillmanager []
  (let [ranged-weapon (skillmanager-skill
                        :stype :ranged
                        :cooldown 4000
                        :attacktime 400
                        :cost 0
                        (redball-projectile-skill-props))
        heal-spell (skillmanager-skill
                     :stype :healing
                     :cooldown (rand-int-between 2000 3000)
                     :attacktime 500
                     :cost 0
                     {:shoot-sound "bfxr_healmonsters.wav"
                      :do-skill (fn [healer component]
                                  (let [healed-monsters (heal-nearby-monsters healer)]
                                    (create-lines-render-effect healer healed-monsters 500)))})]
    (skillmanager-component
      :rotatefn rotate-to-player
      :choosefn choose-active-healer
      :mana 0
      :skills [heal-spell ranged-weapon]
      (blocks-component {:attacking :movement})
      ; heilen trotzdem alle auf 1mal da 1sek attacktime?
      {:counter (make-counter 500)})))

; TODO also use cached-monsters-around ?
(defmonster healer {:hp 3 :armor 0 :pxw 15 :pxh 15}
  (default-death-trigger)
  (ranged-runaway-movement-comp 30 (rand-int-between 1 3) :ground)
  (rotation-component)
  (single-animation-component
    (folder-animation :folder "opponents/healer/" :duration 1000 :looping true))
  (create-healer-skillmanager))

(defmonster little-bot {:hp 0.5 :armor 0 :pxw 9 :pxh 9}
  (death-trigger (fn [body]
                   (play-sound "bfxr_defaultmonsterdeath.wav")
                   (monster-die-effect body)))
  (path-to-player-movement 15)
  (rotation-component)
  (image-render-component (create-image "opponents/littlebot.png"))
  (monster-melee-component
    :cooldown 1000
    :attacktime 100
    :hit-sound (create-sound "slash.wav")
    :target-id (:id (meta player-body))))

(defmonster littlespider {:hp 1 :armor 7 :pxw 13 :pxh 13}
  (default-death-trigger)
  (path-to-player-movement 47)
  (rotation-component)
  (single-animation-component
    (folder-animation :folder "opponents/littlespider/" :duration 300 :looping true))
  (normal-monster-melee))

(defmonster xploding-drone {:hp 1.5 :armor 5 :pxw 15 :pxh 15}
  (death-trigger (fn [this-body]
                   (default-monster-death this-body :sound false) ; TODO this strange sound false and play-sound ... => default monster dead more than 1 thing...
                   (play-sound "bfxr_dronedeath.wav")
                   (nova-effect ; TODO two novas because different dmg to player/monster ... => im dealt dmg trigger berücksichtigen?
                     :position (:value (:position @this-body))
                     :duration 150
                     :maxradius 2
                     :affects-side [:player]
                     :dmg [20 20]
                     :animation (folder-animation :folder "effects/xpldrone/" :duration 150 :looping false))
                   (nova-effect
                     :position (:value (:position @this-body))
                     :duration 150
                     :maxradius 2
                     :affects-side [:monster]
                     :dmg [4 8]
                     :animation (folder-animation :folder "effects/xpldrone/" :duration 150 :looping false))))
  (path-to-player-movement 13)
  (rotation-component)
  (single-animation-component
    (folder-animation :folder "opponents/xplodingdrone/" :duration 500 :looping true))
  (monster-melee-component
    :cooldown 500
    :attacktime 500
    :hit-sound (create-sound "slash.wav")
    :target-id (:id (meta player-body))))

(let [posis [[9 -11] [-1 -11] [-13 -7] [-9 2] [3 3] [3 15] [-5 14] [7 17] [-11 5] [-12 14]]]
  (defmonster research-station {:hp 10 :armor 25 :pxw 43 :pxh 43}
    (big-body-hit-effect posis)
    (death-trigger (fn [body]
                     (play-sound "bfxr_stationdeath.wav")
                     (rand-posis-hit-effect body posis :big-explosion true)))
    (show-on-minimap orange)
    (single-animation-component
      (folder-animation :folder "opponents/station/" :duration 500 :looping true))))

(alter-var-root #'engine.core/fns conj
  (fn []
    (def ^:private mine-explosion-frames (folder-frames "effects/mine/"))))

(defmonster mine {:hp 1 :armor 0 :pxw 15 :pxh 15}
  (death-trigger (fn [this-body]
                   (play-sound "bfxr_minedeath.wav")
                   (nova-effect
                     :position (:value (:position @this-body))
                     :duration 300
                     :maxradius 4
                     :affects-side [:player]
                     :dmg [30 40]
                     :animation (create-animation mine-explosion-frames))
                   (nova-effect
                     :position (:value (:position @this-body))
                     :duration 300
                     :maxradius 4
                     :affects-side [:monster]
                     :dmg [6 8]
                     :animation (create-animation mine-explosion-frames))))
  (image-render-component (create-image "opponents/mine.png"))
  (single-animation-component
    (create-animation (spritesheet-frames "effects/red_glow.png" 32 32)
                      :frame-duration 150
                      :looping true)
    :order :on-ground))

(defmonster skull-chainsaw {:hp 1.3 :armor 4 :pxw 15 :pxh 15}
  (default-death-trigger)
  (lowhp-runaway-movement 50)
  {:type :dealt-dmg-trigger :do lowhp-dealt-dmg-trigger}
  (rotation-component)
  (single-animation-component
    (folder-animation :folder "opponents/teleportraider/" :duration 500 :looping true))
  (normal-monster-melee))

(defmonster storagebox {:hp 1 :armor 25 :pxw 41 :pxh 41}
  ;(default-death-trigger)
  (image-render-component (create-image "opponents/storage1.png")))

(defmonster armored-skull {:hp 2 :armor 65 :pxw 15 :pxh 15}
  (default-death-trigger)
  (path-to-player-movement 13)
  (rotation-component)
  (hp-regen-component 2)
  (image-render-component (create-image "opponents/coredemonhand.png"))
  (normal-monster-melee))

(defmonster armored-skull2 {:hp 1 :armor 23 :pxw 15 :pxh 15}
  (default-death-trigger)
  (path-to-player-movement 13)
  (rotation-component)
  (hp-regen-component 5)
  (image-render-component (create-image "opponents/vorticularcutlass.png"))
  (normal-monster-melee))

(defmonster big-skull-chainsaw {:hp 3 :armor 25 :pxw 15 :pxh 15}
  (default-death-trigger)
  (path-to-player-movement 25)
  (rotation-component)
  (hp-regen-component 1)
  (image-render-component (create-image "opponents/vorticularcutlassiv.png"))
  (normal-monster-melee))

(defmonster big-teleporting-melee {:hp 5 :armor 15 :pxw 15 :pxh 15}
  (default-death-trigger)
  (path-to-player-movement 8)
  (rotation-component)
  (single-animation-component
    (folder-animation :folder "opponents/corebomber/" :duration 300 :looping true))
  (normal-monster-melee)
  (standalone-skill
    :stype :teleporting
    :cooldown 1000
    :attacktime 600
    :props {:show-cast-bar true
            :shoot-sound "bfxr_bigmeleeteleport.wav"
            :check-usable (fn [entity _]
                            (let [dist (get-dist-to-player entity)]
                              (or (not dist) (>= dist 80))))
            :do-skill (fn [entity component]
                        (let [old-posi (:value (:position @entity))
                              posis (get-free-posis entity (:value (:position @player-body)) 2 2)]
                          (when (not-empty posis)
                            (let [posi (rand-nth posis)]
                              (teleport entity posi)
                              (monsterteleport-animation posi)
                              (create-line-render-effect posi old-posi 140 :color white)))))}))
; TODO shoot-sound?

(defmonster burrower {:hp 2.5 :armor 20 :pxw 15 :pxh 15}
  (default-death-trigger)
  (path-to-player-movement 30)
  (rotation-component)
  (image-render-component (create-image "opponents/burrower.png"))
  (normal-monster-melee)
  (game.components.burrow/burrow-component))

(defmonster shield-turret {:hp 1.5 :armor 15 :pxw 15 :pxh 15}
  (path-to-player-movement 13)
  (default-death-trigger)
  (shield-component 1500)
  (rotation-component)
  (image-render-component (create-image "opponents/counternegativeenergyturret.png"))
  (ranged-component :cooldown 4000 :attacktime 500))

(defmonster mage-skull {:hp 4.8 :armor 0 :pxw 15 :pxh 15}
  (default-death-trigger)
  (ranged-runaway-movement-comp 26 (rand-int-between 3 4) :ground)
  (hp-regen-component 5)
  (rotation-component)
  (image-render-component (create-image "opponents/core_predator.png"))
  (ranged-component :cooldown 3000 :attacktime 50))

(let [attacktime 600
      folder "opponents/fly/"]
  (defmonster fly {:hp 1 :armor 0 :pxw 9 :pxh 9}
    (default-death-trigger)
    (ranged-randomly-moving-comp 32 1000 :ground)
    (animation-component (fn [body]
                           (if (is-attacking? (:ranged @body)) :attack :default))
                         {:attack (folder-animation :folder folder :prefix "attack" :duration attacktime :looping false)
                          :default (create-animation [(create-image (str folder "fly.png"))])})
    (ranged-component :cooldown 2000 :state-blocks {}))) ; move+shoot

(defn cache-nearby-monsters-component []
  (safe-merge
    {:type :cache-nearby-monsters
     :counter (make-counter 1000)}
    (active [delta {:keys [counter] :as c} entity]
      (when (update-counter! entity delta c)
        (swap! entity assoc-in [(:type c) :nearby-monsters]
                   (doall (get-healable-monsters-around entity)))))))

; TODO not checking if ray-blocked ---> can heal through walls like other healer
; navigation meshes would make ray-blocked much simpler if in the same polygon/area
(defn- healing-required-and-allowed? [entity healer radius-squared]
  (and (get @id-entity-map (:id (meta entity)))
       (not (is-dead? entity))
       (lower-than-max? (get-hp entity))
       (bodies-in-range? entity healer radius-squared)))

; TODO mehr hervorheben anstatt drawLine vlt so ein "beam" strahl... grün mit weiss als kontrast (not in prototype stage -> do another time!)
; rotating to which monster is healed?
(let [healradius-squared (* heal-radius heal-radius)]
  (defmonster instant-healer {:hp 2 :armor 20 :pxw 14 :pxh 14}
    (default-death-trigger)
    (path-to-player-movement 15)
    (rotation-component)
    (image-render-component (create-image "opponents/coreturret.png"))
    (cache-nearby-monsters-component)
    (monster-melee-component :cooldown 500
                             :attacktime 100
                             :hit-sound (create-sound "slash.wav")
                             :target-id (:id (meta player-body)))
    (standalone-skill
      :stype :instantheal
      :cooldown 1000
      :attacktime 150
      :props {:shoot-sound "bfxr_instanthealer_heal.wav"
              :check-usable (fn [entity _]
                              (when-let [cached (:nearby-monsters (:cache-nearby-monsters @entity))]
                                (when-let [needs-heal (first (sort-by #(:current (get-hp %))
                                                                      (filter #(healing-required-and-allowed? % entity healradius-squared)
                                                                              cached)))]
                                  (swap! entity assoc-in [:instantheal :needs-heal] needs-heal)
                                  true)))
              :do-skill (fn [healer {needs-heal :needs-heal :as component}]
                          (when (healing-required-and-allowed? needs-heal healer healradius-squared)
                            (set-hp-to-max needs-heal)
                            (create-line-render-effect (:value (:position @healer)) (:value (:position @needs-heal)) 200 :color green)))})))

(def ^:private monster-nova-radius 4)

(defn- player-in-nova-range? [monster]
  (circle-collides? (:value (:position @monster)) monster-nova-radius player-body))

(defn- choose-active-nova-melee [entity skillmanager delta]
  (let [skills (:skills skillmanager)
        melee (:melee skills)
        nova (:monster-nova skills)]
    (if
      (and
        (enough-mana? nova skillmanager)
        (is-ready? nova)
        (player-in-nova-range? entity)
        (not (ray-blocked? (:value (:position @entity)) (:value (:position @player-body))))
        (zero? (rand-int 240)))
      :monster-nova
      :melee)))

(alter-var-root #'engine.core/fns conj
  (fn []
    (def ^:private monster-nova-frames (folder-frames "effects/monsternova/"))))

(defn- create-nova-melee-skillmanager []
  (let [melee-weapon (skillmanager-skill
                       :stype :melee
                       :cooldown 1000
                       :attacktime 500
                       :cost 0
                       (monster-melee-props (:id (meta player-body)) (melee-weapon [3 8] (create-sound "slash.wav"))))
        monster-nova (skillmanager-skill
                       :stype :monster-nova
                       :cooldown (rand-int-between 1000 2000)
                       :attacktime 1500
                       :cost 1
                       {:shoot-sound "bfxr_monstercast.wav"
                        :do-skill (fn [entity component]
                                    (nova-effect
                                      :position (:value (:position @entity))
                                      :duration 400
                                      :maxradius monster-nova-radius
                                      :affects-side :player
                                      :dmg [18 22]
                                      :animation (create-animation monster-nova-frames)))})]
    (skillmanager-component
      :choosefn choose-active-nova-melee
      :mana (rand-int-between 2 4)
      :skills [melee-weapon monster-nova]
      (blocks-component {:attacking :movement}))))

(defmonster nova-melee {:hp 2.2 :armor 7 :pxw 15 :pxh 15}
  (default-death-trigger)
  (path-to-player-movement 32)
  (single-animation-component
    (folder-animation :folder "opponents/gravturret/" :duration 1000 :looping true))
  (create-nova-melee-skillmanager))

(defmonster slowdown-caster {:hp 2 :armor 25 :pxw 12 :pxh 12}
  (default-death-trigger)
  (path-to-player-movement 20)
  (single-animation-component
    (folder-animation :folder "opponents/slowdowncaster/" :duration 200 :looping true))
  (standalone-skill        ; TODO für nen ranged skill mit custom hit-effects & movement sehr kompliziert!!
    :stype :slowdown-ranged
    :cooldown 2000
    :attacktime 1000
    :props {:show-cast-bar true
            :do-skill (fn [entity ranged-comp]
                        (let [speed 84
                              rotation-speed 0.1
                              starting-angle (get-angle-to-position (:value (:position @entity)) (:value (:position @player-body)))]
                          (fire-projectile
                            :startbody entity
                            :px-size 10
                            :animation (folder-animation :folder "effects/slowdownprojectile/" :duration 700 :looping true)
                            :side :monster
                            :hits-side :player
                            :movement (create-homing-movement speed player-body starting-angle rotation-speed :air)
                            :hit-effects [(dmg-effect [5 6])
                                          (slowdown-effect 1)]
                            :maxtime 16000)))}))

;TODO ray-blocked = in-sight -> cache it? expensive?
;TODO do-skill spritesheet-frames expensive without preloading the frames?
(let [maxrange-squared (* 10 10)
      attacktime 1000]
  (defmonster ray-shooter {:hp 1 :armor 25 :pxw 15 :pxh 15}
    (default-death-trigger)
    (path-to-player-movement 22)
    {:type :dealt-dmg-trigger :do teleport-and-heal-when-low-hp}
    (single-animation-component
      (folder-animation :folder "opponents/harvesterexoshield/" :duration 200 :looping true))
    (standalone-skill
      :stype :rayshoot
      :cooldown (rand-int-between 1700 2300)
      :attacktime attacktime
      :state-blocks {:attacking :movement} ; important because line rendered from these posis! so dont move when attacking!
      :props {:show-cast-bar false
              :shoot-sound "bfxr_rayshooterhit.wav"
              :target-posi (atom nil) ; REMOVE
              :check-usable (fn [entity component]
                              (let [shooter-posi (:value (:position @entity))
                                    target-posi (:value (:position @player-body))]
                                (when (and (in-range? shooter-posi target-posi maxrange-squared)
                                           (not (ray-blocked? shooter-posi target-posi)))
                                  (reset! (:target-posi component) target-posi)
                                  (play-sound "bfxr_powerup.wav") ; length of sound ~ length of attacktime would be nice
                                  ; TODO line render only as long as monster is alive would also make more sense ? ...
                                  ; also when slowed down ... attacktime changes ...
                                  ; => just like a component of an entity slowed down/lives with it
                                  (create-line-render-effect shooter-posi target-posi attacktime :color white :thin true)
                                  true)))
              :do-skill (fn [entity component]
                          (let [target @(:target-posi component)]
                            (if (some is-player? (get-bodies-at-position target))
                              (deal-dmg [10 15] player-body)
                              (animation-entity :position target
                                                :animation (create-animation (spritesheet-frames "effects/12_16_littleexpl.png" 12 16) :frame-duration 100)
                                                :order :on-ground))
                            (create-line-render-effect (:value (:position @entity)) target 70 :color red)))})))

(defn- rand-spawn-monster [monstertype position areahw areahh]
  (let [{:keys [half-w half-h]} (get-monster-properties monstertype)
        spawn-positions (remove #(blocked-location? % half-w half-h :ground) ; get-free-posis duplicate?
                                (map translate-to-tile-middle
                                     (get-touched-tiles position areahw areahh)))]
    (when (seq spawn-positions)
      (let [target (rand-nth spawn-positions)]
        (wake-up (try-spawn target monstertype)) ; TODO try-spawn also checks if blocked °_°
        (monsterteleport-animation target)
        (create-line-render-effect position target 70 :color white)))))

(defn- fire-boss-ranged-projectile [body speed starting-angle rotation-speed effects]
  (fire-projectile
    :startbody body
    :px-size 10
    :animation (folder-animation :folder "effects/bossball/" :duration 700 :looping true)
    :side :monster
    :hits-side :player
    :movement (create-homing-movement speed player-body starting-angle rotation-speed :air)
    :hit-effects effects
    :maxtime 12000))

(alter-var-root #'engine.core/fns conj
  (fn []
    (def ^:private boss-explosion (folder-frames "effects/bossexplosion/"))))

(defmonster first-boss {:hp 20 :armor 50 :pxw 33 :pxh 63}
  (big-body-hit-effect [[12 -15] [-8 -15] [0 -23] [1 -6] [-9 -4] [-8 4] [8 8] [-8 15] [3 23] [-6 25]])
  ;(light-component :color (rgbcolor :r 0.8 :g 0.2 :b 0.2) :intensity 1 :radius 12)
  (death-trigger (fn [body]
                   (play-sound "bfxr_bossdeath.wav")
                   (animation-entity
                     :animation (create-animation boss-explosion)
                     :position (:value (:position @body)))
                   (create-item-body (:value (:position @body)) "The Golden Banana")

                   ; no lvl after this => no need to spawn an item!
                   ; (create-rand-item (:value (:position @body)) :max-lvl (:rand-item-max-lvl (get-current-map-data)))

                   (dorun (map add-to-removelist (:projectiles (:boss-ranged @body))))))
  (standalone-skill
    :stype :monster-spawner
    :cooldown 2000
    :attacktime 500
    :props {:show-cast-bar true
            :shoot-sound "bfxr_monstercast.wav"
            :do-skill (fn [entity component]
                        (rand-spawn-monster :little-bot (:value (:position @entity)) 6 3))})
  (standalone-skill
    :stype :boss-ranged
    :cooldown 3200
    :attacktime 3000
    :props {:show-cast-bar true
            :projectiles []
            :dmg [5 15]
            :shoot-sound "bfxr_monstercast.wav"
            :do-skill (fn [entity ranged-comp]
                        (let [speed 48 ; ca. player move speed
                              rotation-speed 0.05
                              effects [(dmg-effect (:dmg ranged-comp)) (stun-collision-effect 75 300)]]
                          (swap! entity update-in [:boss-ranged :projectiles] concat
                                      (doall
                                        (map #(fire-boss-ranged-projectile entity speed % rotation-speed effects)
                                             [0 90 180 270])))))})
  (movement-component ; TODO komische args ...
    {:control-update (fn [body _ _] (direction-vector (:value (:position @body)) (:value (:position @player-body))))}
    12
    :ground)
  (single-animation-component ; TODO gleich folder-animation auchnoch reinpacken in single-animation-component?
    (folder-animation :folder "opponents/boss/" :duration 300 :looping true)))

(defmonster test-hunter {:hp 1.5 :armor 7 :pxw 47 :pxh 47}
  (path-to-player-movement 72)
  (rotation-component)
  (image-render-component (create-image "opponents/coredemonhand.png")))

;;; player.skills

(defn dmg-info-player-spell [skill]
  (variance-val-str
    (calc-effective-spell-dmg
      (:dmg skill)
      (:percent-modify-spell (:item-boni @player-body)))))

(def curse-infostr "Curse\nOnly one curse is active at a time\n")

(alter-var-root #'engine.core/fns conj
  (fn []
    (def ^:private curse-frames (map #(get-scaled-copy % 0.3) (folder-frames "effects/curse/")))))

(defn create-curse-effect [[x y]]
  (play-sound "bfxr_curse.wav")
  (animation-entity
    :position [x (- y (in-tiles (/ 80 3)))]
    :animation (create-animation curse-frames)))

(alter-var-root #'engine.core/fns conj
  (fn []
    (def ^:private projectile-frames (folder-frames "effects/energyball/"))))

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

(defn- psi-charge-hit-effect [_]
  (let [{:keys [attack-bonus movement-bonus]} (:psi-charger game.player.skill.learnable/learnable-skills)]
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

(alter-var-root #'engine.core/fns conj
  (fn []
    (def ^:private nova-frames (folder-frames "effects/nova/"))))

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
                 :position (:value (:position @entity))
                 :duration 200
                 :maxradius radius
                 :affects-side :monster
                 :dmg dmg
                 :is-player-spell true
                 :animation (create-animation nova-frames)))})

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
               (let [posi (:value (:position @entity))
                     cnt (consume-psi-charges entity)
                     duration (+ 1000 (* cnt 333))
                     dmg (* cnt dmg-modifier (rand-int-between (get-current-player-melee-dmg)))
                     hits (get-destructible-bodies posi radius :monster)]
                 (play-sound "explode-erco.wav")
                 (animation-entity
                   :position posi
                   :animation (folder-animation :folder "effects/stomp/" :looping false))
                 (dorun (map #(stun % duration) hits))
                 (dorun (map #(deal-dmg dmg %) hits))))})

(def ^:private psi-bolt-pxradius 17)
(alter-var-root #'engine.core/fns conj
  (fn []
    (def ^:private psi-bolt-frames (folder-frames "effects/psibolt/"))))

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
    (dorun (map #(deal-dmg dmg %) hits))))

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

;;; serialization (was game.serialization)

(defn- relevant-data [^Image image]
  {:transparent-color (.transparent image)
   :iref              (.getResourceReference image)
   :width             (.getWidth          image)
   :height            (.getHeight         image)
   :texture-width     (.getTextureWidth   image)
   :texture-height    (.getTextureHeight  image)
   :texture-offset-x  (.getTextureOffsetX image)
   :texture-offset-y  (.getTextureOffsetY image)
   :texture (let [^org.newdawn.slick.opengl.Texture texture (.getTexture image)]
              {:texture-width  (.getTextureWidth  texture)
               :texture-height (.getTextureHeight texture)})})

(defn- save-image-data [image]
  (let [{:keys [iref
                transparent-color
                width
                height
                texture-width
                texture-height
                texture-offset-x
                texture-offset-y
                texture]}
        (relevant-data image)

        in-px-w (fn [texels] (int (* texels (:texture-width  texture))))
        in-px-h (fn [texels] (int (* texels (:texture-height texture))))
        sw (in-px-w texture-width)
        sh (in-px-h texture-height)
        sx (in-px-w texture-offset-x)
        sy (in-px-h texture-offset-y)]
    {:iref              iref
     :transparent-color transparent-color
     :sub-bounds        [sx sy sw sh]
     :bounds            [width height]}))

(defn- recreate-image [{iref              :iref
                        transparent-color :transparent-color
                        [sx sy sw sh]     :sub-bounds
                        [width height]    :bounds}]
  (-> (g/create-image iref :transparent transparent-color)
      (g/get-sub-image sx sy sw sh)
      (g/get-scaled-copy width height)))

(defmethod sess/write-to-disk Image [^Image i]
  (with-meta (save-image-data i)
             {:pr :image}))

(defmethod sess/load-from-disk :image [data]
  (recreate-image data))

(defmethod sess/write-to-disk Color [^Color c]
  (with-meta [(.r c) (.g c) (.b c) (.a c)]
             {:pr :color}))

(defmethod sess/load-from-disk :color [[r g b a]]
  (color/rgbcolor :r r :g g :b b :a a))

;;; resolution setup (was game.resolutionsetup)

(defn- makeframe []
  (doto (JFrame. "Resolution Setup")
    (.setVisible true)
    (.setResizable false)
    (.setLocationRelativeTo nil) ; center
    (.setPreferredSize (Dimension. 300 150))
    (.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)))

(defn- fits-in-desktop? [w h]
  (let [displaymode (org.lwjgl.opengl.Display/getDesktopDisplayMode)]
    (and (<= w (.getWidth displaymode))
         (<= h (.getHeight displaymode)))))

(defn- make-button [[scale width height] & {fs :fs}]
  {:scale scale
   :width width
   :height height
   :button (JRadioButton. (str width "*" height (when fs " fullscreen")))
   :fullscreen fs})

(defn- make-startbutton [start-the-game buttons frame]
  (let [startbutton (JButton. "Start")]
    (doto startbutton
      (.setMnemonic  KeyEvent/VK_S)
      (.addActionListener
        (reify ActionListener
          (actionPerformed [this e]
            (when-let [{:keys [scale fullscreen]} (first (filter #(.isSelected (:button %)) buttons))]
              (intern 'game.settings 'screen-scale scale)
              (.setEnabled startbutton false)
              (.dispose frame)
              (start-the-game fullscreen))))))))

(defn- disable-too-big-resolutions [buttons]
  (dorun
      (map #(when-not (fits-in-desktop? (:width %) (:height %))
              (.setEnabled (:button %) false))
           buttons)))

(defn- resolution-setup-frame [start-the-game]
  (let [scales-res (map (fn [s] [s (* screen-width s) (* screen-height s)]) [1 2 3 4])
        buttons (sort-by :scale
                         (concat (map make-button scales-res)
                                 (map #(make-button % :fs true)
                                      (filter #(fullscreen-supported? (% 1) (% 2)) scales-res))))
        panel (JPanel. (GridLayout. 0 1))
        buttongroup (ButtonGroup.)
        frame (makeframe)
        startbutton (make-startbutton start-the-game buttons frame)]
    (disable-too-big-resolutions buttons)
    (let [recommended (:button (first (filter #(= (:scale %) 3) buttons)))]
      (.setSelected (if (.isEnabled recommended)
                      recommended
                      (:button (first (filter #(.isEnabled (:button %)) (reverse buttons)))))
        true))
    (dorun (map #(.add buttongroup (:button %)) buttons))
    (dorun (map #(.add panel (:button %)) buttons))
    (.add panel startbutton)
    (.add frame panel)
    (.pack frame)))

;;; boot

(defn- start-the-game [fullscreen]
  (init-state-based-game "Cyber-Dungeon-Quest"
    [deferred-preload-gamestate
     mainmenu-gamestate
     loading-gamestate
     ingame-gamestate
     options-gamestate
     minimap-gamestate])
  (create-and-start-app-game-container
    :game state-based-game
    :width screen-width
    :height screen-height
    :scale screen-scale
    :full-screen fullscreen
    :show-fps (get-setting :show-fps)
    :lock-framerate false))

(defn start [& {:keys [config]}]
  (let [development-mode (if-let [dev (System/getenv "development")]
                           (Boolean/valueOf dev)
                           (not jar-file?))
        file (cond config config
                   development-mode "config/development.clj"
                   :else "config/production.clj")]
    (utils/log "Using config file: " file)
    (init-config! file))
  (if (get-setting :show-resolution-setup)
    (resolution-setup-frame start-the-game)
    (start-the-game false)))

(defn -main []
  (start))
