(ns game.monsters.common
  (:require
    [engine.core :refer [create-sound defpreload make-counter play-sound]]
    [engine.render :refer [create-animation folder-frames spritesheet-frames]]
    [utils.core :refer [assoc-in! get-ratio translate-to-tile-middle update-in!]]
    [game.settings :refer [in-tiles tile-height tile-width]]
    [game.components.core :refer [create-comp exists? get-component get-id get-position player-body update-counter!]]
    [game.components.body :refer [blocked-location? bodies-in-range? teleport]]
    [game.components.render :refer [animation-entity]]
    [game.components.destructible :refer [explosion-frames get-destructible-bodies get-hp set-hp-to-max]]
    [game.components.body-effects-impl :refer [dmg-effect stun-collision-effect]]
    [game.components.movement :refer [movement-component projectile-movement-component]]
    [game.components.movement.ai.potential-field :refer [potential-field-player-following]]
    [game.components.skills.core :refer [get-skill is-usable? standalone-skill]]
    [game.components.skills.melee :refer [monster-melee-component]]
    [game.entity.projectile :refer [fire-projectile]]
    [game.item.instance :refer [create-item-body]]
    [game.item.instance-impl :refer [create-rand-item]]
    [game.maps.data :refer [get-current-map-data]]
    [game.utils.geom :refer [get-angle-to-position get-touched-tiles get-vector-away-from-player get-vector-to-player normalise rotate-angle-to-angle vector-from-angle vector2f]]
    [game.utils.random :refer [get-rand-weighted-item if-chance percent-chance rand-int-between]]
    [game.utils.raycast :refer [is-path-blocked?]]))

;;; movement.ai.homing

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

;;; movement.ai.ranged-monster

(defn- runaway [body component]
  (cond
    (not (is-usable? body (get-skill body :ranged)))
    (potential-field-player-following body)

    (bodies-in-range? body player-body (:runaway-dist-sqrd component))
    (get-vector-away-from-player body)

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
      (assoc-in! body [:movement :current-movement-vector] v)
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
      (assoc-in! body [:movement :running-away] false))
    (if (and running-away (not finished))
      (get-vector-away-from-player body)
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
  (let [move-comp (get-component body :movement)]
    (when (and (not lethal)
               (not (:running-away move-comp))
               (rand-when-low-hp body))
      (update-in! body [:movement]
                  #(-> %
                       (assoc-in [:counter :maxcnt] (* (rand-int-between 3 10) 1000))
                       (assoc-in [:running-away] true))))))

;;; shared monster helpers

(defn- path-to-player-blocked?
  [[sx sy] projectile-pxsize]
  (let [[tx ty] (get-position player-body)
        path-w (in-tiles projectile-pxsize)]
    (is-path-blocked? sx sy tx ty path-w)))

(defpreload ^:private redball-frames (folder-frames "effects/red_ball/"))

(let [maxrange 10
      maxrange-squared (Math/pow maxrange 2)
      pxsize 7]
  (defn redball-projectile-skill-props []
    {:show-cast-bar false
     :check-usable (fn [entity component]
                     (and (not (path-to-player-blocked? (get-position entity) pxsize))
                          (bodies-in-range? entity player-body maxrange-squared)))
     :do-skill (fn [entity component]
                 (fire-projectile
                   :startbody entity
                   :px-size pxsize
                   :animation (create-animation redball-frames :looping true)
                   :side :monster
                   :hits-side :player
                   :movement (projectile-movement-component (get-vector-to-player entity) 80)
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

(defpreload ^:private monsterdie-frames (folder-frames "effects/monsterexplosion/"))

(defn monster-die-effect [body]
  (animation-entity
    :animation (create-animation monsterdie-frames)
    :position (get-position body)))

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
  (let [position (get-position body)]
    (if-chance 20 ; TODO when-chance
      (let [item-name (get-rand-weighted-item
                        (get-rand-weighted-item monster-drop-table))]
        (create-item-body position item-name)))
    (if-chance 6
      (create-rand-item position :max-lvl (:rand-item-max-lvl (get-current-map-data))))))

(defn death-trigger [f] (create-comp :death-trigger {:destruct f}))

(defn default-death-trigger [] (death-trigger default-monster-death))

(defpreload ^:private bigger-explosion-frames (spritesheet-frames "effects/explosn.png" 20 20))
(defpreload ^:private big-explosion-frames (spritesheet-frames "effects/expbig.png" 40 40))

(defn rand-posis-hit-effect
  "pixel distance from center of boss for explosions"
  [body hit-posis & {big-explosion :big-explosion}]
  (doseq [[x y] (take
                  (int (/ (count hit-posis) 2))
                  (shuffle hit-posis))
          :let [vx (/ x tile-width)
                vy (/ y tile-height)
                [x y] (get-position body)
                explosion-posi [(+ x vx) (+ y vy)]]]
    (animation-entity
      :position explosion-posi
      :animation (create-animation (if big-explosion
                                     big-explosion-frames
                                     (if-chance 50 explosion-frames bigger-explosion-frames))))))

(defn big-body-hit-effect
  [hit-posis]
  (create-comp :hit-effect
    {:trigger (fn [body] (rand-posis-hit-effect body hit-posis))}))

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
    (let [free-posis (get-free-posis body (get-position body) 6 3)]
      (when (seq free-posis)
        (let [posi (rand-nth free-posis)]
          (teleport body posi)
          (monsterteleport-animation posi)))
      (set-hp-to-max body)))) ; danach hp-to-max damit an neuer posi +hp string steht

(defn normal-monster-melee []
  (monster-melee-component :cooldown 500
                           :attacktime 250
                           :hit-sound (create-sound "slash.wav")
                           :target-id (get-id player-body)))

(def heal-radius 6)

(defn get-healable-monsters-around [healer]
  (remove #(= % healer)
          (get-destructible-bodies (get-position healer) heal-radius :monster)))
