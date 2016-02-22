(ns game.monster.monsters
  (:require [engine.render :as color]
            game.maps.data
            game.components.burrow)
  (:use utils.core
        (engine core render)
        game.settings
        (game.item instance instance-impl)
        (game.monster defmonster [spawn :only (try-spawn)])
        game.maps.minimap
        (game.components core [sleeping :only (wake-up)]
                         active misc body destructible movement render body-effects body-effects-impl
                         [shield :only (shield-component)])
        (game.entity nova [projectile :only (fire-projectile)] teleporters)
        (game.components.movement.ai ranged-monster homing
                                     [potential-field :only (path-to-player-movement)])
        (game.components.skills core melee)
        game.player.core
        (game.utils random raycast geom lightning)))

(defn- path-to-player-blocked?
  [[sx sy] projectile-pxsize]
  (let [[tx ty] (get-position player-body)
        path-w (in-tiles projectile-pxsize)]
    (is-path-blocked? sx sy tx ty path-w)))

(defpreload ^:private redball-frames (folder-frames "effects/red_ball/"))

(let [maxrange 10
      maxrange-squared (Math/pow maxrange 2)
      pxsize 7]
  (defn- redball-projectile-skill-props []
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

(defnks ranged-component
  [:cooldown :opt :attacktime :opt-def :state-blocks {:attacking :movement}]
  (standalone-skill
    :stype :ranged
    :cooldown cooldown
    :attacktime attacktime
    :state-blocks state-blocks
    :props (redball-projectile-skill-props)))

;;

(defpreload ^:private monsterdie-frames (folder-frames "effects/monsterexplosion/"))

(defn- monster-die-effect [body]
  (animation-entity
    :animation (create-animation monsterdie-frames)
    :position (get-position body)))

(def- monster-drop-table
  {{"Grenade" 1
    "Battle-Drugs" 1} 2
   {
    ;"Mana-Potion" 2
    "Heal-Potion" 4
    "Big-Mana-Potion" 1
    "Big-Heal-Potion" 4} 9})

(defn- default-monster-death [body & {sound :sound :or {sound true}}]
  (monster-die-effect body) ; "effect" was ist das? sound/animation oder was
  (when sound
    (play-sound "bfxr_defaultmonsterdeath.wav"))
  (let [position (get-position body)]
    (if-chance 20 ; TODO when-chance
      (let [item-name (get-rand-weighted-item
                        (get-rand-weighted-item monster-drop-table))]
        (create-item-body position item-name)))
    (if-chance 6
      (create-rand-item position :max-lvl (:rand-item-max-lvl (game.maps.data/get-current-map-data))))))

(defn death-trigger [f] (create-comp :death-trigger {:destruct f}))

(defn- default-death-trigger [] (death-trigger default-monster-death))

(defpreload ^:private bigger-explosion-frames (spritesheet-frames "effects/explosn.png" 20 20))
(defpreload ^:private big-explosion-frames (spritesheet-frames "effects/expbig.png" 40 40))

(defn- rand-posis-hit-effect
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

(defn- big-body-hit-effect
  [hit-posis]
  (create-comp :hit-effect
    {:trigger (fn [body] (rand-posis-hit-effect body hit-posis))}))

;;

(defmonster little-bot {:hp 0.5 :armor 0 :pxsize 9}
  (death-trigger (fn [body]
                   (play-sound "bfxr_defaultmonsterdeath.wav")
                   (monster-die-effect body)))
  (path-to-player-movement 15)
  (rotation-component)
  (image-render-component (monsterimage "littlebot.png"))
  (monster-melee-component
    :cooldown 1000
    :attacktime 100
    :hit-sound (create-sound "slash.wav")
    :target-id (get-id player-body)))

(defn- get-free-posis [body position half-w half-h]
  (remove #(blocked-location? % body)
          (map translate-to-tile-middle
               (get-touched-tiles position half-w half-h))))

(defn monsterteleport-animation [position]
  (animation-entity
    :animation (create-animation (spritesheet-frames "effects/red_teleport.png" 17 17) :frame-duration 100)
    :order :on-ground
    :position position))

; TODO mach irgendwo in SICHTBAREN cells von player
; nur dann m�glich wenn er eine findet
; set-hp-to-max macht noch ne animation h�tt ich net erwartet
; wenn lethal dmg dann hier healing aber trotzdem death -> aber sonst fast unkillbar wohl...
(defn- teleport-and-heal-when-low-hp [body lethal]
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

(defn- normal-monster-melee []
  (monster-melee-component :cooldown 500
                           :attacktime 250
                           :hit-sound (create-sound "slash.wav")
                           :target-id (get-id player-body)))

(defmonster littlespider {:hp 1 :armor 7 :pxsize 13}
  (default-death-trigger)
  (path-to-player-movement 47)
  (rotation-component)
  (single-animation-component
    (folder-animation :folder (monsterresrc "littlespider/") :duration 300 :looping true))
  (normal-monster-melee))

(defmonster xploding-drone {:hp 1.5 :armor 5 :pxsize 15}
  (death-trigger (fn [this-body]
                   (default-monster-death this-body :sound false) ; TODO this strange sound false and play-sound ... => default monster dead more than 1 thing...
                   (play-sound "bfxr_dronedeath.wav")
                   (nova-effect ; TODO two novas because different dmg to player/monster ... => im dealt dmg trigger berücksichtigen?
                     :position (get-position this-body)
                     :duration 150
                     :maxradius 2
                     :affects-side [:player]
                     :dmg [20 20]
                     :animation (folder-animation :folder "effects/xpldrone/" :duration 150 :looping false))
                   (nova-effect
                     :position (get-position this-body)
                     :duration 150
                     :maxradius 2
                     :affects-side [:monster]
                     :dmg [4 8]
                     :animation (folder-animation :folder "effects/xpldrone/" :duration 150 :looping false))))
  (path-to-player-movement 13)
  (rotation-component)
  (single-animation-component
    (folder-animation :folder (monsterresrc "xplodingdrone/") :duration 500 :looping true))
  (monster-melee-component
    :cooldown 500
    :attacktime 500
    :hit-sound (create-sound "slash.wav")
    :target-id (get-id player-body)))

(let [posis [[9 -11] [-1 -11] [-13 -7] [-9 2] [3 3] [3 15] [-5 14] [7 17] [-11 5] [-12 14]]]
  (defmonster research-station {:hp 10 :armor 25 :pxsize 43}
    (big-body-hit-effect posis)
    (death-trigger (fn [body]
                     (play-sound "bfxr_stationdeath.wav")
                     (rand-posis-hit-effect body posis :big-explosion true)))
    (show-on-minimap color/orange)
    (single-animation-component
      (folder-animation :folder (monsterresrc "station/") :duration 500 :looping true))))

(defpreload ^:private mine-explosion-frames (folder-frames "effects/mine/"))

(defmonster mine {:hp 1 :armor 0 :pxsize 15}
  (death-trigger (fn [this-body]
                   (play-sound "bfxr_minedeath.wav")
                   (nova-effect
                     :position (get-position this-body)
                     :duration 300
                     :maxradius 4
                     :affects-side [:player]
                     :dmg [30 40]
                     :animation (create-animation mine-explosion-frames))
                   (nova-effect
                     :position (get-position this-body)
                     :duration 300
                     :maxradius 4
                     :affects-side [:monster]
                     :dmg [6 8]
                     :animation (create-animation mine-explosion-frames))))
  (image-render-component (monsterimage "mine.png"))
  (single-animation-component
    (create-animation (spritesheet-frames "effects/red_glow.png" 32 32)
                      :frame-duration 150
                      :looping true)
    :order :on-ground))

(defmonster skull-chainsaw {:hp 1.3 :armor 4 :pxsize 15}
  (default-death-trigger)
  (lowhp-runaway-movement 50)
  (create-comp :dealt-dmg-trigger {:do lowhp-dealt-dmg-trigger})
  (rotation-component)
  (single-animation-component
    (folder-animation :folder (monsterresrc "teleportraider/") :duration 500 :looping true))
  (normal-monster-melee))

(defmonster storagebox {:hp 1 :armor 25 :pxsize 41}
  ;(default-death-trigger)
  (image-render-component (monsterimage "storage1.png")))

(defmonster armored-skull {:hp 2 :armor 65 :pxsize 15}
  (default-death-trigger)
  (path-to-player-movement 13)
  (rotation-component)
  (hp-regen-component 2)
  (image-render-component (monsterimage "coredemonhand.png"))
  (normal-monster-melee))

(defmonster armored-skull2 {:hp 1 :armor 23 :pxsize 15}
  (default-death-trigger)
  (path-to-player-movement 13)
  (rotation-component)
  (hp-regen-component 5)
  (image-render-component (monsterimage "vorticularcutlass.png"))
  (normal-monster-melee))

(defmonster big-skull-chainsaw {:hp 3 :armor 25 :pxsize 15}
  (default-death-trigger)
  (path-to-player-movement 25)
  (rotation-component)
  (hp-regen-component 1)
  (image-render-component (monsterimage "vorticularcutlassiv.png"))
  (normal-monster-melee))

(defmonster big-teleporting-melee {:hp 5 :armor 15 :pxsize 15}
  (default-death-trigger)
  (path-to-player-movement 8)
  (rotation-component)
  (single-animation-component
    (folder-animation :folder (monsterresrc "corebomber/") :duration 300 :looping true))
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
                        (let [old-posi (get-position entity)
                              posis (get-free-posis entity (get-position player-body) 2 2)]
                          (when (not-empty posis)
                            (let [posi (rand-nth posis)]
                              (teleport entity posi)
                              (monsterteleport-animation posi)
                              (create-line-render-effect posi old-posi 140 :color color/white)))))}))
; TODO shoot-sound?


(defmonster burrower {:hp 2.5 :armor 20 :pxsize 15}
  (default-death-trigger)
  (path-to-player-movement 30)
  (rotation-component)
  (image-render-component (monsterimage "burrower.png"))
  (normal-monster-melee)
  (game.components.burrow/burrow-component))

(defmonster ranged {:hp 0.6 :armor 5 :pxsize 15}
  (default-death-trigger)
  (ranged-runaway-movement-comp 24 (rand-int-between 2 6) :ground)
  (rotation-component)
  (image-render-component (monsterimage "core_raider.png"))
  (ranged-component :cooldown (rand-int-between 2000 2500) :attacktime 500))

(defmonster shield-turret {:hp 1.5 :armor 15 :pxsize 15}
  (path-to-player-movement 13)
  (default-death-trigger)
  (shield-component 1500)
  (rotation-component)
  (image-render-component (monsterimage "counternegativeenergyturret.png"))
  (ranged-component :cooldown 4000 :attacktime 500))

(defmonster mage-skull {:hp 4.8 :armor 0 :pxsize 15}
  (default-death-trigger)
  (ranged-runaway-movement-comp 26 (rand-int-between 3 4) :ground)
  (hp-regen-component 5)
  (rotation-component)
  (image-render-component (monsterimage "core_predator.png"))
  (ranged-component :cooldown 3000 :attacktime 50))

(let [attacktime 600
      folder (monsterresrc "fly/")]
  (defmonster fly {:hp 1 :armor 0 :pxsize 9}
    (default-death-trigger)
    (ranged-randomly-moving-comp 32 1000 :ground)
    (animation-component (fn [body]
                           (if (is-attacking? (get-component body :ranged)) :attack :default))
                         {:attack (folder-animation :folder folder :prefix "attack" :duration attacktime :looping false)
                          :default (create-animation [(create-image (str folder "fly.png"))])})
    (ranged-component :cooldown 2000 :state-blocks {}))) ; move+shoot

;; Healer

(def- heal-radius 6)

(defn- get-healable-monsters-around [healer]
  (remove #(= % healer)
          (get-destructible-bodies (get-position healer) heal-radius :monster)))

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

(defn create-healer-skillmanager []
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
(defmonster healer {:hp 3 :armor 0 :pxsize 15}
  (default-death-trigger)
  (ranged-runaway-movement-comp 30 (rand-int-between 1 3) :ground)
  (rotation-component)
  (single-animation-component
    (folder-animation :folder (monsterresrc "healer/") :duration 1000 :looping true))
  (create-healer-skillmanager))

;; Instant-Healer

(defcomponent :cache-nearby-monsters []
  {:counter (make-counter 1000)}
  (active [delta {:keys [counter] :as c} entity]
    (when (update-counter! entity delta c)
      (assoc-in! entity [(:type c) :nearby-monsters]
                 (doall (get-healable-monsters-around entity))))))

; TODO not checking if ray-blocked ---> can heal through walls like other healer
; navigation meshes would make ray-blocked much simpler if in the same polygon/area
(defn- healing-required-and-allowed? [entity healer radius-squared]
  (and (exists? entity)
       (not (is-dead? entity))
       (lower-than-max? (get-hp entity))
       (bodies-in-range? entity healer radius-squared)))

; TODO mehr hervorheben anstatt drawLine vlt so ein "beam" strahl... gr�n mit weiss als kontrast (not in prototype stage -> do another time!)
; rotating to which monster is healed?
(let [healradius-squared (* heal-radius heal-radius)]
  (defmonster instant-healer {:hp 2 :armor 20 :pxsize 14}
    (default-death-trigger)
    (path-to-player-movement 15)
    (rotation-component)
    (image-render-component (monsterimage "coreturret.png"))
    (cache-nearby-monsters-component)
    (monster-melee-component :cooldown 500
                             :attacktime 100
                             :hit-sound (create-sound "slash.wav")
                             :target-id (get-id player-body))
    (standalone-skill
      :stype :instantheal
      :cooldown 1000
      :attacktime 150
      :props {:shoot-sound "bfxr_instanthealer_heal.wav"
              :check-usable (fn [entity _]
                              (when-let [cached (:nearby-monsters (get-component entity :cache-nearby-monsters))]
                                (when-let [needs-heal (first (sort-by #(:current (get-hp %))
                                                                      (filter #(healing-required-and-allowed? % entity healradius-squared)
                                                                              cached)))]
                                  (assoc-in! entity [:instantheal :needs-heal] needs-heal)
                                  true)))
              :do-skill (fn [healer {needs-heal :needs-heal :as component}]
                          (when (healing-required-and-allowed? needs-heal healer healradius-squared)
                            (set-hp-to-max needs-heal)
                            (create-line-render-effect (get-position healer) (get-position needs-heal) 200 :color color/green)))})))


;; Nova-Melee

(def monster-nova-radius 4)

(defn player-in-nova-range? [monster]
  (circle-collides? (get-position monster) monster-nova-radius player-body))

(defn- choose-active-nova-melee [entity skillmanager delta]
  (let [skills (:skills skillmanager)
        melee (:melee skills)
        nova (:monster-nova skills)]
    (if
      (and
        (enough-mana? nova skillmanager)
        (is-ready? nova)
        (player-in-nova-range? entity)
        (not (ray-blocked? (get-position entity) (get-position player-body)))
        (zero? (rand-int 240)))
      :monster-nova
      :melee)))

(defpreload ^:private nova-frames (folder-frames "effects/monsternova/"))

(defn create-nova-melee-skillmanager []
  (let [melee-weapon (skillmanager-skill
                       :stype :melee
                       :cooldown 1000
                       :attacktime 500
                       :cost 0
                       (monster-melee-props (get-id player-body) (melee-weapon [3 8] (create-sound "slash.wav"))))
        monster-nova (skillmanager-skill
                       :stype :monster-nova
                       :cooldown (rand-int-between 1000 2000)
                       :attacktime 1500
                       :cost 1
                       {:shoot-sound "bfxr_monstercast.wav"
                        :do-skill (fn [entity component]
                                    (nova-effect
                                      :position (get-position entity)
                                      :duration 400
                                      :maxradius monster-nova-radius
                                      :affects-side :player
                                      :dmg [18 22]
                                      :animation (create-animation nova-frames)))})]
    (skillmanager-component
      :choosefn choose-active-nova-melee
      :mana (rand-int-between 2 4)
      :skills [melee-weapon monster-nova]
      (blocks-component {:attacking :movement}))))

(defmonster nova-melee {:hp 2.2 :armor 7 :pxsize 15}
  (default-death-trigger)
  (path-to-player-movement 32)
  (single-animation-component
    (folder-animation :folder (monsterresrc "gravturret/") :duration 1000 :looping true))
  (create-nova-melee-skillmanager))

;;


(defmonster slowdown-caster {:hp 2 :armor 25 :pxsize 12}
  (default-death-trigger)
  (path-to-player-movement 20)
  (single-animation-component
    (folder-animation :folder (monsterresrc "slowdowncaster/") :duration 200 :looping true))
  (standalone-skill        ; TODO f�r nen ranged skill mit custom hit-effects & movement sehr kompliziert!!
    :stype :slowdown-ranged
    :cooldown 2000
    :attacktime 1000
    :props {:show-cast-bar true
            :do-skill (fn [entity ranged-comp]
                        (let [speed 84
                              rotation-speed 0.1
                              starting-angle (get-angle-to-position (get-position entity) (get-position player-body))]
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
  (defmonster ray-shooter {:hp 1 :armor 25 :pxsize 15}
    (default-death-trigger)
    (path-to-player-movement 22)
    (create-comp :dealt-dmg-trigger {:do teleport-and-heal-when-low-hp})
    (single-animation-component
      (folder-animation :folder (monsterresrc "harvesterexoshield/") :duration 200 :looping true))
    (standalone-skill
      :stype :rayshoot
      :cooldown (rand-int-between 1700 2300)
      :attacktime attacktime
      :state-blocks {:attacking :movement} ; important because line rendered from these posis! so dont move when attacking!
      :props {:show-cast-bar false
              :shoot-sound "bfxr_rayshooterhit.wav"
              :target-posi (atom nil) ; REMOVE
              :check-usable (fn [entity component]
                              (let [shooter-posi (get-position entity)
                                    target-posi (get-position player-body)]
                                (when (and (in-range? shooter-posi target-posi maxrange-squared)
                                           (not (ray-blocked? shooter-posi target-posi)))
                                  (reset! (:target-posi component) target-posi)
                                  (play-sound "bfxr_powerup.wav") ; length of sound ~ length of attacktime would be nice
                                  ; TODO line render only as long as monster is alive would also make more sense ? ...
                                  ; also when slowed down ... attacktime changes ...
                                  ; => just like a component of an entity slowed down/lives with it
                                  (create-line-render-effect shooter-posi target-posi attacktime :color color/white :thin true)
                                  true)))
              :do-skill (fn [entity component]
                          (let [target @(:target-posi component)]
                            (if (some is-player? (get-bodies-at-position target))
                              (deal-dmg [10 15] player-body)
                              (animation-entity :position target
                                                :animation (create-animation (spritesheet-frames "effects/12_16_littleexpl.png" 12 16) :frame-duration 100)
                                                :order :on-ground))
                            (create-line-render-effect (get-position entity) target 70 :color color/red)))})))
;;

(defn- rand-spawn-monster [monstertype position areahw areahh]
  (let [{:keys [half-w half-h]} (get-monster-properties monstertype)
        spawn-positions (remove #(blocked-location? % half-w half-h :ground) ; get-free-posis duplicate?
                                (map translate-to-tile-middle
                                     (get-touched-tiles position areahw areahh)))]
    (when (seq spawn-positions)
      (let [target (rand-nth spawn-positions)]
        (wake-up (try-spawn target monstertype)) ; TODO try-spawn also checks if blocked °_°
        (monsterteleport-animation target)
        (create-line-render-effect position target 70 :color color/white)))))

(use 'game.components.ingame-loop)

(comment
  (let [mlist [:littlespider :xploding-drone :mine :skull-chainsaw :armored-skull :armored-skull2 :ranged :shield-turret :mage-skull :healer :nova-melee :slowdown-caster]]
  (ingame-loop-comp :randspawn
    {:counter (make-counter 10000)}
    (active [delta c entity]
      (when (and (not= @current-map :town)
                 (update-counter! entity delta c))
        (play-sound "bfxr_monstercast.wav")
        (rand-spawn-monster mlist
                            (get-position player-body)
                            half-display-w-in-tiles
                            half-display-h-in-tiles
                            :nospawn (rand-int-between 1 9))
        (assoc-in! entity [(:type c) :counter :maxcnt] (rand-int-between 1000 20000)))))))

; (remove-entity :randspawn)

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

(defpreload ^:private boss-explosion (folder-frames "effects/bossexplosion/"))

(defmonster first-boss {:hp 20 :armor 50 :pxw 33 :pxh 63}
  (big-body-hit-effect [[12 -15] [-8 -15] [0 -23] [1 -6] [-9 -4] [-8 4] [8 8] [-8 15] [3 23] [-6 25]])
  ;(light-component :color (rgbcolor :r 0.8 :g 0.2 :b 0.2) :intensity 1 :radius 12)
  (death-trigger (fn [body]
                   (play-sound "bfxr_bossdeath.wav")
                   (animation-entity
                     :animation (create-animation boss-explosion)
                     :position (get-position body))
                   (create-item-body (get-position body) "The Golden Banana")

                   ; no lvl after this => no need to spawn an item!
                   ; (create-rand-item (get-position body) :max-lvl (:rand-item-max-lvl (get-current-map-data)))

                   (runmap add-to-removelist (:projectiles (get-component body :boss-ranged)))))
  (standalone-skill
    :stype :monster-spawner
    :cooldown 2000
    :attacktime 500
    :props {:show-cast-bar true
            :shoot-sound "bfxr_monstercast.wav"
            :do-skill (fn [entity component]
                        (rand-spawn-monster :little-bot (get-position entity) 6 3))})
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
                          (update-in! entity [:boss-ranged :projectiles] concat
                                      (doall
                                        (map #(fire-boss-ranged-projectile entity speed % rotation-speed effects)
                                             [0 90 180 270])))))})
  (movement-component ; TODO komische args ... mach mit defnks?!
    {:control-update (fn [body _ _] (get-vector-to-player body))}
    12
    :ground)
  (single-animation-component ; TODO gleich folder-animation auchnoch reinpacken in single-animation-component?
    (folder-animation :folder (monsterresrc "boss/") :duration 300 :looping true)))

(defmonster test-hunter {:hp 1.5 :armor 7 :pxsize 47}
  (path-to-player-movement 72)
  (rotation-component)
  (image-render-component (monsterimage "coredemonhand.png")))
