(ns game.player.core
  (:require
    [game.components.body-effects-impl :refer [create-armor-reduce-effect slowdown stun]]
    [game.utils.random :refer [when-chance]]
    [engine.render.color :as color]
    [engine.render.image :refer [create-image]]
    [engine.render.animation :refer [create-animation]]
    [engine.render.assets :refer [folder-animation]]
    [engine.render.graphics :refer [render-readable-text]]
    game.components.update
    game.maps.data
    [game.maps.minimap :refer [show-on-minimap]]
    [game.components.skills.core :refer [is-attacking? reset-skills]]
    [game.components.skills.melee :refer [get-player-skill-animation-key]]
    [game.utils.lightning :refer [light-component]]
    [utils.numbers :refer [set-to-max]]
    [utils.coll :refer [when-seq]]
    [engine.core :refer [play-sound]]
    [engine.input :refer [is-key-down? is-leftbutton-down? is-leftm-consumed?]]
    engine.input
    game.maps.contentfields
    [game.components.core :refer [add-to-removelist create-entity player-body]]
    [game.components.active :refer [switch-state]]
    [game.components.position :refer [position-component]]
    [game.components.movement :refer [movement-component]]
    game.components.movement
    [game.components.misc :refer [mana-regen-component rotation-component set-rotation-angle]]
    [game.components.ingame-loop :refer [ingame-loop-comp]]
    [game.components.render :refer [animation-component rendering]]
    game.components.render
    [game.components.destructible :refer [destructible-component is-dead? player-start-hp]]
    [game.components.body :refer [colliding-with-other-solid-bodies? create-body teleport]]
    [game.components.body-effects :refer [get-sub-entities]]
    [game.item.cells :refer [get-inventory-cells-with-item-name remove-item-from-cell]]
    [game.components.movement.ai.potential-field :refer [potential-field-component]]
    [game.player.skill.skillmanager :refer [create-player-skillmanager get-current-standard-skill]]
    game.player.skill.learnable
    [game.player.speed :refer [player-move-speed]]
    [game.mouseoverbody :refer [saved-mouseover-body]]
    game.settings
    [game.debug-settings :as debug]
    [game.utils.geom :refer [add direction-vector get-vector-to-mouse-coords normalise vector2f]]
    game.utils.geom
    [game.utils.msg-to-player :refer [show-msg-to-player]]))

; use require and remove the -player prefixes or suffixes here

;;; movement (was game.player.settings + game.player.movement)

(defn- add-vs [vs]
  (normalise (reduce add (vector2f [0 0]) vs)))

(defn- calc-movement-v [body]
  (let [r (when (is-key-down? :RIGHT) [1 0])
        l (when (is-key-down? :LEFT) [-1 0])
        u (when (is-key-down? :UP) [0 -1])
        d (when (is-key-down? :DOWN) [0 1])]
    (cond
      @saved-mouseover-body
      (direction-vector (:value (:position @body)) (:value (:position @@saved-mouseover-body)))

      (and (not (is-leftm-consumed?)) (is-leftbutton-down?))
      (get-vector-to-mouse-coords)

;      (or r l u d)
;      (let [^Vector2f v (add-vs (map vector2f (remove nil? [r l u d])))]
;        (when (pos? (.length v))
;          v))

      )))

(ingame-loop-comp :player-noclip-info
  (rendering :above-gui [g component]
    (when debug/player-noclip-info
      (render-readable-text g 0 200
        (str "player noclip? " (colliding-with-other-solid-bodies? player-body))))))

(defn player-movement-component []
  (movement-component
    {:control-update (fn [body _ delta]
                       (swap! body assoc-in [:movement :noclip] (colliding-with-other-solid-bodies? body))
                       (calc-movement-v body))}
    player-move-speed
    :ground))

;;; animation (was game.player.animation)

(defn- animation-control [body]
  (let [is-moving (:play-move-animation (:movement @body))
        holding-gun (= :gun (:type (get-current-standard-skill)))]
    (swap! body assoc-in [:movement :play-move-animation] false)
    (cond
      (is-dead? body)
        :death
      (is-attacking? (:skillmanager @body))
        (get-player-skill-animation-key)
      is-moving
        (if holding-gun :with-gun :moving)
      :else
        (if holding-gun :with-gun :standing))))

(defn player-animation []
  (let [folder (fn [s] (str "player/" s))]
    (animation-component
      animation-control
      {:fist        (folder-animation :folder (folder "sword/") :prefix "fist"  :duration 300  :looping false)
       :sword       (folder-animation :folder (folder "sword/") :prefix "sword" :duration 350  :looping false)
       :sword-small (folder-animation :folder (folder "sword/") :prefix "small" :duration 250  :looping false)
       :sword-big   (folder-animation :folder (folder "sword/") :prefix "big"   :duration 700  :looping false)
       :with-gun    (folder-animation :folder (folder "with_gun/")              :duration 600  :looping true)
       :moving      (folder-animation :folder (folder "running/")               :duration 1000 :looping true)
       :standing    (folder-animation :folder (folder "stand/")                 :duration 4000 :looping true)
       :meditation  (folder-animation :folder (folder "meditation/")            :duration 1500 :looping false)
       :casting     (folder-animation :folder (folder "casting/")               :duration 300  :looping false)
       :shooting    (folder-animation :folder (folder "shooting/")              :duration 150  :looping false)
       :death       (create-animation [(create-image (folder "die/l.png"))])})))

;;; core

(defn player-death
  "this is NOT implemented as a death-trigger (immediately called at deal-dmg and hp<0) because the snapshot and update order of components
  affected the player-body after the death event ->  the rotation-angle of player-body was altered in rare cases.
  To be independent of the order of entitiy updates call this after the frame is finished updating.
  TLDR: player components affected player-body after deathtrigger happened."
  []
  (show-msg-to-player (str "You died!\nPress ESCAPE to "
                           (let [restorations (get-inventory-cells-with-item-name "Restoration" :inventory)]
                             (if (seq restorations)
                               (str "be revived. Restorations left: " (dec (count restorations)) "")
                               "exit the game."))))
  (let [body player-body]
    (reset! game.components.update/running false)
    (play-sound "bfxr_playerdeath.wav")
    (dorun (map add-to-removelist (get-sub-entities body)))
    (game.components.update/update-component 0 (:animation @body) body) ; set death animation
    (swap! player-body assoc-in [:destructible :hp :current] 0)
    (set-rotation-angle body 0)))

(defn- revive-player []
  (show-msg-to-player "") ; removes the old msg
  (reset! game.components.update/running true)
  (teleport player-body (:start-position (game.maps.data/get-current-map-data)))
  (swap! player-body #(-> % 
    (assoc-in [:destructible :is-dead] false)
    (update-in [:destructible :hp] set-to-max)
    (update-in [:skillmanager :mana] set-to-max)
    (switch-state :skillmanager :ready)
    (update-in [:skillmanager :skills] reset-skills))))

(defn try-revive-player []
  (when-seq [cells (get-inventory-cells-with-item-name "Restoration" :inventory)]
    (remove-item-from-cell (rand-nth cells))
    (revive-player)
    true))

; use integers because floating point arithmetic is not precise.
; So when using floats and adding/subtracting some bonis it may be 0.999 instead of 1
(defn item-boni-component []
  {:type :item-boni
   :min-inc 0
   :max-inc 0
   :percent-modify 100
   :percent-modify-spell 100
   :melee-crit 0
   :spell-crit 0
   :hp-leech 0
   :mana-leech 0
   :chance-stun 0
   :chance-slow 0
   :chance-reduce-armor 0
   :dealt-melee-dmg-effect (fn [target-body]
                             (let [{:keys [chance-stun chance-slow chance-reduce-armor]} (:item-boni @player-body)]
                               (when-chance chance-stun
                                 (stun target-body 1000))
                               (when-chance chance-slow
                                 (slowdown target-body 10))
                               (when-chance chance-reduce-armor
                                 (create-armor-reduce-effect target-body 50 10))))})


(defn- create-player-body [position]
  (create-entity
    (position-component position)
    (create-body :solid true
                 :side :player
                 :pxw 14
                 :pxh 14
                 :mouseover-outline true)
    (create-player-skillmanager)
    (player-movement-component)
    (destructible-component player-start-hp 0)
    (rotation-component)
    (item-boni-component)
    (mana-regen-component 5)
    (light-component :intensity 1 :radius 12 :falloff 5) ; (/ screen-height 2 16) = 9
    (player-animation)
    (show-on-minimap color/red)
    (potential-field-component)))

(defn init-player [position]
  (intern 'game.components.core 'player-body (create-player-body position)))
