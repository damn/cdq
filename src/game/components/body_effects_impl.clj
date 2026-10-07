(ns game.components.body-effects-impl
  (:require
    [utils.coll :refer [mapvals]]
    [utils.numbers :refer [approx-numbers]]
    [engine.core :refer [defpreload reset update]]
    [engine.render :refer [create-animation create-image folder-animation folder-frames get-scaled-copy render-centered-animation render-readable-text rgbcolor]]
    game.settings
    [game.components.core :refer [active add-to-removelist defcomponent get-components get-id is-player? player-body reset-component-state-after-blocked]]
    game.components.misc
    game.components.body
    [game.components.body-effects :refer [defeffectentity get-certain-effect-entities get-sub-entities]]
    [game.components.render :refer [circle-around-body-render-comp render-above-body render-on-map single-animation-component]]
    [game.components.destructible :refer [deal-dmg get-armor get-destructible-bodies]]
    game.components.movement
    [game.components.active :refer [add-blocks mark-slowed remove-blocks unmark-slowed]]
    [game.utils.random :refer [if-chance]]
    [game.utils.geom :refer [degree-add degree-minus degrees? scale vec-posi vector-from-angle]]
    game.item.instance
    [game.player.speed :refer [incr-attack-speed incr-casting-speed incr-move-speed]]))

(defeffectentity ^:private battle-drugs-entity [seconds speedup]
  :target player-body
  :duration (* seconds 1000)
  :dofn (fn []
          (swap! player-body #(-> % 
               (incr-casting-speed speedup)
               (incr-attack-speed  speedup)
               (incr-move-speed    speedup))))
  :undofn (fn []
            (swap! player-body #(-> % 
                 (incr-casting-speed (- speedup))
                 (incr-attack-speed  (- speedup))
                 (incr-move-speed    (- speedup)))))
  (single-animation-component (folder-animation :folder "effects/redpulse/"
                                                :looping true)
                              :order :on-ground))

(defn battle-drugs [seconds] ; argument ms, then dont have to (* 1000 2 times here)!
  (let [entity player-body]
    (if-let [existing (first (get-certain-effect-entities entity :battle-drugs-entity))]
      (swap! entity update-in [:delete-after-duration :counter :maxcnt] + (* seconds 1000))
      (battle-drugs-entity seconds 0.5))))

;;

; only when active components!
(defeffectentity stun [body milliseconds]
  :target body
  :duration milliseconds
  (let [stunned-comps (filter :updatefn (get-components body))
        stunned-comp-types (map :type stunned-comps)]
    {:type :do-and-undo
     :init (fn [_]
             (reset-component-state-after-blocked body)
             (swap! body add-blocks stunned-comp-types))
     :destruct (fn [_]
                 (swap! body remove-blocks stunned-comp-types))})
  (circle-around-body-render-comp body (rgbcolor :a 0.6) :on-ground))

;; Curses

(defn- icon-over-body-render-comp
  "target entity is not the entity of this component (used at sub-entities that only share position)"
  [target image]
  (merge {:type :visuals}
         (render-on-map :air [g _ c render-posi]
           (render-above-body g target render-posi image :ypuffer 2))))

(defn- remove-curses [body]
  (dorun (map add-to-removelist
              (filter #(:is-curse @%)
                      (get-sub-entities body)))))

; only when active components!
; slows the components of body - not the effect-entities
(defeffectentity ^:private slowdown-entity [body seconds]
  :target body
  :duration (* seconds 1000)
  :dofn (fn []
          (remove-curses body)
          (swap! body mapvals mark-slowed))
  :undofn #(swap! body mapvals unmark-slowed)
  {:type :is-curse}
  (icon-over-body-render-comp body (create-image "effects/bullettime.png")))

(defn slowdown [body seconds]
  (if-let [existing (first (get-certain-effect-entities player-body :slowdown-entity))]
    (swap! existing update-in [:delete-after-duration :counter] reset)
    (slowdown-entity body seconds)))

(defn create-bullettime-effects [posi radius seconds]
  (dorun (map #(slowdown % seconds) (get-destructible-bodies posi radius :monster))))

; for save/load order of changed-armor .. ? first item armor or this @ player? ..
; need to put calculation for changed armor @ defeffectentity?
(defeffectentity ^:private armor-reduce [body changed-armor seconds]
  :target body
  :duration (* seconds 1000)
  :dofn (fn []
          (remove-curses body)
          (swap! body update-in [:destructible :armor] - changed-armor))
  :undofn #(swap! body update-in [:destructible :armor] + changed-armor)
  {:type :is-curse}
  (icon-over-body-render-comp body (create-image "effects/armorreduce.png")))

(defn create-armor-reduce-effect [body percent seconds]
  (if-let [existing (first (get-certain-effect-entities body :armor-reduce))]
    (swap! existing update-in [:delete-after-duration :counter] reset)
    (let [armor (get-armor body)
          changed-armor (min armor (* armor (/ percent 100)))]
      (armor-reduce body changed-armor seconds))))

(defn aoe-armor-reducer [posi radius percent seconds]
  (dorun (map #(create-armor-reduce-effect % percent seconds)
    (get-destructible-bodies posi radius :monster))))

;; PSI-CHARGER

(defn equip-psi-charge-bonus [attack-speedup move-speedup]
  (swap! player-body #(-> % 
       (incr-attack-speed attack-speedup)
       (incr-move-speed move-speedup))))

(defn unequip-psi-charge-bonus [attack-speedup move-speedup]
  (swap! player-body #(-> % 
       (incr-attack-speed (- attack-speedup))
       (incr-move-speed (- move-speedup)))))

(def max-psi-charges 3)
(def ^:private duration (* 20 1000))

(defn current-psi-charges [body]
  (count (get-certain-effect-entities body :psi-charge)))

(defn- set-durations
  "Every 'duration' one charge disappears"
  [body]
  (dorun
    (map-indexed (fn [n entity]
                   (swap! entity update-in [:delete-after-duration :counter]
                               assoc :cnt 0 :maxcnt (* duration (inc n))))
                 (get-certain-effect-entities body :psi-charge))))

(defn get-psi-charge-degrees [body]
  {:post [(degrees? %)]}
  (let [degrees (map #(:degree (:psi-charge-visuals @%)) (get-certain-effect-entities body :psi-charge))
        cnt (count degrees) p1 (first degrees) p2 (second degrees)]
    (case cnt
      0 0
      1 (degree-add p1 120)
      ; epsilon bei approx-numbers: mindestens 7:  = (* 20 (/ 360 1000)) bei max. 60 FPS => max. 16.7 delta sagen wir 20
      2 (if (approx-numbers p2 (degree-add p1 120) 30)
          (degree-minus p1 120)
          (degree-add p1 120)))))
; case 2: zwei P1 und P2 mit ca. 120� abstand; gesucht P3 im abstand 120� von P1 und P2
; falls P2 im UZS rechts von P1 (durch addition) ist P3 dann entgegen den UZS von P1 ansonsten ist P3 im UZS rechts von P1

(def ^:private charge-body-distance 10)

(defpreload ^:private psicharge-frames (map #(get-scaled-copy % 7 7) (folder-frames "effects/psicharges/")))

(defeffectentity ^:private psi-charge [body attackinc moveinc]
  :target body
  :duration duration
  :dofn (fn []
          (equip-psi-charge-bonus attackinc moveinc))
  :undofn (fn []
            (set-durations body)
            (unequip-psi-charge-bonus attackinc moveinc))
  (merge {:type :psi-charge-visuals
          :degree (get-psi-charge-degrees body)
          :animation (create-animation psicharge-frames :looping true)}
         (active [delta c entity]
           (swap! entity update-in [(:type c)]
                       #(-> %
                            (update-in [:animation] update delta)
                            (update-in [:degree] degree-add (* delta (/ 360 1000))))))
         (render-on-map :on-ground [g _ c [x y]]
           (let [v (vector-from-angle (:degree c))
                 [vx vy] (vec-posi
                           (scale v charge-body-distance))
                 render-position [(+ x vx)
                                  (+ y vy)]]
             (render-centered-animation (:animation c) render-position)))))

(defn add-psi-charge [body attackinc moveinc]
  {:pre [(is-player? body)]}
  (when (< (current-psi-charges body) max-psi-charges)
    (psi-charge body attackinc moveinc))
  (set-durations body))
; first add a new charge, then set durations according to number of charges

(defn consume-psi-charges [body]
  (let [charges (get-certain-effect-entities body :psi-charge)]
    (dorun (map add-to-removelist charges))
    (count charges)))

(comment
  (.getSpeed (:casting (:animation @player-body)))
  (.getSpeed (:sword (:animation @player-body)))
  (:speed (:movement @player-body))

  (defn- get-durations []
    (clojure.string/join "\n"(map (fn [entity]
                                    (let [counter (:counter (:delete-after-duration @entity))]
                                      [(str (:cnt counter) "/" (:maxcnt counter))
                                       (get-id entity)]))
                                  (get-certain-effect-entities player-body :psi-charge))))
  (defcomponent test []
    (render-on-map :air [g _ c [x y]]
      (render-readable-text g x (- y 20)
                            :shift false
                            (get-durations))))

  (defcomponent debug-render []
    (render-on-map :air [g entity c [x y]]
      (render-readable-text g x (- y 20)
                            :shift false
                                (get-in (:delete-after-duration @entity)
                                        [:counter :cnt]))))
  )

; Hit Effects
; can use 'partial'

(defn dmg-effect [dmg & {is-player-spell :is-player-spell}]
  (fn [affected-body]
    (deal-dmg dmg affected-body :is-player-spell is-player-spell)))

(defn stun-collision-effect [chance duration]
  (fn [affected-body]
    (if-chance chance (stun affected-body duration))))

(defn slowdown-effect [seconds]
  (fn [affected-body]
    (slowdown affected-body seconds)))

