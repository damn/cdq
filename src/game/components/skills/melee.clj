(ns game.components.skills.melee
  (:use
    utils.core
    [engine.core :only (create-sound)]
    (game mouseoverbody
      [settings :only (in-tiles)])
    (game.utils random front-of-body-shape geom)
    (game.components core body destructible [update :only (max-delta)])
    (game.components.skills core)
    [game.item.cells :only (get-equiped-hands-item)]
    game.maps.cell-grid))

; TODO unabh von tiles machen ->in pixels?
(def melee-puffer 0.8)

(let [pixel 5]
  (def start-melee-puffer (in-tiles pixel)))

(comment
  "visual \"close-ness\" required for melee attacks
if an entity is very fast and the movement step in 1 frame is >as this then
it can not get in melee range -> in this case split up movement into multiple smaller steps
so the entity can get in melee range.")

(defn body-in-melee-range?
  "attacked-already weil wenn noch nicht @attacking state sondern schauen ob in range -> kleinerer abstand.
   später grösserer abstand möglich weil sonst bewegt sich gegner 1 frame weg  und ausser range .."
  ([body target]
    (body-in-melee-range? body target true))
  ([body target attacked-already] ; TODO als :attacked-already key machen
    (let [puffer (if attacked-already melee-puffer start-melee-puffer)
          radius (+ puffer (get-half-width body))] ; TODO geht davon aus half-w = half-h von body (ansonsten komplizierter!)
      (circle-collides? (get-position body) radius target))))
; TODO vlt schneller einfach distance und den bounding radius der beiden abziehen
; == abstand und mit start-melee-puffer vergleichen

(defn- target-in-range? [entity target-id & {:keys [attacked-already]}]
  (body-in-melee-range? entity (get-entity target-id) attacked-already))

(defn- check-usable-melee-player [entity {:keys [target-body-id] :as melee-comp}]
  (let [mouseover-target-id (when-let [body (get-mouseover-body)]
                              (when (attackable-by-player? body)
                                (get-id body)))]
    (case (get-skill-use-mouse-button)
      :left (when mouseover-target-id
              (assoc-in! entity [:skillmanager :skills (:type melee-comp) :target-body-id]
                         mouseover-target-id)
              (target-in-range? entity mouseover-target-id :attacked-already false))
      :right (do
               (assoc-in! entity [:skillmanager :skills (:type melee-comp) :target-body-id]
                          (or mouseover-target-id nil))
               true))))

(defn- check-usable-melee-monster [entity melee-comp]
  (target-in-range? entity (:target-body-id melee-comp) :attacked-already false))

(defn melee-weapon
  ([base-dmg hit-sound]
    {:base-dmg base-dmg :hit-sound hit-sound})
  ([base-dmg hit-sound animation]
    (assoc (melee-weapon base-dmg hit-sound) :animation animation)))

(def- fist-weapon
  (melee-weapon [1 2] (create-sound "bfxr_fisthit.wav") :fist))

(defn- get-current-player-melee-weapon []
  (or (:melee-weapon (get-equiped-hands-item)) fist-weapon))

(defn- get-melee-weapon [entity melee-skill]
  (if (is-player? entity)
    (get-current-player-melee-weapon)
    (:melee-weapon melee-skill)))

(defn- melee-hit [entity meleecomp targetbody]
  (let [{:keys [base-dmg hit-sound]} (get-melee-weapon entity meleecomp)]
    (deal-dmg base-dmg targetbody :is-player-melee (is-player? entity))
    (runmap #(% targetbody) (:hit-effects meleecomp))
    ;(play-sound hit-sound)
    ))

(defn- try-hit-target
  "returns nil when target not in range or does not exist anymore"
  [entity {:keys [target-body-id] :as meleecomp}]
  (when target-body-id
    (let [targetbody (get-entity target-body-id)]
      (if (and targetbody
               (is-affectable? targetbody)
               (target-in-range? entity target-body-id :attacked-already true))
        (melee-hit entity meleecomp targetbody)
        (comment "missed target!")))))

(defn- create-melee-props [target-body-id]
  {:target-body-id target-body-id
   :is-melee true
   :do-skill try-hit-target})

(defn get-attackable-target-in-front [body]
  (let [half-w-max (+ (get-half-width body) melee-puffer)
        posi (get-position body)
        cshape (in-front-of-body-shape body melee-puffer)]
    (find-first #(and (attackable-by-player? %)
                      (collides? cshape (rect-shape %)))
                (get-bodies-from-cells
                  (calc-touched-cells posi half-w-max half-w-max)))))

(defn- do-skill-melee-player [entity {:keys [target-body-id] :as meleecomp}]
  (let [mouse (get-skill-use-mouse-button)]
    (case mouse
      :left
      (try-hit-target entity meleecomp)

      :right
      (when-not (and target-body-id (try-hit-target entity meleecomp))
        (when-let [target (get-attackable-target-in-front entity)]
          (melee-hit entity meleecomp target))))))

(defn player-melee-props [hit-effects]
  (merge
    (create-melee-props :undefined-yet)
    {:do-skill do-skill-melee-player
     :hit-effects hit-effects
     :check-usable check-usable-melee-player}))

(defn monster-melee-props [target-body-id melee-weapon]
  (merge
    (create-melee-props target-body-id)
    {:melee-weapon melee-weapon
     :check-usable check-usable-melee-monster}))

(defn get-current-player-melee-dmg []
  (-> (get-current-player-melee-weapon) :base-dmg (calc-effective-melee-dmg (get-component player-body :item-boni))))

(defnks monster-melee-component
  [:cooldown :hit-sound :target-id :opt :attacktime]
  (standalone-skill
    :stype :melee
    :cooldown cooldown
    :attacktime attacktime
    :state-blocks {:attacking :movement
                   :cooldown :movement}
    :props (monster-melee-props target-id (melee-weapon [4 6] hit-sound))))

(defn get-player-skill-animation-key []
  (let [skill (get-active-skill (get-component player-body :skillmanager))]
    (:animation
      (if (:is-melee skill) (get-current-player-melee-weapon) skill))))

