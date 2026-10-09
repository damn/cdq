(ns game.components.burrow
  (:require
    [utils.counter :refer [create-counter update-counter]]
    [engine.core :refer [defpreload play-sound]]
    [engine.render.animation :refer [create-animation]]
    [engine.render.assets :refer [spritesheet-frames]]
    [game.session :refer [atom-session]]
    game.components.active
    [game.components.core :refer [active block-active-components reset-component-state-after-blocked unblock-active-components]]
    [game.components.render :refer [animation-entity]]
    [game.components.body :refer [colliding-with-other-solid-bodies? get-dist-to-player get-other-bodies-in-adjacent-cells is-burrowed?]]
    game.components.position
    [game.components.ingame-loop :refer [ingame-loop-comp]]
    [game.maps.contentfields :refer [get-entities-in-active-content-fields]]
    game.maps.cell-grid))

(defpreload ^:private dust-frames (spritesheet-frames "effects/dust.png" 15 15))
(defpreload ^:private dark-dust-frames (spritesheet-frames "effects/darkdust.png" 30 30))

(defn- burrow [entity & {audiovisual :audiovisual :or {audiovisual true}}]
  (when audiovisual
    (play-sound "bfxr_burrow.wav")
    (animation-entity :position (:value (:position @entity))
                      :animation (create-animation dust-frames :frame-duration 100)
                      :order :is-ground))
  (swap! entity #(-> % 
       block-active-components
       (assoc-in [:body :solid] false)
       (assoc-in [:burrow :burrowed] true)))
  (reset-component-state-after-blocked entity))

(defn- try-unburrow [entity]
  (when (and (is-burrowed? entity)
             (not (colliding-with-other-solid-bodies? entity)))
    (play-sound "bfxr_unburrow.wav")
    (animation-entity :position (:value (:position @entity))
                      :animation (create-animation dark-dust-frames))
    (swap! entity #(-> % 
         unblock-active-components
         (assoc-in [:body :solid] true)
         (assoc-in [:burrow :burrowed] false)))
    (->> entity
      get-other-bodies-in-adjacent-cells
      (filter is-burrowed?)
      (map try-unburrow)
      dorun)))

(defn burrow-component []
  {:type :burrow
   :burrowed false
   :init (fn [entity]
           (assert (:solid (:body @entity))) ; must be solid because burrow/unburrow switches the solid flag
           (assert (not (:is-multiple-cell (:body @entity)))) ; also single cell body because using get-occupied-cell @ get-dist-to-player and not -cell (s)
           (burrow entity :audiovisual false))
   :depends [:body]})

; Tested:
; -> too little maxdist and you can burrow monster piece by piece and pick out some loner monsters... -> faster or only @ max 120 dist / no dist works better
; -> for island maps no dist very good when jumping to another island -> safe from player harrassment

(defn- check [entity]
  (let [dist (get-dist-to-player entity)]
    (if (is-burrowed? entity)
      (when (and dist (<= dist 30))  ; 30 = 3x tiledist
        (try-unburrow entity))
      (when-not dist ; out of range -> hide
        (burrow entity)))))

(def ^:private counter (create-counter 300))
(def session (atom-session counter :save-session false))

(ingame-loop-comp :burrow-check
    (active [delta _ _]
      (when (update-counter counter delta)
        (dorun (map check
          (filter #(:burrow @%)
                  (get-entities-in-active-content-fields)))))))

