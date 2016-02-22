(ns game.components.burrow
  (:use
    utils.core
    (engine core render)
    game.session
    (game.components active core render body position ingame-loop)
    (game.maps contentfields cell-grid)))

(defpreload ^:private dust-frames (spritesheet-frames "effects/dust.png" 15 15))
(defpreload ^:private dark-dust-frames (spritesheet-frames "effects/darkdust.png" 30 30))

(defn- burrow [entity & {audiovisual :audiovisual :or {audiovisual true}}]
  (when audiovisual
    (play-sound "bfxr_burrow.wav")
    (animation-entity :position (get-position entity)
                      :animation (create-animation dust-frames :frame-duration 100)
                      :order :is-ground))
  (->! entity
       block-active-components
       (assoc-in [:body :solid] false)
       (assoc-in [:burrow :burrowed] true))
  (reset-component-state-after-blocked entity))

(defn- try-unburrow [entity]
  (when (and (is-burrowed? entity)
             (not (colliding-with-other-solid-bodies? entity)))
    (play-sound "bfxr_unburrow.wav")
    (animation-entity :position (get-position entity)
                      :animation (create-animation dark-dust-frames))
    (->! entity
         unblock-active-components
         (assoc-in [:body :solid] true)
         (assoc-in [:burrow :burrowed] false))
    (->> entity
      get-other-bodies-in-adjacent-cells
      (filter is-burrowed?)
      (runmap try-unburrow))))

(defcomponent burrow []
  {:burrowed false
   :init (fn [entity]
           (assert (is-solid? entity)) ; must be solid because burrow/unburrow switches the solid flag
           (assert (not (is-multiple-cell? entity))) ; also single cell body because using get-occupied-cell @ get-dist-to-player and not -cell (s)
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
        (runmap check
          (filter #(get-component % :burrow)
                  (get-entities-in-active-content-fields))))))


