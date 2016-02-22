(ns game.components.shield
  (:require [engine.core :refer [make-counter update-finally-merge]])
  (:use
    utils.core
    engine.render
    (game.components core render body-effects)
    game.utils.geom))

(def- rotation-speed (/ 360 2000)) ; 360 degrees in 2 seconds = 2000 ms

(defcomponent shield [regeneration-duration]
  {:is-active true
   :image (create-image "effects/shield.png")
   :angle 0
   :counter (make-counter regeneration-duration)}

  (render-on-map :on-ground [g _ {:keys [is-active image angle] :as c} render-posi]
    (when is-active
      (render-rotated-centered-image g image angle render-posi)))

  (active [delta {:keys [is-active counter] :as c} entity]
    (assoc-in! entity [(:type c)]
               (if is-active
                 (update-in c [:angle] degree-add (* delta rotation-speed))
                 (update-finally-merge c :counter delta
                                       {:angle 0 :is-active true})))))

(defeffectentity ^:private shield-hit [body]
  :target body
  :duration 100
  (circle-around-body-render-comp body (rgbcolor :g 1 :r 0.5 :a 0.5) :air))

(defn shield-try-consume-damage [body]
  (when-let [shield (get-component body :shield)]
    (when (:is-active shield)
      (assoc-in! body [:shield :is-active] false)
      (shield-hit body)
      true)))
