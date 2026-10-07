(ns game.components.shield
  (:require
    [engine.core :refer [make-counter update-finally-merge]]
        [engine.render :refer [create-image render-rotated-centered-image rgbcolor]]
    [game.components.core :refer [active defcomponent]]
    [game.components.render :refer [circle-around-body-render-comp render-on-map]]
    [game.components.body-effects :refer [defeffectentity]]
    [game.utils.geom :refer [degree-add]]))

(def ^:private rotation-speed (/ 360 2000)) ; 360 degrees in 2 seconds = 2000 ms

(defcomponent shield [regeneration-duration]
  {:is-active true
   :image (create-image "effects/shield.png")
   :angle 0
   :counter (make-counter regeneration-duration)}

  (render-on-map :on-ground [g _ {:keys [is-active image angle] :as c} render-posi]
    (when is-active
      (render-rotated-centered-image g image angle render-posi)))

  (active [delta {:keys [is-active counter] :as c} entity]
    (swap! entity assoc-in [(:type c)]
               (if is-active
                 (update-in c [:angle] degree-add (* delta rotation-speed))
                 (update-finally-merge c :counter delta
                                       {:angle 0 :is-active true})))))

(defeffectentity ^:private shield-hit [body]
  :target body
  :duration 100
  (circle-around-body-render-comp body (rgbcolor :g 1 :r 0.5 :a 0.5) :air))

(defn shield-try-consume-damage [body]
  (when-let [shield (:shield @body)]
    (when (:is-active shield)
      (swap! body assoc-in [:shield :is-active] false)
      (shield-hit body)
      true)))
