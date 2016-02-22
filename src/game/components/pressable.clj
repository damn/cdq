(ns game.components.pressable
  (:require [engine.render :as color])
  (:use
    (engine render input)
    (game settings mouseoverbody)
    (game.utils tilemap)
    (game.components core body ingame-loop render [body-render :only (body-outline-height)])))

(defcomponent pressable [:mouseover-text :pressed & {color :color}]
  {:color color})

(ingame-loop-comp :render-mouseover-body-text
  (rendering :below-gui  [g _]
    (when-let [mouseover-body (get-mouseover-body)]
      (when-let [{:keys [mouseover-text color]} (get-component mouseover-body :pressable)]
        (when (seq mouseover-text) ; rendering "" leads to just background black line ...
          (let [[body-x body-y] (screenpos-of-tilepos (get-position mouseover-body))]
            (render-readable-text g
                                  body-x
                                  (- body-y (get-half-pxh mouseover-body) body-outline-height)
                                  :centerx true
                                  :above true
                                  (or color color/white)
                                  mouseover-text)))))))

(def ^:private click-distance-tiles 1.5)
(def ^:private click-dist-sqr (* click-distance-tiles click-distance-tiles))

(ingame-loop-comp :check-pressable-mouseoverbody
  (active [_ _ _]
    (when-let [body (get-mouseover-body)]
      (when-let [{pressedfn :pressed} (get-component body :pressable)]
        (when (and (bodies-in-range? player-body body click-dist-sqr)
                   (try-consume-leftm-pressed))
          (pressedfn body))))))


