(ns game.components.sleeping
  (:require [game.utils.geom :as geom])
  (:use
   engine.render
   utils.core
   (game session settings)
   (game.components core active render ingame-loop body)
   (game.maps camera contentfields cell-grid)))

(def- half-aggro-width-tiles 11)
(def- half-aggro-height-tiles 8.5)

; > wakeup-range-rectangle
; (because wakeup checks with rectangle collision..
; and this is center to center distance ...)
; RADIUS needs to be at least half the diagonal of the rectangle!
; so no entities will be awoken which are at corner...
(def start-position-safezone-radius (+ 2.2 half-aggro-width-tiles))

(defn- in-wakeup-range? [body]
  (geom/rect-collision? (get-body-bounds body)
                        [(get-camera-position)
                         half-aggro-width-tiles
                         half-aggro-height-tiles]))
; TODO not camera but player ... if we seperate them at some point...

(defn- is-sleeping? [entity] (:sleeping (get-component entity :sleeping)))

(defcomponent sleeping []
  {:depends [:body]
   :sleeping true
   :init #(swap! % block-active-components)})

(defn wake-up [entity]
  (when (is-sleeping? entity)
    (->! entity
         unblock-active-components
         (assoc-in [:sleeping :sleeping] false))
    (->> entity
      get-other-bodies-in-adjacent-cells
      (filter is-sleeping?)
      (runmap wake-up))))

(def ^:private counter (create-counter 1000))
(def session (atom-session counter :save-session false))

(def- show-range false)

(ingame-loop-comp :wakeup

  (active [delta c _]
    (when (update-counter counter delta)
      (runmap wake-up
              (filter in-wakeup-range?
                      (filter is-sleeping?
                              (get-entities-in-active-content-fields))))))
  (rendering :above-gui [g c]
    (when show-range
      (render-centered-shape
        g
       (geom/rectangle 0 0
                  (in-pixel (* 2 half-aggro-width-tiles))
                  (in-pixel (* 2 half-aggro-height-tiles)))
        [(/ screen-width 2)
         (/ screen-height 2)])
;      (render-centered-shape g
;                             (geom/circle [0 0]
      ;                             (in-pixel start-position-safezone-radius))
;                             [(/ screen-width 2)
;                              (/ screen-height 2)])
      (render-readable-text g 50 100
                            (->> (get-entities-in-active-content-fields)
                                 (filter is-sleeping?)
                                 (filter in-wakeup-range?)
                                 count
                                 (str "In wakeup range&sleeping: "))))))





