(ns game.entity.nova
  (:require
    [engine.core :refer [make-counter ratio]]
    [game.utils.geom :as geom]
    [game.components.position :refer [position-component]]
    [game.components.destructible :refer [deal-dmg get-destructible-bodies]]
    [clojure.set :refer [union]]
        [engine.render :refer [render-centered-shape rgbcolor]]
    [game.settings :refer [in-pixel]]
    [game.components.core :refer [active add-to-removelist create-comp create-entity create-entity-no-init defentity update-counter!]]
    [game.components.render :refer [render-on-map single-animation-component]]))

(defn- circle-debug-comp []
  (let [color (rgbcolor :r 0.6 :a 0.6)]
    (create-comp :circle-debug
      (render-on-map [g _ {:keys [shape color]} render-posi]
        (render-centered-shape g shape render-posi color))
      (active [delta c entity]
        (geom/set-radius (:shape c)
                         (in-pixel
                          (:radius (:nova-effect @entity)))))
      {:shape (geom/circle [-1 -1] 0)})))

(defentity nova-effect [:position :duration :maxradius :affects-side :dmg :animation :opt :is-player-spell]
  ;(circle-debug-comp)
  (position-component position)
  (create-comp :always-in-sight) ; because affects entities behind walls
  (create-comp :nova-effect
    (active [delta {:keys [counter already-hit] :as c} entity]
      (let [radius (* (ratio counter) maxradius) ; erst ratio danach update-counter, sonst beim letzten update => ratio=0
            hits (remove
                   #(contains? already-hit %)
                   (get-destructible-bodies position radius affects-side))]
        (dorun (map #(deal-dmg dmg % :is-player-spell is-player-spell) hits))
        (swap! entity update-in [(:type c)]
                    #(-> %
                         (assoc :radius radius)
                         (update-in [:already-hit] union (set hits))))
        (when (update-counter! entity delta c)
          (add-to-removelist entity))))
    {:radius 0
     :already-hit #{}
     :counter (make-counter duration)})
  (single-animation-component animation :order :air :apply-light false))
