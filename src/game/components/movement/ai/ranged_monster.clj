(ns game.components.movement.ai.ranged-monster
  (:require [engine.core :refer [make-counter]])
  (:use
    utils.core
    (game.components core body movement destructible)
    game.components.skills.core
    (game.utils raycast geom random)
    game.components.movement.ai.potential-field))

(defn- runaway [body component]
  (cond
    (not (is-usable? body (get-skill body :ranged)))
    (potential-field-player-following body)

    (bodies-in-range? body player-body (:runaway-dist-sqrd component))
    (get-vector-away-from-player body)

    :else
    nil))

(defn- randomise [body _]
  (cond
    (not (is-usable? body (get-skill body :ranged)))
    (potential-field-player-following body)

    :else
    (normalise
      (vector2f
        (if-chance 50 (rand) (- (rand)))
        (if-chance 50 (rand) (- (rand)))))))

(defn- duration-counter-control
  [body {:keys [current-movement-vector create-vectorfn counter] :as component} delta]
  (if (or (nil? current-movement-vector)
          (update-counter! body delta component))
    (let [v (create-vectorfn body component)]
      (assoc-in! body [:movement :current-movement-vector] v)
      v)
    current-movement-vector))

(defn ranged-runaway-movement-comp
  ([speed runaway-dist-in-tiles move-type]
    (movement-component
      {:control-update duration-counter-control
       :create-vectorfn runaway
       :counter (make-counter 300)
       :current-movement-vector nil
       :runaway-dist-sqrd (Math/pow runaway-dist-in-tiles 2)}
      speed
      move-type)))

(defn ranged-randomly-moving-comp
  [speed duration move-type]
  (movement-component
    {:control-update duration-counter-control
     :create-vectorfn randomise
     :counter (make-counter duration)
     :current-movement-vector nil}
    speed
    move-type))

; hat nix mit ranged_monster zu tun -> tu woanders hin
; wenn hp wieder max ist (also geheilt wurden -> finished!)
(defn- update-lowhp-runaway [body {:keys [running-away counter] :as c} delta]
  (let [finished (and running-away (update-counter! body delta c))]
    (when finished
      (assoc-in! body [:movement :running-away] false))
    (if (and running-away (not finished))
      (get-vector-away-from-player body)
      (potential-field-player-following body))))

(defn lowhp-runaway-movement [speed]
  (movement-component {:control-update update-lowhp-runaway
                       :running-away false
                       :counter (make-counter 0)}
                      speed
                      :ground))

(defn rand-when-low-hp [body]
  (->> body get-hp get-ratio (- 1) (* 100) percent-chance))

(defn lowhp-dealt-dmg-trigger [body lethal]
  (let [move-comp (get-component body :movement)]
    (when (and (not lethal)
               (not (:running-away move-comp))
               (rand-when-low-hp body))
      (update-in! body [:movement]
                  #(-> %
                       (assoc-in [:counter :maxcnt] (* (rand-int-between 3 10) 1000))
                       (assoc-in [:running-away] true))))))
