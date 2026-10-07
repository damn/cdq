(ns game.monsters.instant-healer
  (:require
    [engine.core :refer [create-sound make-counter]]
    [engine.render :refer [create-image green]]
    [utils.numbers :refer [lower-than-max?]]
    [game.components.core :refer [active defcomponent exists? get-id get-position player-body update-counter!]]
    [game.components.body :refer [bodies-in-range?]]
    [game.components.misc :refer [rotation-component]]
    [game.components.render :refer [create-line-render-effect image-render-component]]
    [game.components.destructible :refer [get-hp is-dead? set-hp-to-max]]
    [game.components.skills.core :refer [standalone-skill]]
    [game.components.skills.melee :refer [monster-melee-component]]
    [game.components.movement.ai.potential-field :refer [path-to-player-movement]]
    [game.monster.defmonster :refer [defmonster]]
    [game.monsters.common :refer [default-death-trigger get-healable-monsters-around heal-radius]]))

(defcomponent :cache-nearby-monsters []
  {:counter (make-counter 1000)}
  (active [delta {:keys [counter] :as c} entity]
    (when (update-counter! entity delta c)
      (swap! entity assoc-in [(:type c) :nearby-monsters]
                 (doall (get-healable-monsters-around entity))))))

; TODO not checking if ray-blocked ---> can heal through walls like other healer
; navigation meshes would make ray-blocked much simpler if in the same polygon/area
(defn- healing-required-and-allowed? [entity healer radius-squared]
  (and (exists? entity)
       (not (is-dead? entity))
       (lower-than-max? (get-hp entity))
       (bodies-in-range? entity healer radius-squared)))

; TODO mehr hervorheben anstatt drawLine vlt so ein "beam" strahl... grün mit weiss als kontrast (not in prototype stage -> do another time!)
; rotating to which monster is healed?
(let [healradius-squared (* heal-radius heal-radius)]
  (defmonster instant-healer {:hp 2 :armor 20 :pxw 14 :pxh 14}
    (default-death-trigger)
    (path-to-player-movement 15)
    (rotation-component)
    (image-render-component (create-image "opponents/coreturret.png"))
    (cache-nearby-monsters-component)
    (monster-melee-component :cooldown 500
                             :attacktime 100
                             :hit-sound (create-sound "slash.wav")
                             :target-id (get-id player-body))
    (standalone-skill
      :stype :instantheal
      :cooldown 1000
      :attacktime 150
      :props {:shoot-sound "bfxr_instanthealer_heal.wav"
              :check-usable (fn [entity _]
                              (when-let [cached (:nearby-monsters (:cache-nearby-monsters @entity))]
                                (when-let [needs-heal (first (sort-by #(:current (get-hp %))
                                                                      (filter #(healing-required-and-allowed? % entity healradius-squared)
                                                                              cached)))]
                                  (swap! entity assoc-in [:instantheal :needs-heal] needs-heal)
                                  true)))
              :do-skill (fn [healer {needs-heal :needs-heal :as component}]
                          (when (healing-required-and-allowed? needs-heal healer healradius-squared)
                            (set-hp-to-max needs-heal)
                            (create-line-render-effect (get-position healer) (get-position needs-heal) 200 :color green)))})))
