(ns game.monsters.healer
  (:require
    [engine.core :refer [make-counter]]
    [engine.render.assets :refer [folder-animation]]
    [game.components.core :refer [update-counter!]]
    [game.components.misc :refer [rotate-to-player rotation-component]]
    [game.components.render :refer [create-lines-render-effect single-animation-component]]
    [game.components.destructible :refer [get-hp set-hp-to-max]]
    [game.components.active :refer [blocks-component]]
    [game.components.skills.core :refer [enough-mana? is-ready? skillmanager-component skillmanager-skill]]
    [utils.numbers :refer [lower-than-max?]]
    [game.utils.random :refer [rand-int-between]]
    [game.monster.defmonster :refer [defmonster]]
    [game.monsters.common :refer [default-death-trigger get-healable-monsters-around redball-projectile-skill-props ranged-runaway-movement-comp]]))

(defn- monsters-need-healing? [healer]
  (some #(lower-than-max? (get-hp %))
        (get-healable-monsters-around healer)))

(defn- choose-active-healer [entity skillmanager delta]
  (let [skills (:skills skillmanager)
        heal-spell (:healing skills)
        ranged-weapon (:ranged skills)]
    (if (and (update-counter! entity delta skillmanager)
             (enough-mana? heal-spell skillmanager)
             (is-ready? heal-spell)
             (monsters-need-healing? entity)
             (zero? (rand-int 5)))
      :healing
      :ranged)))

(defn- heal-nearby-monsters
  "returns the healed monsters"
  [healer]
  (doall (remove nil?
                 (map #(when (lower-than-max? (get-hp %))
                         (set-hp-to-max %) %)
                      (get-healable-monsters-around healer)))))

(defn- create-healer-skillmanager []
  (let [ranged-weapon (skillmanager-skill
                        :stype :ranged
                        :cooldown 4000
                        :attacktime 400
                        :cost 0
                        (redball-projectile-skill-props))
        heal-spell (skillmanager-skill
                     :stype :healing
                     :cooldown (rand-int-between 2000 3000)
                     :attacktime 500
                     :cost 0
                     {:shoot-sound "bfxr_healmonsters.wav"
                      :do-skill (fn [healer component]
                                  (let [healed-monsters (heal-nearby-monsters healer)]
                                    (create-lines-render-effect healer healed-monsters 500)))})]
    (skillmanager-component
      :rotatefn rotate-to-player
      :choosefn choose-active-healer
      :mana 0
      :skills [heal-spell ranged-weapon]
      (blocks-component {:attacking :movement})
      ; heilen trotzdem alle auf 1mal da 1sek attacktime?
      {:counter (make-counter 500)})))

; TODO also use cached-monsters-around ?
(defmonster healer {:hp 3 :armor 0 :pxw 15 :pxh 15}
  (default-death-trigger)
  (ranged-runaway-movement-comp 30 (rand-int-between 1 3) :ground)
  (rotation-component)
  (single-animation-component
    (folder-animation :folder "opponents/healer/" :duration 1000 :looping true))
  (create-healer-skillmanager))
