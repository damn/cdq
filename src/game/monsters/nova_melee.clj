(ns game.monsters.nova-melee
  (:require
    [engine.core :refer [create-sound defpreload]]
    [engine.render.animation :refer [create-animation]]
    [engine.render.assets :refer [folder-animation folder-frames]]
    [game.components.core :refer [player-body]]
    [game.components.body :refer [circle-collides?]]
    [game.components.render :refer [single-animation-component]]
    [game.components.active :refer [blocks-component]]
    [game.components.skills.core :refer [enough-mana? is-ready? skillmanager-component skillmanager-skill]]
    [game.components.skills.melee :refer [melee-weapon monster-melee-props]]
    [game.components.movement.ai.potential-field :refer [path-to-player-movement]]
    [game.entity.nova :refer [nova-effect]]
    [game.utils.random :refer [rand-int-between]]
    [game.utils.raycast :refer [ray-blocked?]]
    [game.monster.defmonster :refer [defmonster]]
    [game.monsters.common :refer [default-death-trigger]]))

(def ^:private monster-nova-radius 4)

(defn- player-in-nova-range? [monster]
  (circle-collides? (:value (:position @monster)) monster-nova-radius player-body))

(defn- choose-active-nova-melee [entity skillmanager delta]
  (let [skills (:skills skillmanager)
        melee (:melee skills)
        nova (:monster-nova skills)]
    (if
      (and
        (enough-mana? nova skillmanager)
        (is-ready? nova)
        (player-in-nova-range? entity)
        (not (ray-blocked? (:value (:position @entity)) (:value (:position @player-body))))
        (zero? (rand-int 240)))
      :monster-nova
      :melee)))

(defpreload ^:private monster-nova-frames (folder-frames "effects/monsternova/"))

(defn- create-nova-melee-skillmanager []
  (let [melee-weapon (skillmanager-skill
                       :stype :melee
                       :cooldown 1000
                       :attacktime 500
                       :cost 0
                       (monster-melee-props (:id (meta player-body)) (melee-weapon [3 8] (create-sound "slash.wav"))))
        monster-nova (skillmanager-skill
                       :stype :monster-nova
                       :cooldown (rand-int-between 1000 2000)
                       :attacktime 1500
                       :cost 1
                       {:shoot-sound "bfxr_monstercast.wav"
                        :do-skill (fn [entity component]
                                    (nova-effect
                                      :position (:value (:position @entity))
                                      :duration 400
                                      :maxradius monster-nova-radius
                                      :affects-side :player
                                      :dmg [18 22]
                                      :animation (create-animation monster-nova-frames)))})]
    (skillmanager-component
      :choosefn choose-active-nova-melee
      :mana (rand-int-between 2 4)
      :skills [melee-weapon monster-nova]
      (blocks-component {:attacking :movement}))))

(defmonster nova-melee {:hp 2.2 :armor 7 :pxw 15 :pxh 15}
  (default-death-trigger)
  (path-to-player-movement 32)
  (single-animation-component
    (folder-animation :folder "opponents/gravturret/" :duration 1000 :looping true))
  (create-nova-melee-skillmanager))
