(ns game.player.core
  (:require
    [engine.render.color :as color]
    game.components.update
    game.maps.data
    [game.maps.minimap :refer [show-on-minimap]]
    [game.components.skills.core :refer [reset-skills]]
    [game.utils.lightning :refer [light-component]]
    [utils.numbers :refer [set-to-max]]
    [utils.coll :refer [when-seq]]
    [engine.core :refer [play-sound]]
    engine.input
    game.maps.contentfields
    [game.components.core :refer [add-to-removelist create-entity create-entity-no-init defentity player-body]]
    [game.components.active :refer [switch-state]]
    [game.components.position :refer [position-component]]
    game.components.movement
    [game.components.misc :refer [mana-regen-component rotation-component set-rotation-angle]]
    game.components.render
    [game.components.destructible :refer [destructible-component player-start-hp]]
    [game.components.body :refer [create-body teleport]]
    [game.components.body-effects :refer [get-sub-entities]]
    [game.components.item-boni :refer [item-boni-component]]
    [game.item.cells :refer [get-inventory-cells-with-item-name remove-item-from-cell]]
    [game.components.movement.ai.potential-field :refer [potential-field-component]]
    [game.player.movement :refer [player-movement-component]]
    [game.player.animation :refer [player-animation]]
    [game.player.skill.skillmanager :refer [create-player-skillmanager]]
    game.player.skill.learnable
    game.utils.geom
    [game.utils.msg-to-player :refer [show-msg-to-player]]))

; use require and remove the -player prefixes or suffixes here

(defn player-death
  "this is NOT implemented as a death-trigger (immediately called at deal-dmg and hp<0) because the snapshot and update order of components
  affected the player-body after the death event ->  the rotation-angle of player-body was altered in rare cases.
  To be independent of the order of entitiy updates call this after the frame is finished updating.
  TLDR: player components affected player-body after deathtrigger happened."
  []
  (show-msg-to-player (str "You died!\nPress ESCAPE to "
                           (let [restorations (get-inventory-cells-with-item-name "Restoration" :inventory)]
                             (if (seq restorations)
                               (str "be revived. Restorations left: " (dec (count restorations)) "")
                               "exit the game."))))
  (let [body player-body]
    (reset! game.components.update/running false)
    (play-sound "bfxr_playerdeath.wav")
    (dorun (map add-to-removelist (get-sub-entities body)))
    (game.components.update/update-component 0 (:animation @body) body) ; set death animation
    (swap! player-body assoc-in [:destructible :hp :current] 0)
    (set-rotation-angle body 0)))

(defn- revive-player []
  (show-msg-to-player "") ; removes the old msg
  (reset! game.components.update/running true)
  (teleport player-body (:start-position (game.maps.data/get-current-map-data)))
  (swap! player-body #(-> % 
    (assoc-in [:destructible :is-dead] false)
    (update-in [:destructible :hp] set-to-max)
    (update-in [:skillmanager :mana] set-to-max)
    (switch-state :skillmanager :ready)
    (update-in [:skillmanager :skills] reset-skills))))

(defn try-revive-player []
  (when-seq [cells (get-inventory-cells-with-item-name "Restoration" :inventory)]
    (remove-item-from-cell (rand-nth cells))
    (revive-player)
    true))

(defentity ^:private create-player-body [position]
  (position-component position)
  (create-body :solid true
               :side :player
               :pxw 14
               :pxh 14
               :mouseover-outline true)
  (create-player-skillmanager)
  (player-movement-component)
  (destructible-component player-start-hp 0)
  (rotation-component)
  (item-boni-component)
  (mana-regen-component 5)
  (light-component :intensity 1 :radius 12 :falloff 5) ; (/ screen-height 2 16) = 9
  (player-animation)
  (show-on-minimap color/red)
  (potential-field-component))

(defn init-player [position]
  (intern 'game.components.core 'player-body (create-player-body position)))

