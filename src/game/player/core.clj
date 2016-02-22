(ns game.player.core
  (:require [engine.render :as color]
            game.components.update
            game.maps.data)
  (:use utils.core
        (engine core input)
        (game.maps contentfields [minimap :only (show-on-minimap)])
        (game.components core active position movement misc render destructible body body-effects item-boni)
        game.item.cells
        game.components.movement.ai.potential-field
        [game.components.skills.core :only (reset-skills)]
        (game.player movement animation)
        (game.player.skill skillmanager learnable)
        (game.utils geom msg-to-player [lightning :only (light-component)])))

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
    (game.components.update/update-component 0 (get-component body :animation) body) ; set death animation
    (assoc-in! player-body [:destructible :hp :current] 0)
    (set-rotation-angle body 0)))

(defn- revive-player []
  (show-msg-to-player "") ; removes the old msg
  (reset! game.components.update/running true)
  (teleport player-body (:start-position (game.maps.data/get-current-map-data)))
  (->! player-body
    (assoc-in [:destructible :is-dead] false)
    (update-in [:destructible :hp] set-to-max)
    (update-in [:skillmanager :mana] set-to-max)
    (switch-state :skillmanager :ready)
    (update-in [:skillmanager :skills] reset-skills)))

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

