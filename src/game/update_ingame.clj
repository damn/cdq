(ns game.update-ingame
  (:require (engine [input :as input]
                    [statebasedgame :as state]))
  (:use [game.settings :only (get-setting debug-mode)]
        [game.ingame-gui :only (some-visible-frame? close-all-frames options-hotkey)]
        (game.components [core :only (update-removelist player-body)]
                         [destructible :only (is-dead?)]
                         [ingame-loop :only (get-ingame-loop-entities)]
                         update)
        (game.maps [data :only (iterating-map-dependent-comps get-current-map-data)]
                   [mapchange :only (check-change-map)]
                   [contentfields :only (get-entities-in-active-content-fields)])
        (game.item [instance :only (put-item-on-ground)]
                   [cells :only (is-item-in-hand?)]
                   [update :only (update-items)])
        [game.player.core :only (try-revive-player player-death)]
        [game.state.main :only (mainmenu-gamestate)]
        game.player.skill.selection-list))

(defn update-game [delta]
  (when (input/is-key-pressed? options-hotkey)
    (cond
      ; when game is paused and/or the player is dead, let player be able to drop item-in-hand?
      ; or drop it automatically when dead?
      ; need to drop it here else in options menu it is still item-in-hand at cursor!
      (is-item-in-hand?) (put-item-on-ground)
      (some-visible-frame?) (close-all-frames) ; revive first before closing GUI?
      (some-skill-selection-list-visible?) (close-skill-selection-lists)
      (is-dead? player-body) (when-not (try-revive-player)
                               (state/enter-state mainmenu-gamestate))
      :else (state/enter-state game.state.ids/options)))
  (when (input/is-key-pressed? :TAB)
    (state/enter-state game.state.ids/minimap))
  (when (and (get-setting :debug-mode)
             (input/is-key-pressed? :D))
    (swap! debug-mode not))
  (when (and (get-setting :is-pausable)
             (input/is-key-pressed? :P))
    (swap! running not))
  (when @running
    (input/update-mousebutton-state)
    (update-removelist)
    ; Drag+Drop updating VOR gui update machen sonst geht es nicht mehr da gui alles consumet.
    (update-items)      ; items vor components da items leftm-consumed vlt
    ; Erst map-independent, da:
    ; - gui im vordergrund kann input consumen bevor player ihn erf�hrt bei player-body @ map-dependent-comps
    ; - map contentfields bauen neue current fields zusammen die bei update map dependent comps genutzt werden.
    (try
      (update-active-components delta (get-ingame-loop-entities))
      (catch Throwable t
        (println "Catched throwable: " t)
        (reset! running false)))

    (reset! iterating-map-dependent-comps true)
    (try
      (update-active-components delta (get-entities-in-active-content-fields))
      (catch Throwable t
        (println "Catched throwable: " t)
        (reset! running false)))
    (reset! iterating-map-dependent-comps  false)

    (when (is-dead? player-body)
      (player-death))
    (check-change-map)))
