(ns game.state.minimap
  (:use (engine [input :only (is-key-pressed?)] statebasedgame)
        [game.maps.minimap :only (render-minimap)]))

(defgamestate minimap game.state.ids/minimap
  (enter [container statebasedgame])

  (init [container statebasedgame])

  (update [container statebasedgame delta]
    ; use is-key-pressed? instead of gamestate keyPressed because one of them consumes the keypress...
    (when (or (is-key-pressed? :TAB)
              (is-key-pressed? :ESCAPE))
      (enter-state game.state.ids/ingame)))

  (render [container statebasedgame g]
    (render-minimap g)))
