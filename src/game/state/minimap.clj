(ns game.state.minimap
  (:require
    [engine.input :refer [is-key-pressed?]]
    [engine.statebasedgame :refer [defgamestate enter-state]]
    [game.maps.minimap :refer [render-minimap]]
    [game.state.ids :as ids]))

(defgamestate minimap ids/minimap
  (enter [container statebasedgame])

  (init [container statebasedgame])

  (update [container statebasedgame delta]
    (when (or (is-key-pressed? :TAB)
              (is-key-pressed? :ESCAPE))
      (enter-state ids/ingame)))

  (render [container statebasedgame g]
    (render-minimap g)))
