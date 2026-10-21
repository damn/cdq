(ns game.state.minimap
  (:import org.newdawn.slick.state.BasicGameState)
  (:require
    [engine.input :refer [is-key-pressed?]]
    [engine.statebasedgame :refer [enter-state]]
    [game.maps.minimap :refer [render-minimap]]
    [game.state.ids :as ids]))

(def minimap-gamestate
  (proxy [BasicGameState] []
    (getID [] ids/minimap)

    (enter [container statebasedgame])

    (init [container statebasedgame])

    (update [container statebasedgame delta]
      (when (or (is-key-pressed? :TAB)
                (is-key-pressed? :ESCAPE))
        (enter-state ids/ingame)))

    (render [container statebasedgame g]
      (render-minimap g))))
