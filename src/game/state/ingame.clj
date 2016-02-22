(ns game.state.ingame
  (:require [engine.input :as input]
            game.state.ids
            game.update-ingame)
  (:use (engine [core :only (app-game-container)] statebasedgame)
        (game settings screenshake [render-ingame :only (render-game)])))

(defn- limit-delta "prevents too high movement updates for example"
  [delta]
  (min delta game.components.update/max-delta))

(defgamestate ingame game.state.ids/ingame
  (enter [container statebasedgame]
    ; clear presses from menu-state/options-state before ingame-state
    (input/clear-key-pressed-record)
    (input/clear-mouse-pressed-record))

  (init [container statebasedgame])

  (update [container statebasedgame delta]
    (let [delta (limit-delta delta)]
      (update-shake delta)
      (game.update-ingame/update-game delta)))

  (render [container statebasedgame g]
    (translate-shake-before-render g)
    (render-game g)
    (translate-shake-after-render g))

  (keyPressed [int-key chr]))
