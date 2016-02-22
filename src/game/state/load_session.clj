(ns game.state.load-session
  (:require (engine [core :as core]
                    [render :as g]
                    [statebasedgame :as state])
            game.player.session-data
            game.state.ids)
  (:use [utils.core :only (log)]))

(def is-loaded-character (atom false))

(def ^:private render-once (atom false))

(state/defgamestate loading
  (enter [container statebasedgame]
    (reset! render-once false))

  (init [container statebasedgame])

  (update [container statebasedgame delta]
    (when @render-once
      (log "Loading new session")
      (game.player.session-data/init @is-loaded-character)
      (log "Finished loading new session")
      (state/enter-state game.state.ids/ingame)))

  (render [container statebasedgame g]
    (reset! render-once true)
    (g/render-readable-text g (/ (core/get-screen-width) 2) (/ (core/get-screen-height) 2) :centerx true "Loading...")))

