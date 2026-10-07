(ns game.state.loading
  (:require
    [engine.core :refer [get-screen-height get-screen-width]]
    [engine.render :as g]
    [engine.statebasedgame :as state :refer [defgamestate]]
    [utils.core :as utils]
    [game.state.ids :as ids]
    game.player.session-data))

(def is-loaded-character (atom false))

(def ^:private loading-render-once (atom false))

(defgamestate loading
  (enter [container statebasedgame]
    (reset! loading-render-once false))

  (init [container statebasedgame])

  (update [container statebasedgame delta]
    (when @loading-render-once
      (utils/log "Loading new session")
      (game.player.session-data/init @is-loaded-character)
      (utils/log "Finished loading new session")
      (state/enter-state ids/ingame)))

  (render [container statebasedgame g]
    (reset! loading-render-once true)
    (g/render-readable-text g (/ (get-screen-width) 2) (/ (get-screen-height) 2) :centerx true "Loading...")))
