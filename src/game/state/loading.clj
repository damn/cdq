(ns game.state.loading
  (:import org.newdawn.slick.state.BasicGameState)
  (:require
    [engine.core :refer [get-screen-height get-screen-width]]
    [engine.render.graphics :as g]
    [engine.statebasedgame :as state]
    [utils.core :as utils :refer [get-unique-number]]
    [game.state.ids :as ids]
    game.player.session-data))

(def is-loaded-character (atom false))

(def ^:private loading-render-once (atom false))

(def loading-gamestate
  (let [id (get-unique-number)]
    (proxy [BasicGameState] []
      (getID [] id)

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
        (g/render-readable-text g (/ (get-screen-width) 2) (/ (get-screen-height) 2) :centerx true "Loading...")))))
