(ns game.start
  (:use
    [utils.core :as utils]
    [engine.settings :refer (jar-file?)]
    (engine core statebasedgame)
    (game settings [resolutionsetup :only (resolution-setup-frame)])
    (game.state [main :only (deferred-preload-gamestate mainmenu-gamestate)]
                [load-session :only (loading-gamestate)]
                [ingame :only (ingame-gamestate)]
                [minimap :only (minimap-gamestate)]
                [options :only (options-gamestate)]))
  (:load "ns_without_deps")
  (:gen-class))

(defn- start-the-game [fullscreen]
  (init-state-based-game "Cyber-Dungeon-Quest"
    [deferred-preload-gamestate
     mainmenu-gamestate
     loading-gamestate
     ingame-gamestate
     options-gamestate
     minimap-gamestate])
  (create-and-start-app-game-container
    :game state-based-game
    :width screen-width
    :height screen-height
    :scale screen-scale
    :full-screen fullscreen
    :show-fps (get-setting :show-fps)
    :lock-framerate false))

(defn start [& {:keys [config]}]
  (let [development-mode (if-let [dev (System/getenv "development")]
                           (Boolean/valueOf dev)
                           (not jar-file?))
        file (cond config config
                   development-mode "config/development.clj"
                   :else "config/production.clj")]
    (utils/log "Using config file: " file)
    (init-config! file))
  (if (get-setting :show-resolution-setup)
    (resolution-setup-frame start-the-game)
    (start-the-game false)))

(defn -main []
  (start))

; config is not initialized before 'start'
;(when (get-setting :start-automatically)
;  (start))
