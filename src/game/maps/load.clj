(ns game.maps.load
  (:require [game.maps.data :as data]
            game.player.core
            game.utils.lightning)
  (:use [utils.core :only (log translate-to-tile-middle)]
        game.session
        game.maps.tiledmaps
        [game.monster.spawn :only (try-spawn)]))

(defn- place-entities [tiled-map]
  ; looping through all tiles of the map 3 times. but dont do it in 1 loop because player needs to be initialized before all monsters!
  (doseq [[posi _] (get-tileproperties tiled-map "entities" "light")]
    (game.utils.lightning/map-lightsource (translate-to-tile-middle posi)))
  (doseq [[posi value] (get-tileproperties tiled-map "entities" "monster")]
    (try-spawn (translate-to-tile-middle posi) (keyword value) :debug false)))

(defn load-maps-content
  "loads the map content in the right order"
  []
  (game.player.core/init-player (:start-position (data/get-current-map-data)))
  (doseq [map-name data/added-map-order]
    (data/do-in-map map-name
                    (let [{:keys [load-content spawn-monsters tiled-map]} (data/get-map-data map-name)]
                      (when tiled-map (place-entities tiled-map))
                      (load-content)
                      (when spawn-monsters (spawn-monsters))))
    (log "Loaded " map-name " content!")))

(def session (reify game.session/Session
               (load-session [_ mapkey]
                 (data/set-map! mapkey)
                 (load-maps-content))
               (save-session [_]
                 @data/current-map)
               (new-session-data [_]
                 (first data/added-map-order))))






