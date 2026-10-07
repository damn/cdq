(ns game.maps.impl
  (:require
    [game.tools.transitiontilemaker16 :as gauntletly]
    game.maps.add
    game.maps.data
    [data.grid2d :refer [mapgrid->vectorgrid posis transform]]
    [utils.core :refer [log translate-to-tile-middle]]
    engine.core
    [engine.render.assets :refer [get-sprite spritesheet]]
    game.settings
    game.media
    game.components.core
    game.components.position
    [game.entity.chest :refer [create-chest]]
    [game.entity.teleporters :refer [static-teleporter]]
    [game.entity.door :refer [make-door]]
    game.player.core
    game.player.skill.selection-list
    [game.monster.spawn :refer [bloodcaves-groups spawn-monsters tech-groups techgy-groups try-spawn]]
    game.maps.cell-grid
    [game.maps.tiledmaps :refer [construct-tiledmap create-grid-from-tiled-map]]
    game.item.instance
    [game.utils.random :refer [rand-int-between]]
    game.utils.lightning
    [game.tools.tiledmap-grid-convert :refer [get-spriteidx]]
    [mapgen.utils :refer [fill-single-cells scalegrid undefined-value-behind-walls]]
    [mapgen.cave :refer [cave-gridgen]]
    mapgen.spawn-spaces
    [mapgen.populate :refer [get-populated-grid-posis]]
    [mapgen.nad :refer [fix-not-allowed-diagonals]]
    [mapgen.cellular :refer [cellular-automata-gridgen connect-regions]]
    [mapgen.prebuilt-placement :refer [is-5x5-walls-entrance?]]
    [mapgen.module :refer [make-module-based-grid]])
  (:import java.util.Random))

; created&loaded in the order defined here
; map with player is first (items need player dependency and maybe more dependencies; also start the game in that map!)
; -> assert?

(def ^:private dungeon-blockprops
  {:undefined nil
   :airwalkable #{:ground}
   :wall #{:ground :air}
   :ground #{}})

(def gauntletly1-tilesheet (spritesheet "maps/gauntletly1.png" 16 16))
(def gauntletly2-tilesheet (spritesheet "maps/gauntletly2.png" 16 16))
(def gauntletly3-tilesheet (spritesheet "maps/gauntletly3.png" 16 16))
(def details-sprite-sheet  (spritesheet "maps/details.png"     16 16))

;(load "impl_generated")
(load "impl_modules")

#_(game.maps.add/deftilemap :all-monsters-map
  :pretty-name "All Monsters Map"
  :file "all_monsters_map.tmx"
  :spawn-monsters (fn [])
  :load-content (fn [])
  :rand-item-max-lvl 1)
