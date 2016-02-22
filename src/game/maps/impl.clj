(ns game.maps.impl
  (:require [game.tools.transitiontilemaker16 :as gauntletly]
            (game.maps add data))
  (:use data.grid2d
        utils.core
        (engine core render)
        (game settings media)
        (game.components core position)
        (game.entity chest teleporters door)
        game.player.core
        game.player.skill.selection-list
        (game.monster spawn)
        (game.maps cell-grid tiledmaps)
        (game.item instance)
        (game.utils random lightning)
        game.tools.tiledmap-grid-convert
        (mapgen utils cave spawn-spaces populate nad cellular prebuilt-placement module))
  (:import java.util.Random))

; created&loaded in the order defined here
; map with player is first (items need player dependency and maybe more dependencies; also start the game in that map!)
; -> assert?

(def- dungeon-blockprops
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
