(ns game.maps.impl
  (:require [cdq.core :refer :all])
  (:import java.util.Random))

(def gauntletly1-tilesheet (spritesheet "maps/gauntletly1.png" 16 16))
(def gauntletly2-tilesheet (spritesheet "maps/gauntletly2.png" 16 16))
(def gauntletly3-tilesheet (spritesheet "maps/gauntletly3.png" 16 16))
(def details-sprite-sheet  (spritesheet "maps/details.png"     16 16))

;(load "impl_generated")
(load "impl_modules")
