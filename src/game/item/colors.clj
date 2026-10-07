(ns game.item.colors
  (:require
    [engine.render.color :refer [defcolor]]))

; diablo 2 gold:  144 136 88
; blue:           72 80 184
(defcolor equip-boni-item-color :r 0.28 :g 0.31 :b 0.72 :brighter 0.5)
(defcolor gold-item-color :r 0.56 :g 0.53 :b 0.34 :brighter 0.5)
