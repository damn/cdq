(ns game.monsters.storagebox
  (:require
    [engine.render :refer [create-image]]
    [game.components.render :refer [image-render-component]]
    [game.monster.defmonster :refer [defmonster]]))

(defmonster storagebox {:hp 1 :armor 25 :pxw 41 :pxh 41}
  ;(default-death-trigger)
  (image-render-component (create-image "opponents/storage1.png")))
