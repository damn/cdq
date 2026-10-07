(ns game.monsters.test-hunter
  (:require
    [engine.render.image :refer [create-image]]
    [game.components.misc :refer [rotation-component]]
    [game.components.render :refer [image-render-component]]
    [game.components.movement.ai.potential-field :refer [path-to-player-movement]]
    [game.monster.defmonster :refer [defmonster]]))

(defmonster test-hunter {:hp 1.5 :armor 7 :pxw 47 :pxh 47}
  (path-to-player-movement 72)
  (rotation-component)
  (image-render-component (create-image "opponents/coredemonhand.png")))
