(ns game.monsters.research-station
  (:require
    [engine.core :refer [play-sound]]
    [engine.render :refer [folder-animation orange]]
    [game.components.render :refer [single-animation-component]]
    [game.maps.minimap :refer [show-on-minimap]]
    [game.monster.defmonster :refer [defmonster]]
    [game.monsters.common :refer [big-body-hit-effect death-trigger rand-posis-hit-effect]]))

(let [posis [[9 -11] [-1 -11] [-13 -7] [-9 2] [3 3] [3 15] [-5 14] [7 17] [-11 5] [-12 14]]]
  (defmonster research-station {:hp 10 :armor 25 :pxw 43 :pxh 43}
    (big-body-hit-effect posis)
    (death-trigger (fn [body]
                     (play-sound "bfxr_stationdeath.wav")
                     (rand-posis-hit-effect body posis :big-explosion true)))
    (show-on-minimap orange)
    (single-animation-component
      (folder-animation :folder "opponents/station/" :duration 500 :looping true))))
