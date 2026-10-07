(ns game.player.skills.common
  (:require
    [engine.core :refer [defpreload play-sound]]
    [engine.render :refer [create-animation folder-frames get-scaled-copy]]
    [utils.core :refer [variance-val-str]]
    [game.settings :refer [in-tiles]]
    [game.components.core :refer [get-component get-position player-body]]
    [game.components.render :refer [animation-entity]]
    [game.components.destructible :refer [calc-effective-spell-dmg]]))

(defn dmg-info-player-spell [skill]
  (variance-val-str
    (calc-effective-spell-dmg
      (:dmg skill)
      (:percent-modify-spell (get-component player-body :item-boni)))))

(def curse-infostr "Curse\nOnly one curse is active at a time\n")

(defpreload ^:private curse-frames (map #(get-scaled-copy % 0.3) (folder-frames "effects/curse/")))

(defn create-curse-effect [[x y]]
  (play-sound "bfxr_curse.wav")
  (animation-entity
    :position [x (- y (in-tiles (/ 80 3)))]
    :animation (create-animation curse-frames)))
