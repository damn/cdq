(ns game.monsters.mine
  (:require
    [engine.core :refer [defpreload play-sound]]
    [engine.render :refer [create-animation create-image folder-frames spritesheet-frames]]
    [game.components.core :refer [get-position]]
    [game.components.render :refer [image-render-component single-animation-component]]
    [game.entity.nova :refer [nova-effect]]
    [game.monster.defmonster :refer [defmonster]]
    [game.monsters.common :refer [death-trigger]]))

(defpreload ^:private mine-explosion-frames (folder-frames "effects/mine/"))

(defmonster mine {:hp 1 :armor 0 :pxw 15 :pxh 15}
  (death-trigger (fn [this-body]
                   (play-sound "bfxr_minedeath.wav")
                   (nova-effect
                     :position (get-position this-body)
                     :duration 300
                     :maxradius 4
                     :affects-side [:player]
                     :dmg [30 40]
                     :animation (create-animation mine-explosion-frames))
                   (nova-effect
                     :position (get-position this-body)
                     :duration 300
                     :maxradius 4
                     :affects-side [:monster]
                     :dmg [6 8]
                     :animation (create-animation mine-explosion-frames))))
  (image-render-component (create-image "opponents/mine.png"))
  (single-animation-component
    (create-animation (spritesheet-frames "effects/red_glow.png" 32 32)
                      :frame-duration 150
                      :looping true)
    :order :on-ground))
