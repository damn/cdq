(ns game.monsters.fly
  (:require
    [engine.render :refer [create-animation create-image folder-animation]]
    [game.components.core :refer []]
    [game.components.render :refer [animation-component]]
    [game.components.skills.core :refer [is-attacking?]]
    [game.monster.defmonster :refer [defmonster]]
    [game.monsters.common :refer [default-death-trigger ranged-component ranged-randomly-moving-comp]]))

(let [attacktime 600
      folder "opponents/fly/"]
  (defmonster fly {:hp 1 :armor 0 :pxw 9 :pxh 9}
    (default-death-trigger)
    (ranged-randomly-moving-comp 32 1000 :ground)
    (animation-component (fn [body]
                           (if (is-attacking? (:ranged @body)) :attack :default))
                         {:attack (folder-animation :folder folder :prefix "attack" :duration attacktime :looping false)
                          :default (create-animation [(create-image (str folder "fly.png"))])})
    (ranged-component :cooldown 2000 :state-blocks {}))) ; move+shoot
