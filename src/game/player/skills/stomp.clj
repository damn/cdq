(ns game.player.skills.stomp
  (:require
    [engine.core :refer [play-sound]]
    [engine.render.assets :refer [folder-animation]]
    [utils.numbers :refer [variance-val-str]]
    [game.components.core :refer [player-body]]
    [game.components.body-effects-impl :refer [consume-psi-charges current-psi-charges stun]]
    [game.components.destructible :refer [deal-dmg get-destructible-bodies]]
    [game.components.render :refer [animation-entity]]
    [game.components.skills.melee :refer [get-current-player-melee-dmg]]
    [game.utils.random :refer [rand-int-between]]
    [game.player.skill.learnable :refer [deflearnable-skill]]))

(deflearnable-skill stomp
  :manacost 10
  :menu-posi [1 1]
  :mousebutton :right
  :icon "icons/stomp.png"
  :info "Finisher-Skill
Stuns and deals damage

0 Charges - 1s stun duration
1 Charge  - 1.3s stun duration
            20% melee weapon damage
2 Charges - 1.6s stun duration
            40% melee weapon damage
3 Charges - 2s stun duration
            60% melee weapon damage"
  {:dmg-info (fn [skill]
               (let [cnt (current-psi-charges player-body)
                     [mn mx] (get-current-player-melee-dmg)
                     modifier (* cnt (:dmg-modifier skill))]
                 (variance-val-str
                   [(* modifier mn) (* modifier mx)])))
   :radius 2
   :dmg-modifier 0.2
   :animation :casting
   :show-info-for [:cost]
   :do-skill (fn [entity {:keys [radius dmg-modifier] :as component}]
               (let [posi (:value (:position @entity))
                     cnt (consume-psi-charges entity)
                     duration (+ 1000 (* cnt 333))
                     dmg (* cnt dmg-modifier (rand-int-between (get-current-player-melee-dmg)))
                     hits (get-destructible-bodies posi radius :monster)]
                 (play-sound "explode-erco.wav")
                 (animation-entity
                   :position posi
                   :animation (folder-animation :folder "effects/stomp/" :looping false))
                 (dorun (map #(stun % duration) hits))
                 (dorun (map #(deal-dmg dmg %) hits))))})
