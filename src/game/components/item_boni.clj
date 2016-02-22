(ns game.components.item-boni
  (:use
    (game.components core body-effects-impl)
    game.utils.random))

; use integers because floating point arithmetic is not precise.
; So when using floats and adding/subtracting some bonis it may be 0.999 instead of 1
(defcomponent item-boni []
  {:min-inc 0
   :max-inc 0
   :percent-modify 100
   :percent-modify-spell 100
   :melee-crit 0
   :spell-crit 0
   :hp-leech 0
   :mana-leech 0
   :chance-stun 0
   :chance-slow 0
   :chance-reduce-armor 0
   :dealt-melee-dmg-effect (fn [target-body]
                             (let [{:keys [chance-stun chance-slow chance-reduce-armor]} (get-component player-body :item-boni)]
                               (when-chance chance-stun
                                 (stun target-body 1000))
                               (when-chance chance-slow
                                 (slowdown target-body 10))
                               (when-chance chance-reduce-armor
                                 (create-armor-reduce-effect target-body 50 10))))})

