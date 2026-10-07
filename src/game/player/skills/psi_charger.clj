(ns game.player.skills.psi-charger
  (:require
    [game.components.core :refer [player-body]]
    [game.components.body-effects-impl :refer [add-psi-charge max-psi-charges]]
    [game.components.skills.melee :refer [player-melee-props]]
    [game.player.skill.learnable :refer [deflearnable-skill]]))

(defn- psi-charge-hit-effect [_]
  (let [{:keys [attack-bonus movement-bonus]} (:psi-charger game.player.skill.learnable/learnable-skills)]
    (add-psi-charge player-body attack-bonus movement-bonus)))

(deflearnable-skill psi-charger
  :manacost 12
  :menu-posi [1 0]
  :mousebutton :both
  :icon "icons/psi_charger.png"
  :info
  "Melee Attack where every successful hit
gives you a PSI-Charge. PSI-Charges give
you a passive bonus and can be used with
finisher-skills.

1 Charge  - +20% Movement-Speed
            +13% Attack-Speed
2 Charges - +40% Movement-Speed
            +27% Attack-Speed
3 Charges - +60% Movement-Speed
            +40% Attack-Speed"
  {:attack-bonus (/ 0.4 max-psi-charges)
   :movement-bonus (/ 0.6 max-psi-charges)
   :show-info-for [:cost]}
  (player-melee-props [psi-charge-hit-effect]))
