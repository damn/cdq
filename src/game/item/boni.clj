(ns game.item.boni
  (:use
    utils.core
    game.utils.random
    (game.components body-effects-impl movement destructible)
    game.player.speed))

; lvl-stats mit count 3 hier �berall explizit -> mach von variable abh�ngig

(def- defbonus-prefix "bonus-")

(defn get-item-bonus [type]
  (find-prefixed-var
    :namespace 'game.item.boni
    :prefix defbonus-prefix
    :prefixed-type type))

(defmacro ^:private defbonus [k lvl-stats f]
  `(def ~(symbol (str defbonus-prefix (name k)))
     {:create ~f
      :lvl-stats ~lvl-stats}))

(def- item-type-boni
  {"Armor" [:mana :hp :move-speed :cast-speed :spell-crit :perc-dmg-spell]
   "Sword" [:perc-dmg :min-dmg :max-dmg :attack-speed :hp-leech :mana-leech :melee-crit :spell-crit
            :melee-chance-reduce-armor :melee-chance-slow :melee-chance-stun :perc-dmg-spell]
   "Cyber-Implant" [:armor :mana-reg :attack-speed :move-speed :hp-leech :mana-leech :melee-crit :spell-crit]})

;; Armor

(defbonus :mana
  [[15 25] [35 45] [55 65]]
  (fn [adds-mana]
    {:info (str "+" adds-mana " MP")
     :unequip #(update-in % [:skillmanager :mana] inc-or-dec-max - adds-mana)
     :equip #(update-in % [:skillmanager :mana] inc-or-dec-max + adds-mana)}))

(defbonus :hp
  [[15 25] [35 45] [55 65]]
  (fn [adds-hp]
    {:info (str "+" adds-hp " HP")
     :unequip #(update-in % [:destructible :hp] inc-or-dec-max - adds-hp)
     :equip #(update-in % [:destructible :hp] inc-or-dec-max + adds-hp)}))

(defbonus :move-speed
  [[8 20] [25 35] [40 50]]
  (fn [perc-diff]
    (let [modifier (/ perc-diff 100)]
      {:info (str "+" perc-diff "% Movement-Speed")
       :unequip #(incr-move-speed %  (- modifier))
       :equip #(incr-move-speed % modifier)})))

(defbonus :cast-speed
  [[15 25] [35 45] [50 60]]
  (fn [percent]
    (let [diff (/ percent 100)]
      {:info (str "+" percent "% Casting-Speed")
       :unequip #(incr-casting-speed % (- diff))
       :equip   #(incr-casting-speed % diff)})))

;; Implant

(defbonus :armor
  [[5 15] [16 25] [26 35]]
  (fn [percent-consume]
    (let [armor (get-player-armor percent-consume)]
      {:info (str "+" (readable-number armor) " Armor")
       :unequip #(update-in % [:destructible :armor] - armor)
       :equip #(update-in % [:destructible :armor] + armor)})))

; Don't use this bonus, player has no :hp-regen component!
;(defbonus :hp-reg
;  [[5 15] [16 25] [26 35]]
;  (fn [tenth-percentile]
;    (let [adds-reg-rate (/ tenth-percentile 10)]
;      {:info (str "+" (readable-number adds-reg-rate) "% HP-Reg-per-second")
;       :unequip #(update-in % [:hp-regen :reg-per-second] - adds-reg-rate)
;       :equip #(update-in % [:hp-regen :reg-per-second] + adds-reg-rate)})))

(defbonus :mana-reg
  [[10 30] [31 50] [51 75]]
  (fn [tenth-percentile]
    (let [adds-reg-rate (/ tenth-percentile 10)]
      {:info (str "+" (readable-number adds-reg-rate) "% MP-Reg-per-second")
       :unequip #(update-in % [:mana-regen :reg-per-second] - adds-reg-rate)
       :equip #(update-in % [:mana-regen :reg-per-second] + adds-reg-rate)})))

(defbonus :attack-speed
  [[15 25] [35 45] [50 60]]
  (fn [percent]
    (let [diff (/ percent 100)]
      {:info (str "+" percent "% Attack-Speed")
       :unequip #(incr-attack-speed % (- diff))
       :equip   #(incr-attack-speed % diff)})))

;; Weapon

(defbonus :mana-leech
  [[40 60] [61 80] [81 100]]
  (fn [percent]
    {:info (str "+" percent "% Mana-Leech")
     :unequip #(update-in % [:item-boni :mana-leech] - percent)
     :equip #(update-in % [:item-boni :mana-leech] + percent)}))

(defbonus :hp-leech
  [[20 30] [31 40] [41 50]]
  (fn [percent]
    {:info (str "+" percent "% HP-Leech")
     :unequip #(update-in % [:item-boni :hp-leech] - percent)
     :equip #(update-in % [:item-boni :hp-leech] + percent)}))

(defbonus :perc-dmg
  [[15 25] [45 55] [65 75]]
  (fn [percent]
    {:info (str "+" percent "% Damage")
     :unequip #(update-in % [:item-boni :percent-modify] - percent)
     :equip #(update-in % [:item-boni :percent-modify] + percent)}))

(defbonus :perc-dmg-spell
  [[15 25] [45 55] [65 75]]
  (fn [percent]
    {:info (str "+" percent "% Spell-Damage")
     :unequip #(update-in % [:item-boni :percent-modify-spell] - percent)
     :equip #(update-in % [:item-boni :percent-modify-spell] + percent)}))

(defbonus :max-dmg
  [[2 6] [7 11] [12 16]]
  (fn [inc-by]
    {:info (str "+" inc-by " Maximum-Damage")
     :unequip #(update-in % [:item-boni :max-inc] - inc-by)
     :equip #(update-in % [:item-boni :max-inc] + inc-by)}))

(defbonus :min-dmg
  [[1 3] [4 6] [7 9]]
  (fn [inc-by]
    {:info (str "+" inc-by " Minimum-Damage")
     :unequip #(update-in % [:item-boni :min-inc] - inc-by)
     :equip #(update-in % [:item-boni :min-inc] + inc-by)}))

(defbonus :melee-crit
  [[2 6] [7 11] [12 16]]
  (fn [inc-by]
    {:info (str "+" inc-by "% Critical-Hit Chance (Melee)")
     :unequip #(update-in % [:item-boni :melee-crit] - inc-by)
     :equip #(update-in % [:item-boni :melee-crit] + inc-by)}))

(defbonus :spell-crit
  [[2 6] [7 11] [12 16]]
  (fn [inc-by]
    {:info (str "+" inc-by "% Critical-Hit Chance (Spells)")
     :unequip #(update-in % [:item-boni :spell-crit] - inc-by)
     :equip #(update-in % [:item-boni :spell-crit] + inc-by)}))

(defbonus :melee-chance-stun
  [[20 30] [31 40] [41 50]]
  (fn [inc-by]
    {:info (str "+" inc-by "% Chance to Stun on Hit")
     :unequip #(update-in % [:item-boni :chance-stun] - inc-by)
     :equip #(update-in % [:item-boni :chance-stun] + inc-by)}))

(defbonus :melee-chance-slow
  [[20 30] [31 40] [41 50]]
  (fn [inc-by]
    {:info (str "+" inc-by "% Chance to Slow on Hit")
     :unequip #(update-in % [:item-boni :chance-slow] - inc-by)
     :equip #(update-in % [:item-boni :chance-slow] + inc-by)}))

(defbonus :melee-chance-reduce-armor
  [[20 30] [31 40] [41 50]]
  (fn [inc-by]
    {:info (str "+" inc-by "% Chance to Reduce Armor on Hit")
     :unequip #(update-in % [:item-boni :chance-reduce-armor] - inc-by)
     :equip #(update-in % [:item-boni :chance-reduce-armor] + inc-by)}))

;;

(defn create-item-bonus [lvl bonus-type & {value :value}]  ; TODO assert alles richtigem lvl ?
  (let [{:keys [create lvl-stats]} (get-item-bonus bonus-type)
        value (or value (rand-int-between (nth lvl-stats lvl)))]
    (merge (create value)
      {:type bonus-type :lvl lvl :value value})))

(defn create-random-boni [itemname cnt & {max-level :max-level :or {max-level 3}}] ; TODO 3 hardcoded
  (map
    #(create-item-bonus (rand-int max-level) %)
    (take cnt (shuffle (get item-type-boni itemname)))))

(defn create-equip-boni-save [equip-boni]
  (map #(select-keys % [:lvl :type :value]) equip-boni))

(defn load-equip-boni-save [data]
  (map #(create-item-bonus (:lvl %) (:type %) :value (:value %)) data))










