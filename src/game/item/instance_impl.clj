(ns game.item.instance-impl
  (:require
    [game.maps.data :refer [current-map]]
    [game.components.movement :refer [projectile-movement-component]]
    [game.components.body-effects-impl :refer [battle-drugs dmg-effect]]
    [game.components.core :refer [get-id player-body]]
    [game.components.skills.melee :refer [melee-weapon]]
    [utils.numbers :refer [increase-min-max-val lower-than-max? readable-number rest-to-max round-n-decimals variance-val-str]]
    [engine.core :refer [create-sound defpreload]]
    [engine.render :refer [create-animation create-image folder-frames get-duration]]
    [game.media :refer [get-itemsprite]]
    game.mouseoverbody
    [game.entity.nova :refer [nova-effect]]
    [game.entity.projectile :refer [fire-projectile]]
    game.utils.geom
    [game.utils.random :refer [get-rand-weighted-item rand-int-between]]
    [game.utils.msg-to-player :refer [show-msg-to-player]]
    [game.components.render :refer [show-gains-hp-effect show-gains-mana-effect]]
    [game.components.destructible :refer [get-hp get-player-armor]]
    [game.components.skills.core :refer [get-mana get-skill-use-mouse-tile-pos skillmanager-skill]]
    [game.components.skills.utils :refer [check-line-of-sight get-player-ranged-vector]]
    [data.grid2d :refer [cells]]
    [game.item.cells :refer [remove-one-item-from try-pickup-item]]
    [game.item.grids :refer [item-grids]]
    [game.item.colors :refer [equip-boni-item-color]]
    [game.item.in-hand :refer [item-in-hand]]
    [game.item.boni :refer [create-item-bonus create-random-boni]]
    [game.item.instance :refer [create-item-body create-item-instance def-usable-item defitem]]
    [game.player.skill.skillmanager :refer [add-player-skill change-standard-attack-skill remove-player-skill reset-standard-attack-skill set-selected-skill]]
    [game.player.skill.learnable :refer [learn-skill learnable-skills]]))

;; Random Items

(let [item-names {"Cyber-Implant" 1, "Sword" 1, "Armor" 1}
      boni-counts {1 80, 2 15, 3 5}]

  (defn create-rand-item [position & {:keys [max-lvl]}]
    (create-item-body position
                      (let [itemname (get-rand-weighted-item item-names)]
                        (assoc (create-item-instance itemname {:lvl (rand-int max-lvl)})
                               :equip-boni (create-random-boni itemname (get-rand-weighted-item boni-counts) :max-level max-lvl)
                               :color equip-boni-item-color)))))

(defpreload ^:private grenade-frames (folder-frames "effects/grenade/"))

; TODO auch defusable aber net damit gemacht -> vlt mit split-kvs-and-more und unten equip/unequip zuf�hren?
(defitem "Grenade" [{cnt :count}]
  (let [dmg [10 10]
        image (get-itemsprite [1 1])
        contains-skill? (fn [components skill]
                          (contains? (-> components :skillmanager :skills) (:type skill)))
        skill (skillmanager-skill
                :stype :grenade
                :cooldown 0
                :cost 0
                {:shoot-sound "bfxr_grenade.wav"
                 :animation :casting
                 :mousebutton :right
                 :icon image
                 :radius 2.5
                 :item-name this-name
                 :dmg dmg
                 :dmg-info (fn [skill] dmg)
                 :check-usable check-line-of-sight
                 :do-skill (fn [_ {:keys [item-name radius dmg] :as component}]
                             (remove-one-item-from :belt item-name)
                             (nova-effect
                               :position (get-skill-use-mouse-tile-pos)
                               :duration 300
                               :maxradius radius
                               :affects-side :monster
                               :dmg dmg
                               :animation (create-animation grenade-frames)))})]
    {:type :usable
     :skill skill
     :effect (fn [] (set-selected-skill :right skill) false)
     :use-sound "bfxr_click.wav"
     :unequip (fn [components]
                {:pre [(contains-skill? components skill)]}
                (if
                  (not-any?
                    #{this-name}
                    (map #(:name @(:item %)) (cells (:belt item-grids))))
                  (update-in components [:skillmanager :skills] remove-player-skill skill)
                  components))
     :equip (fn [components]
              (if-not (contains-skill? components skill)
                (update-in components [:skillmanager :skills] add-player-skill skill)
                components))
     :info (str "Put into belt to use.\nThrows a grenade. Deals " dmg " Area of Effect Dmg")
     :image image
     :count (or cnt (rand-int-between 2 4))}))

(let [seconds 30]
  (def-usable-item "Battle-Drugs"
    :effect (fn [] (battle-drugs seconds))
    :use-sound "bfxr_drugsuse.wav"
    :info (str "Gives +50% casting,attack and movement speed for " seconds " seconds.")
    :image (get-itemsprite [1 6])))

(let [adds-hp 40]
  (def-usable-item "Heal-Potion"
    :effect #(when (lower-than-max? (get-hp player-body))
               (show-gains-hp-effect player-body (min adds-hp (rest-to-max (get-hp player-body))))
               (swap! player-body update-in [:destructible :hp] increase-min-max-val adds-hp))
    :use-sound "bfxr_potionuse.wav"
    :info (str "Gives " adds-hp " hitpoints")
    :image (get-itemsprite [0 3])))

(let [adds-hp 80] ; TODO ... same code as above...
  (def-usable-item "Big-Heal-Potion"
    :effect #(when (lower-than-max? (get-hp player-body))
               (show-gains-hp-effect player-body (min adds-hp (rest-to-max (get-hp player-body))))
               (swap! player-body update-in [:destructible :hp] increase-min-max-val adds-hp))
    :use-sound "bfxr_potionuse.wav"
    :info (str "Gives " adds-hp " hitpoints")
    :image (get-itemsprite [0 0])))

(let [adds-mana 20]
  (def-usable-item "Mana-Potion"
    :effect #(when (lower-than-max? (get-mana player-body))
               (show-gains-mana-effect player-body (min adds-mana (rest-to-max (get-mana player-body))))
               (swap! player-body update-in [:skillmanager :mana] increase-min-max-val adds-mana))
    :use-sound "bfxr_potionuse.wav"
    :info (str "Gives " adds-mana " mana")
    :image (get-itemsprite [0 4])))

(let [adds-mana 40] ; TODO ... same code as above...; also @ meditation skill
  (def-usable-item "Big-Mana-Potion"
    :effect #(when (lower-than-max? (get-mana player-body))
               (show-gains-mana-effect player-body (min adds-mana (rest-to-max (get-mana player-body))))
               (swap! player-body update-in [:skillmanager :mana] increase-min-max-val adds-mana))
    :use-sound "bfxr_potionuse.wav"
    :info (str "Gives " adds-mana " mana")
    :image (get-itemsprite [0 1])))

(def-usable-item "Cyborg Brain Booster"
  :effect (fn [] (swap! player-body update-in [:skillmanager :free-skill-points] inc))
  :use-sound "fanfare10.wav"
  :info ""
  :image (get-itemsprite [1 0]))

(def-usable-item "The Golden Banana"
  :effect (fn [] (show-msg-to-player "THE END"))
  :use-sound "fanfare10.wav"
  :info ""
  :image (get-itemsprite [5 5]))

(defn- in-seconds "in float just for presentation not correct calculation."
  [ms]
  (float (/ ms 1000)))

(defn- get-gun-infostr [weapon]
  (str
    (:dmg weapon) " Damage\n"
    (in-seconds (:maxcnt (:cooldown-counter weapon))) "s cooldown"))

; lang nicht mehr benutzt; m�glicherweise funzt nicht mehr, nicht getestet.
(defitem "Gun" [_]
  (let [image (create-image "items/gun.png")
        dmg [10 10]
        cooldown 1000
        skill (skillmanager-skill
                :stype :gun
                :cooldown cooldown
                :cost 0
                {:sound (create-sound "shoot.wav")
                 :animation :shooting
                 :mousebutton :both
                 :icon image
                 :dmg dmg
                 :do-skill (fn [entity component]
                             (fire-projectile
                               :startbody entity
                               :px-size 5
                               :side :player
                               :hits-side :monster
                               :movement (projectile-movement-component (get-player-ranged-vector) 480)
                               :hit-effects [(dmg-effect (:dmg component))]))})]
    {:skill skill
     :unequip #(update-in % [:skillmanager] reset-standard-attack-skill)
     :equip #(update-in % [:skillmanager] change-standard-attack-skill skill)
     :info (get-gun-infostr skill)
     :type :hands
     :image image}))

(defn- get-dps [[mi mx] seconds]
  (round-n-decimals (/ (+ mi mx) 2 seconds) 2))

(defn- get-sword-infostr [{:keys [animation base-dmg] :as weapon}]
  (let [seconds (-> (:animation @player-body) animation get-duration in-seconds)]
    (str
      (variance-val-str base-dmg) " Damage\n"
      (case animation :sword-small "Fast" :sword "Normal" :sword-big "Slow") " Attack-Speed\n"
      (str "DPS: " (get-dps base-dmg seconds)))))

(let [base-dmg-per-lvl [[[3 5] [5 7]]
                        [[8 10] [11 18]]
                        [[16 26] [35 50]]]
      calc-base-dmg #(let [[mi mx] (base-dmg-per-lvl %)] [(rand-int-between mi) (rand-int-between mx)])
      sprite-idx-per-lvl [[[2 2]]
                          [[2 0] [2 6]]
                          [[3 0] [3 5]]]]
  (defitem "Sword" [{:keys [lvl sprite-idx base-dmg] :or {lvl 0}}]
    (let [sprite-idx (or sprite-idx (rand-nth (sprite-idx-per-lvl lvl)))
          base-dmg (or base-dmg (calc-base-dmg lvl))
          melee-weapon (melee-weapon base-dmg (create-sound "slash.wav") (case lvl 0 :sword-small 1 :sword 2 :sword-big))]
      {:pretty-name (case lvl 0 "Light Sword" 1 "Normal Sword" 2 "Heavy Sword")
       :lvl lvl
       :base-dmg base-dmg
       :melee-weapon melee-weapon
       :info (get-sword-infostr melee-weapon)
       :type :hands
       :image (get-itemsprite sprite-idx)
       :sprite-idx sprite-idx})))

(let [armor-per-lvl [[10 30] [31 50] [51 75]]
      calc-base-armor #(get-player-armor (rand-int-between (armor-per-lvl %)))
      sprite-idx-per-lvl [[[6 3] [6 4]]
                          [[6 1] [6 2]]
                          [[6 0]]]]

  (defitem "Armor" [{:keys [lvl sprite-idx base-armor] :or {lvl 0}}]
    (let [sprite-idx (or sprite-idx (rand-nth (sprite-idx-per-lvl lvl)))
          base-armor (or base-armor (calc-base-armor lvl))]
      {:pretty-name (case lvl 0 "Light Armor" 1 "Normal Armor" 2 "Heavy Armor")
       :lvl lvl
       :base-armor base-armor
       :unequip #(update-in % [:destructible :armor] - base-armor)
       :equip #(update-in % [:destructible :armor] + base-armor)
       :info (str (readable-number base-armor)  " Base-Armor")
       :type :torso
       :image (get-itemsprite sprite-idx)
       :sprite-idx sprite-idx})))

(defitem "Cyber-Implant" [{:keys [sprite-idx] :or {sprite-idx [4 (rand-int 5)]}}]
  {:type :implant
   :image (get-itemsprite sprite-idx)
   :sprite-idx sprite-idx})

(defitem "Restoration" [_]
  {:image (get-itemsprite [4 6])
   :info "If you die this item will be used to\n revive you one time and teleport you to the start of the level."})

;;
;; DEBUG/CHEAT items

(defn- pickup-cheat-items "does not work anymore" []
    (try-pickup-item
      (create-item-instance
        "Sword"
        {:equip-boni [(create-item-bonus 2 :mana-leech)
                      (create-item-bonus 2 :melee-chance-stun)
                      (create-item-bonus 2 :hp-reg)
                      (create-item-bonus 2 :mana-reg)
                      (create-item-bonus 2 :perc-dmg)
                      (create-item-bonus 2 :min-dmg)
                      (create-item-bonus 2 :max-dmg)
                      (create-item-bonus 2 :attack-speed)
                      (create-item-bonus 2 :move-speed)]}))
    (try-pickup-item
      (create-item-instance
        "Armor"
        {:equip-boni [(create-item-bonus 2 :mana-reg)
                      (create-item-bonus 2 :hp-reg)
                      (create-item-bonus 2 :armor)
                      (create-item-bonus 2 :cast-speed)
                      (create-item-bonus 2 :move-speed)]}))
    (try-pickup-item
      (create-item-instance
        "Cyber-Implant"
        {:equip-boni [(create-item-bonus 2 :mana-reg)
                      (create-item-bonus 2 :hp-reg)
                      (create-item-bonus 2 :armor)
                      (create-item-bonus 2 :cast-speed)
                      (create-item-bonus 2 :move-speed)]})))


(comment
  (do
    (learn-skill (:meditation learnable-skills))
    (learn-skill (:player-ranged learnable-skills))
    (learn-skill (:nova learnable-skills))
    (learn-skill (:teleport learnable-skills))
    nil))

(comment


  (require 'clojure.pprint)
  (swap! item-in-hand assoc :equip-boni
    [(create-item-bonus 2 :move-speed)
     (create-item-bonus 2 :mana-leech)
     (create-item-bonus 2 :hp-leech)
     (create-item-bonus 2 :mana-reg)
     (create-item-bonus 2 :cast-speed)
     (create-item-bonus 2 :perc-dmg-spell)
     (create-item-bonus 2 :spell-crit)
     (create-item-bonus 2 :armor)])

  (try-pickup-item
    (assoc (create-item-instance "Sword")
      :equip-boni [(create-item-bonus 2 :melee-chance-slow)]))

  (dotimes [_ 10]
    (try-pickup-item
      (create-item-instance "Grenade")))

  (require '[game.components.ingame-loop :refer [do-in-game-loop]])
  (do-in-game-loop
    (try-pickup-item
      (create-item-instance "Battle-Drugs")))

  (pickup-cheat-items)

  (create-rand-item (get-position player-body) :max-lvl 3)

  (try-pickup-item
    (create-item-instance "Gun"
      {:equip-boni [(create-item-bonus 0 :armor)]}))
)
