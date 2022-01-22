(ns game.monster.spawn
  (:use data.grid2d
        utils.core
        engine.render
        (game.monster defmonster)
        (game.components core body render)
        game.maps.data
        [game.item.instance :only (create-item-body)]
        (game.utils random random)))

(defn try-spawn [posi monster-type & {debug :debug}]
  {:pre [(get-monster-properties monster-type)]}
  (let [{:keys [half-w half-h movement-type create]} (get-monster-properties monster-type)
        blocked (blocked-location? posi half-w half-h movement-type)
        spawned (when-not blocked (create posi))]
    (when debug
      (println (str "try-spawn " monster-type " at " @game.maps.data/current-map " " posi " spawned? " (boolean spawned))))
    spawned))

(comment

  (use 'game.components.ingame-loop)
  (do-in-game-loop
      (try-spawn [12.5 5.5] :first-boss))
  )



(def test-group {:test-hunter 1})

(def drones {:xploding-drone 1})
(def bloodcaves3 {:xploding-drone 1
                  :slowdown-caster 1
                  :nova-melee 6
                  :armored-skull2 10})
(def bloodcaves2 {:xploding-drone 1
                  :slowdown-caster 1
                  :armored-skull 12})
(def bloodcaves1 {:xploding-drone 1
                  :slowdown-caster 1
                  :healer 1
                  :mage-skull 5})
(def bloodcaves-groups [{:healer 1
                         :big-teleporting-melee 10
                         :xploding-drone 10
                         :ranged 1}
                        {:burrower 10
                         :ray-shooter 1}
                        {:armored-skull 5
                         :mage-skull 5
                         :healer 1}])

(def flies {:nova-melee 1 :fly 9 :mine 1})
(def healer-group {:nova-melee 5
                   :mine 1
                   :slowdown-caster 1
                   :healer 1
                   :skull-chainsaw 10})
(def big-skulls {:nova-melee 5
                 :mine 1
                 :slowdown-caster 4
                 :big-skull-chainsaw 3})
(def shield-turrets {:nova-melee 2
                     :shield-turret 14
                     :healer 1})
(def tech-groups [flies healer-group big-skulls shield-turrets])

(def techgy1 {:skull-chainsaw 4 :littlespider 2 :armored-skull2 1})
(def techgy2 {:instant-healer 1 :littlespider 6 :ranged 4})
(def techgy3 {:nova-melee 1 :ray-shooter 4})
(def techgy-groups [techgy1 techgy2 techgy3])

; function is for monsters with width&height <1 tile
(defn spawn-group [group posis & {maxno :maxno :or {maxno 9}}]
  (let [posis (map translate-to-tile-middle (take maxno posis))
        ;; dont spawn monsters near the start locations of the levels ... so a calm start... start-positions need to be translated to tile middle !!
        start-position (:start-position (get-current-map-data))
        posis (remove #(< (game.utils.geom/get-distance % start-position)
                          game.components.sleeping/start-position-safezone-radius)
                      posis)
        monstertypes (get-rand-weighted-items (count posis) group)]
    (comment (log "#posis " (count posis)
                  " group: " group
                  " frequencies: " (frequencies monstertypes)))
    (doall ; returning it
      (remove nil?
              (map #(try-spawn %1 %2) posis monstertypes)))))

(use '[mapgen.spawn-spaces :only (get-spawn-positions-groups)])
(use '[game.maps.cell-grid :only (get-cell-grid)])

; + verteilt etwas am boden (schau ob net auf blocked gegend?!)
(let [dropweights {"Mana-Potion" 7
                   "Heal-Potion" 7
                   "Big-Mana-Potion" 3
                   "Big-Heal-Potion" 3}]
  (defn- champion-drop [body]
    (let [p (get-position body)]
      (doseq [itemname (get-rand-weighted-items (rand-int-between 2 4) dropweights)]
        (create-item-body p itemname)))))

(defn championize-monster [monster]
  (update-in! monster [:destructible :hp] inc-or-dec-max * 5) ; same code @ item-boni
  (add-component monster
                 (create-comp :extra-loot {:destruct champion-drop}))
  (add-component monster
                 (assoc (single-animation-component (folder-animation :folder "effects/champion/" :looping true)
                                             :order :on-ground)
                        :type
                        :championized-animation))) ; not two :animation types allowed in one entity!

(defn spawn-monsters [group-definitions & {champions :champions :or {champions true} :as more}]
  (doseq [posis (get-spawn-positions-groups (get-cell-grid))]
    (let [monsters (apply spawn-group (rand-nth group-definitions) posis (apply concat more))]  ; concat so the map is a sequence of its elements
      (when champions
        (when-chance 5
                     ;(log "monsters at posi " (first posis) "championized" " in map " @game.maps.data/current-map)
                     ;(runmap championize-monster monsters)
                     ; deactivated because problem with 'current-animation' in game.render and because
                     ; sleeping monsters with blocks fails because new components added dont have blocks
                     )))))



