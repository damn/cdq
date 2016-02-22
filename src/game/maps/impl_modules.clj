
(def ^:private door-sprite-indizes {:below [6 5]
                                    :horizontal {:left [4 0] :right [6 0] :closed [5 0] :open [5 1]}
                                    :vertical {:up [4 2] :down [4 4] :closed [4 3] :open [5 3]}})

(defn- repeatedly-until-no-error [f]
  (let [n (atom 0)
        result (first (remove nil?
                              (repeatedly 100
                                          (fn []
                                            (let [result (try (f) (catch java.lang.AssertionError e nil))]
                                              (log "Build module-based-map ... Try#" (swap! n inc) "..." (if result "Success!" "Fail!"))
                                              result)))))]
    (assert result "Faild to build map!")
    result))

; TODO instead docells when :monster value
; => assoc at grided  {:width :height :data :monsters :player-posi :teleporter-posi}
; => quicker!? ... 10 ms for docells ...
(defnks makemodulebasedmap [:modules :next-map :spritesheet :pretty-name :rand-item-max-lvl]
  (let [mapgrid (repeatedly-until-no-error
                  (fn []
                    (make-module-based-grid (map #(str "maps/" %) modules) door-sprite-indizes)))
        grid2d (first (mapgrid->vectorgrid mapgrid identity))]
    {:pretty-name pretty-name
     :grid (undefined-value-behind-walls (transform grid2d #(or (:blockvalue %2) :wall)))
     :blockprops dungeon-blockprops
     :calc-sprite-posi (let [sprite-idx-grid (transform grid2d #(:sprite-idx %2))]
                         (fn [grid posi value]
                           (if-let [sprite-idx (get sprite-idx-grid posi)]
                             sprite-idx
                             (gauntletly/calc-sprite-posi grid posi value))))
     :sprite-sheet spritesheet
     :details-calc-sprite-posi (let [sprite-idx-grid (transform grid2d #(:details-sprite-idx %2))]
                                 (fn [grid posi value]
                                   (get sprite-idx-grid posi)))
     :details-sprite-sheet details-sprite-sheet
     :start-position (translate-to-tile-middle
                       (first (filter #(:player (get grid2d %)) (posis grid2d))))
     :spawn-monsters (fn []
                       (doseq [[posi value] grid2d]
                         (when-let [value (:monster value)]
                           (try-spawn (translate-to-tile-middle posi) (keyword value) :debug false))))
     :load-content (fn []
                     (doseq [[posi value] grid2d]
                       (when (:teleporter value)
                         (static-teleporter
                           :save-game true
                           :from [@game.maps.data/current-map (translate-to-tile-middle posi)]
                           :to [next-map (:start-position (game.maps.data/get-map-data next-map))])))
                     (doseq [[posi value] grid2d]
                       (when-let [doortype (:door value)]
                         (make-door (translate-to-tile-middle posi)
                                    (get-sprite spritesheet (:closed (doortype door-sprite-indizes)))
                                    (get-sprite spritesheet (:open (doortype door-sprite-indizes))))))
                     (doseq [[posi value] grid2d]
                       (when (:chest value)
                         (create-chest (translate-to-tile-middle posi) :item-name "Cyborg Brain Booster"))))
     :rand-item-max-lvl rand-item-max-lvl}))


;(use 'game.components.ingame-loop)
;(do-in-game-loop
;  (repeatedly-until-no-error
;                  (fn []
;                    (make-module-based-grid (map #(str "maps/" %) (map
;                                 #(str "level1/" %)
;                                 (concat
;                                   ["module1.tmx"]
;                                   (shuffle
;                                     ["module2.tmx"
;                                      "module3.tmx"
;                                      "module4.tmx"
;                                      "module5.tmx"
;                                      "module2.tmx"
;                                      "module8.tmx"
;                                      "module6.tmx"
;                                      "module7.tmx"])
;                                   ["module_end.tmx"]))) door-sprite-indizes))))

(game.maps.add/defgeneratedmap :level-one
  (makemodulebasedmap :modules (map
                                 #(str "modules/level1/" %)
                                 (concat
                                   ["module1.tmx"]
                                   (shuffle
                                     ["module2.tmx"
                                      "module3.tmx"
                                      "module4.tmx"
                                      "module5.tmx"
                                      "module2.tmx"
                                      "module8.tmx"
                                      "module6.tmx"
                                      "module7.tmx"])
                                   ["module_end.tmx"]))
                      :next-map :level-two
                      :spritesheet gauntletly1-tilesheet
                      :pretty-name "Level 1"
                      :rand-item-max-lvl 1))

(game.maps.add/defgeneratedmap :level-two
  (makemodulebasedmap :modules (map #(str "modules/" %)
                                    (concat
                                      ["level2/start.tmx"]
                                      (shuffle
                                        ["level2/flies.tmx"
                                         "level2/slowdowns_chest.tmx"
                                         "level2/corridor.tmx"
                                         "level2/4_entries.tmx"
                                         "level2/shields.tmx"])
                                      ["level1/module_end.tmx"]))
                      :next-map :level-three
                      :spritesheet gauntletly2-tilesheet
                      :pretty-name "Level 2"
                      :rand-item-max-lvl 2))

(game.maps.add/defgeneratedmap :level-three
  (makemodulebasedmap :modules (map #(str "modules/" %) ["level3/boss.tmx"])
                      :next-map :level-three
                      :spritesheet gauntletly3-tilesheet
                      :pretty-name "Level 3"
                      :rand-item-max-lvl 3))


; can reuse modules in different maps even when different spritesheets are used
; because spritesheet layout of walls/walkables/doors is always the same

; line of sight for nova&grenade etc.?  not through walls? teleport super boring cut really ... !


