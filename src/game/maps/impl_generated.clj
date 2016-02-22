; maps each layer tiles only from one spritesheet! else doesnt work!
; :details-sprite-idx from details-sheet and so on

; rotation destroys order of room tiles when a 2x2 cells form something
(defn- addroom [grid roomname]
  (let [abc (remove false?
                    (map #(is-5x5-walls-entrance? % grid)
                         (posis grid)))
        _ (when-not (seq abc)
            (println "No 5x5-walls-entrance for room found..."))
        [room entrance center rotatefn] (rand-nth abc)
        roomtopleftloc (first room)
        tiled-map (construct-tiledmap (str "maps/" roomname))
        unrotated-roomgrid (create-grid-from-tiled-map tiled-map)
        roomgrid (rotatefn unrotated-roomgrid)
        ;_ (print-grid roomgrid)

        ground-layer-idx (.getLayerIndex tiled-map "ground")

        ; can not access tileIds directly in the next step because tiledmap is not rotated...
        room-spriteidx-grid (rotatefn
                              (transform unrotated-roomgrid
                                         (fn [p value]
                                           (get-spriteidx p tiled-map ground-layer-idx))))
        roomposi-to-spriteidx (atom {})
        grid (reduce (fn [grid {x 0 y 1 :as roomloc}]
                       (let [gridloc (mapv + roomloc roomtopleftloc)]
                         (when-let [sprite-idx (get room-spriteidx-grid roomloc)]
                           (swap! roomposi-to-spriteidx assoc gridloc sprite-idx))
                         (assoc grid gridloc (or (get roomgrid roomloc) :wall))))
                     grid
                     (posis roomgrid))]

    [grid center @roomposi-to-spriteidx]))

; TODO postprocessing for cellular/cave in a function? -> and use everywhere where postprocessing is used?
; there is postprocessing before scaling (NADs, single cells, connect-regions for cellular)
; and there is after scaling (mark undefined cells)
;
(game.maps.add/defgeneratedmap :tech-graveyard
  (let [grid (cellular-automata-gridgen (rand-int-between 35 75) (rand-int-between 35 75)
                                        :fillprob 64
                                        :generations 5
                                        :wall-borders true)
        grid (-> grid connect-regions fix-not-allowed-diagonals)
        ;;
        [grid center-startroom roomposi-to-spriteidx]     (addroom grid "start_room.tmx")
        [grid center-endroom roomposi-to-spriteidx2]      (addroom grid "end_room.tmx")
        [grid center-treasureroom roomposi-to-spriteidx3] (addroom grid "treasure_room.tmx")
        roomposi-to-spriteidx (merge roomposi-to-spriteidx roomposi-to-spriteidx2 roomposi-to-spriteidx3)
        ;;
        grid (undefined-value-behind-walls (transform grid #(or %2 :wall)))
        ;;
        start-posi center-startroom
        {:keys [end stuff-posis]} (get-populated-grid-posis grid start-posi 3)
        stuff-posis (map translate-to-tile-middle stuff-posis)
        start-posi (translate-to-tile-middle start-posi)]
    {:pretty-name "Tech-Graveyard"
     :grid grid
     :blockprops dungeon-blockprops
     :calc-sprite-posi (fn [grid posi value]
                         (if-let [value (get roomposi-to-spriteidx posi)]
                           value
                           (gauntletly/calc-sprite-posi grid posi value)))
     :sprite-sheet gauntletly1-tilesheet
     :start-position start-posi
     :spawn-monsters (fn []
                       (spawn-monsters techgy-groups :maxno 4 :champions false))
     :load-content (fn []
                     (static-teleporter
                       :save-game true
                       :from [:tech-graveyard (translate-to-tile-middle center-endroom)]
                       :to [:tech (:start-position (game.maps.data/get-map-data :tech))])
                     (create-chest (translate-to-tile-middle center-treasureroom) :item-name "Cyborg Brain Booster")
                     (doseq [p stuff-posis]
                       (create-chest p)))
     :rand-item-max-lvl 1}))

(game.maps.add/defgeneratedmap :tech
  (let [grid (cellular-automata-gridgen (rand-int-between 55 100) (rand-int-between 55 100)
                                        :fillprob 64
                                        :generations 5
                                        :wall-borders true)
        grid (-> grid connect-regions fix-not-allowed-diagonals
                 ; fill-single-cells
                 ; (scalegrid 2)
                 )
        [grid center-treasureroom roomposi-to-spriteidx] (addroom grid "treasure_room.tmx")
        grid (undefined-value-behind-walls grid)
        start-posi (first (filter #(= :ground (get grid %)) (shuffle (posis grid)))) ; use rand-nth?
        {:keys [end stuff-posis]} (get-populated-grid-posis grid start-posi 7)
        stuff-posis (map translate-to-tile-middle stuff-posis)
        start-posi (translate-to-tile-middle start-posi)
        end-posi (translate-to-tile-middle end)]
    {:pretty-name "Tech"
     :grid grid
     :blockprops dungeon-blockprops
     :calc-sprite-posi (fn [grid posi value]
                         (if-let [value (get roomposi-to-spriteidx posi)]
                           value
                           (gauntletly/calc-sprite-posi grid posi value)))
     :sprite-sheet gauntletly2-tilesheet
     :start-position start-posi
     :spawn-monsters (fn [] (spawn-monsters tech-groups))
     :load-content (fn []
                     (doseq [p (rest stuff-posis)]
                       (create-chest p))
                     (create-chest (translate-to-tile-middle center-treasureroom) :item-name "Cyborg Brain Booster")
                     (static-teleporter
                       :save-game true
                       :from [:tech end-posi]
                       :to [:blood-caves (:start-position (game.maps.data/get-map-data :blood-caves))]))
     :rand-item-max-lvl 2}))

(game.maps.add/defgeneratedmap :blood-caves
  (let [{:keys [start grid]} (cave-gridgen (Random.) 25 1000 :wide)
        scale 2
        scale-position (fn [p] (mapv #(* scale %) p))
        start-posi (scale-position start)
        grid (-> grid fix-not-allowed-diagonals fill-single-cells (scalegrid scale))
        [grid center-treasureroom roomposi-to-spriteidx] (addroom grid "treasure_room.tmx")
        grid (undefined-value-behind-walls grid)
        {:keys [end stuff-posis]} (get-populated-grid-posis grid start-posi 7)
        stuff-posis (map translate-to-tile-middle stuff-posis)
        translated-start-posi (translate-to-tile-middle start-posi)]
    {:pretty-name "Blood-Caves"
     :grid grid
     :blockprops dungeon-blockprops
     :calc-sprite-posi (fn [grid posi value]
                         (if-let [value (get roomposi-to-spriteidx posi)]
                           value
                           (gauntletly/calc-sprite-posi grid posi value)))
     :sprite-sheet gauntletly3-tilesheet
     :start-position translated-start-posi
     :spawn-monsters (fn [] (spawn-monsters bloodcaves-groups))
     :load-content (fn []
                     (create-chest (first stuff-posis) :item-name "The Golden Banana")
                     (doseq [p (rest (rest stuff-posis))]
                       (create-chest p))
                     (create-chest (translate-to-tile-middle center-treasureroom) :item-name "Cyborg Brain Booster"))
     :rand-item-max-lvl 3}))
