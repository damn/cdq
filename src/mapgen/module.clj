(ns mapgen.module
  (:use data.grid2d
        game.maps.tiledmaps
        [game.tools.tiledmap-grid-convert :only (get-spriteidx)]
        [mapgen.utils :only (border-position?)]))

(defn- print-cell [celltype]
  (print (case celltype
           nil " "
           :ground "_"
           :wall "#"
           :airwalkable ".")))

(defn- print-row [y grid]
  (dorun (map #(print-cell (get grid [% y])) (range (width grid))))
  (println))

(defn printgrid [grid]  ; TODO use :print-cell print-cell
  (dorun (map #(print-row % grid) (range (height grid)))))

(defn- transform-grid-to-print [grid]
  (-> grid
      (dissoc :connections)
      (mapgrid->vectorgrid :blockvalue)
      first
      printgrid))

;;;;;;;;;;;;;;;;;;;;

(defn- get-tiles [[leftx topy] width height]
  (for [x (range leftx (+ leftx width))
        y (range topy (+ topy height))]
    [x y]))

(defn- place-module [grid topleft {:keys [modulegrid tiledmap connections]} & {set-player-start :set-player-start}]
  (let [ground-layer-idx (get-layer-index tiledmap "ground")
        details-layer-idx (get-layer-index tiledmap "details")
        entities-layer-idx (get-layer-index tiledmap "entities")
        grid (reduce (fn [grid module-loc]
                       (assoc grid
                              (mapv + topleft module-loc)
                              {:blockvalue (get modulegrid module-loc)
                               :sprite-idx (get-spriteidx module-loc tiledmap ground-layer-idx )
                               :details-sprite-idx (get-spriteidx module-loc tiledmap details-layer-idx)
                               :monster (when entities-layer-idx
                                          (get-tile-property-value module-loc tiledmap entities-layer-idx "monster"))
                               :chest (when entities-layer-idx
                                          (get-tile-property-value module-loc tiledmap entities-layer-idx "chest"))
                               :teleporter (when entities-layer-idx
                                             (get-tile-property-value module-loc tiledmap entities-layer-idx "teleporter"))}))
                     grid
                     (posis modulegrid))
        ; connect to one of all module connection points:
        ;grid (update-in grid [:connections] concat (map #(mapv + topleft %) connections))

        ; connect to only the last module:
        grid (assoc-in grid [:connections] (map #(mapv + topleft %) connections))
        ]
    (if set-player-start
      (assoc-in grid [(mapv + topleft (get-player-entity-start-position tiledmap)) :player] true)
      grid)))

; TODO could change this so it checks only tiles that are placed and not all in a rectangle
(defn- overlap?
  "Module overlaps with existing grid tiles when some of the tiles of the potential location of the module
   are already defined in the grid. Keeps 1 distance to other modules so we have a wall of width 1 between them ->
  no not allowed diagonals!"
  [grid grid-cp {:keys [modulegrid]} module-cp]
  (some grid (get-tiles (mapv - grid-cp module-cp [1 1])
                        (+ (width modulegrid) 2)
                        (+ (height modulegrid) 2))))

(defn- fits?
  "Checks if the module fits on the grid connection point. If yes returns the module-cp(s) that fit with it."
  [grid grid-cp module]
  (remove #(overlap? grid grid-cp module %) (:connections module)))

(defn- door-sprite-idx [grid [x y] horizontal? door-sprite-indizes]
  (if horizontal?
    (-> grid
      (assoc-in [[(dec x) y] :sprite-idx] (:left (:horizontal door-sprite-indizes)))
      (assoc-in [[(inc x) y] :sprite-idx] (:right (:horizontal door-sprite-indizes))))
    (-> grid
      (assoc-in [[x (dec y)] :sprite-idx] (:up (:vertical door-sprite-indizes)))
      (assoc-in [[x (inc y)] :sprite-idx] (:down (:vertical door-sprite-indizes))))))

(defn- set-connection-point-door [grid [x y] door-sprite-indizes]
  (let [horizontal? (not (get grid [(dec x) y]))]
    (-> grid
      (assoc [x y] {:blockvalue :wall
                    :sprite-idx (:below door-sprite-indizes)
                    :door (if horizontal? :horizontal :vertical)})
      (door-sprite-idx [x y] horizontal? door-sprite-indizes))))

(defn- place-module-at-random-connection [grid module door-sprite-indizes]
  (let [grid-connection-points (shuffle (:connections grid))
        matches (for [grid-cp grid-connection-points
                      :let [module-cps (fits? grid grid-cp module)]
                      :when (seq module-cps)]
                  ; rand-nth because multiple connection points on 1 wall may result in multiple fitting locations
                  [grid-cp (rand-nth module-cps)])
        _ (assert (seq matches) "Can not fit module with other modules.")
        [grid-cp module-cp] (first matches)]
    (-> grid
      (place-module (mapv - grid-cp module-cp) module)
      (set-connection-point-door grid-cp door-sprite-indizes))))

; TODO for performance remove already connected cps (grid-cp) from (:connections grid)
; grid (update-in grid [:connections] #(remove #{grid-cp} %))
; but takes x2 as long ! ..
; can also remove @ place-module :remove-connectionpoint...
; use visualvm for bottleneck anyway...
; test with (Random. seed) & nth coll (srand-int (count ...) random) & (sshuffle) ...

(defn- is-corner-position? [[x y] grid] ; TODO move to grid2d/mapgen.utils ?
  (and (or (= x 0) (= x (dec (width grid))))
       (or (= y 0) (= y (dec (height grid))))))

(defn- read-module [modulefilename]
  (let [tiled-map (construct-tiledmap modulefilename)
        modulegrid (create-grid-from-tiled-map tiled-map)
        width (width modulegrid)
        height (height modulegrid)
        connection-points (map first (get-tileproperties tiled-map "entities" "connection"))
        shifted-connection-points (map (fn [[x y]]
                                         (cond (= x 0)            [-1 y]
                                               (= x (dec width))  [width y]
                                               (= y 0)            [x -1]
                                               (= y (dec height)) [x height]))
                                       connection-points)]
    (assert (every? #(border-position? % modulegrid) connection-points))
    (assert (not-any? #(is-corner-position? % modulegrid) connection-points)
            "Connection points of modules can not be in one of the four corners.")
    {:tiledmap tiled-map
     :modulegrid modulegrid
     :connections shifted-connection-points}))

(defn make-module-based-grid [modulefilenames door-sprite-indizes]
  (let [modules (map read-module modulefilenames)
        grid {}
        grid (place-module grid [0 0] (first modules) :set-player-start true)
        grid (reduce (fn [grid module]
                       (let [grid (place-module-at-random-connection grid module door-sprite-indizes)]
                         ;(transform-grid-to-print grid)
                         ;(println)
                         grid))
                     grid
                     (rest modules))]
    ;(transform-grid-to-print grid)
    ;(println)
    (dissoc grid :connections)))



















; Concept;

; rooms will not have empty borders like before => walls will be added automatically so just make ground tiles

; there can be any number of connection points but they will need to be at the border of the module
; these connection points are then shifted 1 or -1 so they are outside of the room (depending on which side)

(comment
"################
 ########X#######
 ####__________##
 ####__________##
 ####__________##
 ####__________##
 #X#X__________X#
 #_____________##
 #_____________##
 #_____________##
 ####__________##
 ####__________##
 ########XX######
 #######_____####
 #######_____####
 #######_____####
 #######_____####
 #######_____####
 #########X######
 ################")
; the left room has a C.point on the top right corner connected with the left connection point of the middle room
; this leads to both rooms touching so close ...
; we only want 1 cell wall openings where we put a door
; so dont put connection point in one of the four corners









