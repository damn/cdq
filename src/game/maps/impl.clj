(ns game.maps.impl
  (:require
    [cdq.core :refer [green magenta]]
    game.maps.add
    cdq.core
    [cdq.core :as grid :refer [get-4-neighbour-positions mapgrid->vectorgrid posis transform height width]]
    [cdq.core :refer [log translate-to-tile-middle]]
    [cdq.core :refer [play-sound]]
    [cdq.core :refer [get-sprite spritesheet]]
    cdq.core
    game.media
    [game.media :refer [get-itemsprite]]
    [cdq.core :refer [add-to-removelist create-entity]]
    [cdq.core :refer [position-component]]
    [game.components.body :refer [create-body]]
    [game.components.pressable :refer [pressable-component]]
    [game.components.render :refer [image-render-component]]
    [game.entity.teleporters :refer [static-teleporter]]
    game.player.core
    game.player.skill.selection-list
    [game.monster.spawn :refer [bloodcaves-groups spawn-monsters tech-groups techgy-groups try-spawn]]
    cdq.core
    [cdq.core :refer [cell-blocks-changed-update-listeners change-cell-blocks get-cell]]
    [cdq.core :refer [construct-tiledmap create-grid-from-tiled-map get-layer-index get-player-entity-start-position get-tile-property-value get-tileproperties]]
    [cdq.core :refer [show-on-minimap]]
    game.item.instance
    [game.item.instance :refer [create-item-body]]
    [game.item.instance-impl :refer [create-rand-item]]
    [cdq.core :refer [get-current-map-data]]
    [cdq.core :refer [rand-int-between]]
    game.utils.lightning
    [game.tools.tiledmap-grid-convert :refer [convert-to-spriteposi get-spriteidx]]
    [cdq.core :refer [border-position? fill-single-cells scalegrid undefined-value-behind-walls wall-at?]]
    [cdq.core :refer [cave-gridgen]]
    [cdq.core :refer [get-populated-grid-posis]]
    [cdq.core :refer [fix-not-allowed-diagonals]]
    [cdq.core :refer [cellular-automata-gridgen connect-regions]]
    [cdq.core :refer [is-5x5-walls-entrance?]])

  (:import java.util.Random))

;;; entity.chest (inlined)

(defn- create-chest* [position item-name]
  (create-entity
    (position-component position)
    (create-body :solid true
                 :dimensions [16 16]
                 :mouseover-outline true)
    ;(game.utils.lightning/light-component :intensity 0.7 :radius 2)
    (pressable-component ""
                         (fn [this-body]
                           (play-sound "bfxr_chestopen.wav")
                           (add-to-removelist this-body)
                           (if item-name
                             (create-item-body position item-name)
                             (create-rand-item position :max-lvl (:rand-item-max-lvl (get-current-map-data))))))
    (show-on-minimap magenta)
    {:type :always-in-sight}
    (image-render-component (get-itemsprite [1 4]))))

(defn create-chest [position & {item-name :item-name}]
  (create-chest* position item-name))

;;; entity.door (inlined)

; another way of handling the open door was to change the image&mouseover-outline to false&remove component :pressable
; but it had still a :body component which blocks mouseoverentities that lie below it because there is always only 1 mouseoverbody at each position
; ... or remove the :body component too?
(defn- make-open-door [p image]
  (create-entity
    (position-component p)
    {:type :always-in-sight}
    (image-render-component image :order :air)))

(defn make-door [p closed-image open-image] ; make-closed-door ?
  (create-entity
    (position-component p)
    (create-body :solid false
                 :dimensions [16 16]
                 :mouseover-outline true)
    {:type :always-in-sight}
    (show-on-minimap green)
    (image-render-component closed-image :order :air)
    {:type :clicked :is? false}
    (pressable-component ""
                         (fn [entity]
                           (when-not (:is? (:clicked @entity))
                             (swap! entity assoc-in [:clicked :is?] true)
                             (play-sound "ReversyH-Nick_Ros-105.wav")
                             (add-to-removelist entity)
                             (make-open-door p open-image)
                             (change-cell-blocks (get-cell p) #{})
                             (cell-blocks-changed-update-listeners))))))



;;; tools.transitiontilemaker16 (inlined; was :as gauntletly)

(let [idxvalues-order [[1 0] [-1 0] [0 1] [0 -1]]]
  (assert (= (get-4-neighbour-positions [0 0]) idxvalues-order)))

(comment
  ; Values for every neighbour:
  {          [0 -1] 1
   [-1 0]  8          [1 0] 2
             [0 1] 4 })

; so the idxvalues-order corresponds to the following values for a neighbour tile:
(def ^:private idxvalues [2 8 4 1])

(defn- get-transition-tile-idxvalue [count-neighbour? grid posi]
  (apply +
         (map-indexed (fn [idx p] (if (count-neighbour? p (get grid p))
                                    (get idxvalues idx)
                                    0))
                      (get-4-neighbour-positions posi))))

; => gives value between 0 and 15
; => sprite-idx for every value is defined in a map
; => count :walls

(def ^:private idxvalue-localids
  {0 0,
   1 22,
   2 7,
   3 23,
   4 1,
   5 15,
   6 16,
   7 21,
   8 10,
   9 24,
   10 9,
   11 2,
   12 17,
   13 14,
   14 3,
   15 8})

(defn- calc-localid [grid posi value]
  (case value
    :undefined nil
    :ground 43
    :wall (let [idxvalue (get-transition-tile-idxvalue (fn [posi value] (= :wall value)) grid posi)]
            (assert ((set (keys idxvalue-localids)) idxvalue))
            (get idxvalue-localids idxvalue))))

(let [spritesheet-width 7]

  (defn calc-sprite-posi [grid posi value]
    (when-let [localid (calc-localid grid posi value)]
      (convert-to-spriteposi localid spritesheet-width))))

;;

(def ^:private firstgid 1)

(defn- calc-gid [grid posi value]
  (when-let [localid (calc-localid grid posi value)]
    (+ firstgid localid)))

;(use '(mapgen nad cave utils cellular))
;(start-slick-basicgame
;  :init (fn [container]
;          (doseq [f cdq.core/init-fns]
;            (f))
;          (let [grid (cellular-automata-gridgen 100
;                                                100
;                                                :fillprob 64
;                                                :random (java.util.Random.)
;                                                :generations 5
;                                                :wall-borders true)
;                grid (-> grid connect-regions fix-not-allowed-diagonals undefined-value-behind-walls)]
;            (make-tiledmap-file 16
;                                (resource "gauntletly.tmx")
;                                grid
;                                calc-gid
;                                [firstgid "gauntletly.tsx"]))
;          (println "SUCCESS!")
;          (System/exit 0)))


;;; mapgen.module (inlined)

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

(defn- is-corner-position? [[x y] grid] ; TODO move to grid2d/cdq.core ?
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










;;; maps.impl

; created&loaded in the order defined here
; map with player is first (items need player dependency and maybe more dependencies; also start the game in that map!)
; -> assert?

(def ^:private dungeon-blockprops
  {:undefined nil
   :airwalkable #{:ground}
   :wall #{:ground :air}
   :ground #{}})

(def gauntletly1-tilesheet (spritesheet "maps/gauntletly1.png" 16 16))
(def gauntletly2-tilesheet (spritesheet "maps/gauntletly2.png" 16 16))
(def gauntletly3-tilesheet (spritesheet "maps/gauntletly3.png" 16 16))
(def details-sprite-sheet  (spritesheet "maps/details.png"     16 16))

;(load "impl_generated")
(load "impl_modules")
