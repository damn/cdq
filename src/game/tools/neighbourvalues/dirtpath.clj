(ns game.tools.dirtpath
  (:use data.grid2d
        utils.core
        engine.core
        game.utils.random
        mapgen.findpath
        game.maps.tiledmaps
        (game.tools transitiontilemaker tiledmap-grid-convert)))

(def- ground-firstgid 1)
(def- circle-tiles-firstgid 91)

(def- neighbourvalues {:topandleft 11
                       :top 9
                       :topandright 10
                       :bottomright 24
                       :bottomleft 25
                       :left 18
                       :right 17
                       :bottom 21
                       :topright 22
                       :topleft 23
                       :leftandbottom 20
                       :rightandbottom 19})

(defn- pathblocked? "check adjacent posis too because pathwide is adjacent tiles"
  [grid posi]
  (or
    (= :wall (get grid posi))
    (some #(= :wall (get grid %)) (get-8-neighbour-positions posi))))

(start-slick-basicgame
  :init (fn [container]
          (let [grid (create-grid-from-tiled-map (construct-tiledmap (resource "tech.tmx")))
                pathposis (find-path [10 10] [5 73] grid pathblocked?)
                is-path-posi? (fn [p] (some #{p} pathposis))
                is-transition-posi? (fn [posi value] (and (= :ground value)
                                                          (not (is-path-posi? posi))
                                                          (some is-path-posi? (get-8-neighbour-positions posi))))
                blocked (+ ground-firstgid 63)

                main-stone-gid (+ circle-tiles-firstgid 16)
                walkable main-stone-gid
                walkableopt (map #(+ circle-tiles-firstgid %) [12 13 14 15])
                dirt-lid 28
                pathgid (+ circle-tiles-firstgid dirt-lid)
                calc-gid (fn [grid posi value]
                           (cond
                             (is-transition-posi? posi value)  (+ circle-tiles-firstgid
                                                                  (get-transition-tile-value
                                                                    posi
                                                                    grid
                                                                    :neighbourvalues neighbourvalues
                                                                    :default-value dirt-lid
                                                                    :count-neighbour? (fn [posi value] (is-path-posi? posi))))
                             (some #{posi} pathposis) pathgid
                             (= :ground value) (if-chance 10 (rand-nth walkableopt) walkable)
                             (= :wall value) blocked))]
            (make-tiledmap-file 48
                                (resource "neu_with_path.tmx")
                                grid
                                calc-gid
                                [ground-firstgid "Ground.tsx"]
                                [circle-tiles-firstgid "circletiles48.tsx"]))
          (System/exit 0)))


