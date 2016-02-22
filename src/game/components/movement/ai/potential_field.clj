(ns game.components.movement.ai.potential-field
  "Assumption: The map contains no not-allowed diagonal cells, diagonal wall cells where both
  adjacent cells are walls and blocked.
  (important for wavefront-expansion and field-following)"
  (:require [engine.render :as color])
  (:use data.grid2d
        utils.core
        engine.render
        game.settings
        (game.components core body
                         [position :only (get-tile)]
                         [movement :only (movement-component)])
        game.maps.cell-grid
        game.utils.geom))

;; Standart Potential Field algorithm (brute force)

(defn- potential-field
  "returns the marked cells"
  [size entity]
  (let [start-cell (get-cell (get-tile entity))
        start-posi (:posi @start-cell)]
    (for [cell (cells (get-cell-grid))
          :let [posi (:posi @cell)
                dist (get-distance start-posi posi)]
          :when (and
                  (not (cell-blocked? cell :ground))
                  (<= dist size))]
      (do
        (swap! cell assoc :dist-to-player dist)
        cell))))

(comment
  (clear-cells player-body)

  (def marked-cells (doall (potential-field 7 player-body)))

  (doseq [cell marked-cells]
    (swap! cell dissoc :dist-to-player)))

;;

(def- max-iterations 15)

(def- sqrt-of-2 14)

(defn- get-dist [cell] (:dist-to-player @cell))

(defn- wavefront-expansion
  "returns the new marked-cells"
  [start-cell]
  (swap! start-cell assoc :dist-to-player 0) ; first step
  (loop [last-marked-cells [start-cell]
         marked-cells [start-cell]
         iterations 0]
    (if (= iterations max-iterations)
      marked-cells
      (let [new-marked-cells (atom [])]
        ; sorting important because of diagonal-cell values, flow from lower dist first for correct distance
        (doseq [marked (sort-by get-dist last-marked-cells)
                :let [value (get-dist marked)]]
          (doseq [cell (cached-get-adjacent-cells marked)
                  :when (not (or
                               (cell-blocked? cell :ground) ; filters nil cells
                               (get-dist cell)))]
            (swap! cell assoc :dist-to-player
                   (+ value (if (is-diagonal? marked cell) sqrt-of-2 10)))
            (swap! new-marked-cells conj cell)))
        (recur
          @new-marked-cells
          (concat marked-cells @new-marked-cells)
          (inc iterations))))))

(defn- clear-cells [entity]
  (when-let [cells (:marked-cells (get-component entity :potential-field))]
    (assoc-in! entity [:potential-field :marked-cells] nil)
    (runmap #(swap! % dissoc :dist-to-player) cells)))

(defn- try-generate [entity]
  (let [component (get-component entity :potential-field)
        current-tile (get-tile entity)
        dirty (not= current-tile (:last-tile component))]
    (when dirty
      (assoc-in! entity [:potential-field :last-tile] current-tile)
      (clear-cells entity)
      (assoc-in! entity [:potential-field :marked-cells]
        (wavefront-expansion (get-cell current-tile))))))

(defcomponent potential-field []
  {:depends [:position]
   :posi-changed try-generate
   :init (fn [entity]
           (try-generate entity)
           (add-cell-blocks-changed-listener
             (fn []
               (assoc-in! entity [:potential-field :last-tile] nil)
               (try-generate entity))))})

(comment
  (dotimes [_ 3]
    (time
      (dotimes [_ 10]
        (assoc-in! player-body [:potential-field :last-tile] nil)
        (try-generate player-body)
        nil))))

;; MOVEMENT AI

(let [order (get-8-neighbour-positions [0 0])]
  (def ^:private diagonal-check-indizes
    (into {} (for [[x y] (filter diagonal-direction? order)]
               [(first (positions #(= % [x y]) order))
                (vec (positions #(some #{%} [[x 0] [0 y]])
                                order))]))))

(defn print-cells [cells] ; for debug
  (doseq [cell cells]
    (println (:posi @cell) " dist:" (get-dist cell))))

(defn- is-not-allowed-diagonal? [at-idx adjacent-cells]
  (when-let [[a b] (get diagonal-check-indizes at-idx)]
    (and (nil? (adjacent-cells a))
         (nil? (adjacent-cells b)))))

(defn- remove-not-allowed-diagonals [adjacent-cells]
  (remove nil?
          (map-indexed
            (fn [idx cell]
              (when-not (or (nil? cell)
                            (is-not-allowed-diagonal? idx adjacent-cells))
                cell))
            adjacent-cells)))

(def- movement-type :ground)

; not using filter because nil cells considered @ remove-not-allowed-diagonals
(defn- filter-viable-cells [body adjacent-cells]
  (remove-not-allowed-diagonals
    (mapv #(when-not (or (cell-blocked? % movement-type)
                         (occupied-by-other? % body))
             %)
          adjacent-cells)))

(defn- get-min-dist-cell [cells]
  (when-seq [cells (filter get-dist cells)]
    (apply min-key get-dist cells)))

; rarely called -> no performance bottleneck
(defn- viable-cell? [own-dist body cell]
  (when-let [best-cell (get-min-dist-cell
                         (filter-viable-cells body (cached-get-adjacent-cells cell)))]
    (when (< (get-dist best-cell) own-dist)
      cell)))

(defn- find-next-cell
  "returns :near-player-cell; a target cell or nil."
  [body own-cell]
  (let [own-dist (get-dist own-cell)
        adjacent-cells (cached-get-adjacent-cells own-cell)]
    (if
      (or
        (and own-dist (zero? own-dist)) ; already on player cell
        (some #(when-let [dist (get-dist %)] (zero? dist)) adjacent-cells))  ; next to player cell
      :near-player-cell

      (let [cells (filter-viable-cells body adjacent-cells)
            min-key-cell (get-min-dist-cell cells)]
        (cond
          (not min-key-cell)  ; red
          own-cell

          (not own-dist)
          min-key-cell

          (> (get-dist min-key-cell) own-dist) ; red
          own-cell

          (< (get-dist min-key-cell) own-dist) ; green
          min-key-cell

          (= (get-dist min-key-cell) own-dist) ; yellow
          (or
            (some #(viable-cell? own-dist body %) cells)
            own-cell))))))

(defn potential-field-player-following [body]
  (let [posi (get-position body)
        own-cell (get-cell posi)
        result (find-next-cell body own-cell)]
    (cond
      (= result :near-player-cell)
      (get-vector-to-player body)

      (not result)
      nil

      :else
      (let [cell result]
        (when-not (and (= cell own-cell)
                       (occupied-by-other? own-cell body)) ; verhindere reibereien 2 bewegen sich auf mitte
          (when-not (inside-cell? body cell)
            (direction-vector posi (:middle @cell))))))))

(defn path-to-player-movement [speed]
  (movement-component
    {:control-update (fn [body _ _]
                       (potential-field-player-following body))} ; so damit man das redeffen kann
    speed
    :ground)) ; works only with ground (expands to ground and checks ground)

;; DEBUG RENDER

(defn render-potential-field-info [g leftx topy xrect yrect cell]
  (when-let [dist (get-dist cell)]
    (comment (let [ratio (/ dist max-iterations)]
      (fill-rect g leftx topy tile-width tile-height
        (rgbcolor :r (- 1 ratio) :a 0.5))))
    (set-color g color/white)
    (draw-string g xrect yrect (int dist))))

(let [a 0.5]
  (defcolor transp-red :r 1 :a a)
  (defcolor transp-green :g 1 :a a)
  (defcolor transp-orange :r 1 :g 0.34 :a a)
  (defcolor transp-yellow :r 1 :g 1 :a a))

(def- adjacent-cells-colors (atom nil))

(defn calculate-mouseover-body-colors [mouseoverbody]
  (when-let [body mouseoverbody]
    (let [occupied-cell (get-cell (get-position body))
          own-dist (get-dist occupied-cell)
          adj-cells (cached-get-adjacent-cells occupied-cell)
          potential-cells (filter get-dist
                                  (filter-viable-cells body adj-cells))
          adj-cells (remove nil? adj-cells)]
      (reset! adjacent-cells-colors
        (genmap adj-cells
          (fn [cell]
            (cond
              (not-any? #{cell} potential-cells)
              transp-red

              (not own-dist) ; die andre hat eine dist da sonst potential-cells rausgefiltert -> besser als jetzige cell.
              transp-green

              (< own-dist (get-dist cell))
              transp-red

              (= own-dist (get-dist cell))
              transp-yellow

              :else transp-green)))))))

(defn render-potential-field-following-mouseover-info
  [g leftx topy xrect yrect cell mouseoverbody]
  (when-let [body mouseoverbody]
    (when-let [color (get @adjacent-cells-colors cell)]
      (fill-rect g leftx topy tile-width tile-height color))))

