(ns mapgen.utils
  (:use data.grid2d))

(defn scalegrid [grid factor]
  (create-grid (* (width grid) factor)
               (* (height grid) factor)
               (fn [posi]
                 (get grid (mapv #(int (/ % factor)) posi)))))
; TODO other keys in the map-grid are lost -> look where i transform grids like this
; merge with ld-grid?

(defn create-borders-positions [grid] ; TODO not distinct -> apply distinct or set
  (let [w (width grid),h (height grid)]
    (concat
      (mapcat (fn [x] [[x 0] [x (dec h)]]) (range w))
      (mapcat (fn [y] [[0 y] [(dec w) y]]) (range h)))))

(defn get-3x3-cellvalues [grid posi]
  (map grid (cons posi (get-8-neighbour-positions posi))))

(defn not-border-position? [[x y] grid]
  (and (>= x 1) (>= y 1)
       (< x (dec (width grid)))
       (< y (dec (height grid)))))

(defn border-position? [p grid] (not (not-border-position? p grid)))

(defn wall-at? [grid posi]
  (= :wall (get grid posi)))

(defn undefined-value-behind-walls
  "also border positions set to undefined where there are nil values"
  [grid]
  (transform grid
             (fn [posi value]
               (if (and (= :wall value)
                        (every? #(let [value (get grid %)]
                                   (or (= :wall value) (nil? value)))
                                (get-8-neighbour-positions posi)))
                 :undefined
                 value))))

(defn fill-single-cells [grid]
  (transform grid
             (fn [posi value]
               (if (and (not-border-position? posi grid)
                        (= :wall value)
                        (not-any? #(wall-at? grid %) (get-4-neighbour-positions posi)))
                 :ground
                 value))))
