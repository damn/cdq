(ns game.tools.transitiontilemaker
  (:use
    utils.core
    data.grid2d))

; this transitiontilemaker works with scaledx2 maps and utilizes just 12 different tiles (else 256 different tiles would be needed)
; so for each of these 12 tiles correspond to a number of the 256 combinations

; algorithm from: http://www.saltgames.com/2010/a-bitwise-method-for-applying-tilemaps/
(defn- transitiontile
  ([k obligatory]
    {:pre [(= (count obligatory) 1)]}
    {(first obligatory) k})
  ([k obligatory optional]
    (zipmap (map #(apply + % obligatory) (apply +perm optional))
            (repeat k))))

(def- idxvalues [1 128 64 2 32 4 8 16])

(let [idxvalues-order [[-1 -1] [-1 0] [-1 1] [0 -1] [0 1] [1 -1] [1 0] [1 1]]]
  (assert (= (get-8-neighbour-positions [0 0]) idxvalues-order)))

(comment
  ; Values for every neighbour:
  {[-1 -1] 1  [0 -1] 2 [1 -1] 4
   [-1 0]  128         [1 0] 8
   [-1 1]  64 [0 1] 32 [1 1] 16}

  [1   2  4
   128 _  8
   64  32 16])

(def- neighbour-idxvalue-to-transitiontile-key
  (apply safe-merge
         (map #(apply transitiontile %)
              [[:topleft        [1]]
               [:topright       [4]]
               [:bottomleft     [64]]
               [:bottomright    [16]]
               [:left           [128]    [1 64]]
               [:right          [8]      [4 16]]
               [:top            [2]      [1 4]]
               [:bottom         [32]     [16 64]]
               [:topandleft     [128 2]  [1 4 64]]
               [:topandright    [2 8]    [1 4 16]]
               [:leftandbottom  [128 32] [1 64 16]]
               [:rightandbottom [8 32]   [16 4 64]]])))

(def- transitiontile-keys
  (set (vals neighbour-idxvalue-to-transitiontile-key)))

(defn- get-transition-tile-idxvalue [count-neighbour? grid posi]
  (apply +
         (map-indexed (fn [idx p] (if (count-neighbour? p (get grid p))
                                    (get idxvalues idx)
                                    0))
                      (get-8-neighbour-positions posi))))

(defn- get-localid-for-tile-idxvalue [neighbourvalues id default-value]
  (or (get neighbourvalues
           (get neighbour-idxvalue-to-transitiontile-key id))
      default-value))

(defnks get-transition-tile-value [posi grid :neighbourvalues :default-value :count-neighbour?]
  (let [ks (set (keys neighbourvalues))]
    (assert (= ks transitiontile-keys)
            (str "keys are not transitiontilekeys: " ks)))
  (get-localid-for-tile-idxvalue neighbourvalues
                                 (get-transition-tile-idxvalue count-neighbour? grid posi)
                                 default-value))

; TODO also beforestring/afterstring automatically machen
; just input grid+tilesets.


; TODO make a visual editor for transitiontile properties?
; just set for every tile obligatory/optional and it calculates localid automatically.?
