; urgent: remove protocols because lein clean problems (what bt my idea of working w protocols later?)
; TODO next:
; engine.input
; remove duplicated `cdq.core` in namespace forms.
(ns cdq.core
  (:import (java.io File)
           (java.util Random)
           (java.util.zip ZipInputStream)
           (game.utils RayCaster)
           (org.lwjgl.opengl Display)
           (org.newdawn.slick AppGameContainer
                              BasicGame
                              Color
                              Font
                              GameContainer
                              Graphics
                              Image
                              Input
                              ScalableGame
                              Sound
                              SpriteSheet
                              SpriteSheetFont)
           (org.newdawn.slick.geom Circle Rectangle Shape Transform Vector2f)
           (org.newdawn.slick.state StateBasedGame)
           (org.newdawn.slick.tiled LightManager TiledMap)
           (org.newdawn.slick.util.pathfinding AStarPathFinder Mover Path TileBasedMap)
           (org.newdawn.slick.opengl CursorLoader))
  (:require [clojure.set :as set]
            [clojure.string :as str]))

(defn indexed ; from clojure.contrib.seq-utils (discontinued in 1.3)
  "Returns a lazy sequence of [index, item] pairs, where items come
 from 's' and indexes count up from zero.

 (indexed '(a b c d)) => ([0 a] [1 b] [2 c] [3 d])"
  [s]
  (map vector (iterate inc 0) s))

(defn positions ; from clojure.contrib.seq-utils (discontinued in 1.3)
  "Returns a lazy sequence containing the positions at which pred
	 is true for items in coll."
  [pred coll]
  (for [[idx elt] (indexed coll) :when (pred elt)] idx))

(defn distinct-seq?
  "same as (apply distinct? coll) but returns true if coll is empty/nil."
  [coll]
  (if (seq coll)
    (apply distinct? coll)
    true))

(defn safe-merge
  "same as merge but asserts no key is overridden."
  [& maps]
  (let [ks (mapcat keys maps)]
    (assert (distinct-seq? ks) (str "not distinct keys: " (apply str (interpose "," ks)))))
  (apply merge maps))

(defn mapvals [m f]
  (into {} (for [[k v] m]
             [k (f v)])))

(defn genmap
  "function is applied for every key to get value. use memoize instead?"
  [ks f]
  (zipmap ks (map f ks)))

; geht das auch ohne lambda-fn und mit forms als 2. element wie -> ??
; mit forms ???
(defn thread-through
  "Threads the expr through the sequence of fns, each taking expr as arg and returning it. see '->'.
   The order of evaluation is backwards."
  [expr fns]
  ((apply comp fns) expr))

(defmacro when-seq [[aseq bind] & body]
  `(let [~aseq ~bind]
     (when (seq ~aseq)
       ~@body)))

(defn assoc-ks [m ks v]
  (if (empty? ks)
    m
    (apply assoc m (interleave ks (repeat v)))))

(defn filter-map [m pred]
  (select-keys m (for [[k v] m :when (pred v)] k)))


(defn create-counter
  ([]        (atom {:current 0}))
  ([maxtime] (atom {:current 0,:max maxtime})))

(defn reset-counter! [counter]
  (swap! counter assoc-in [:current] 0))

(defn update-counter
  "updates counter. if maxtime reached, resets current to 0 and returns the last current value,
   else returns nil."
  [counter delta]
  (swap! counter update-in [:current] + delta)
  (let [{current :current maxtime :max} @counter]
    (when (and maxtime (>= current maxtime))
      (let [last-current current]
        (reset-counter! counter)
        last-current))))


(defn approx-numbers [a b epsilon]
  (<=
    (Math/abs (float (- a b)))
    epsilon))

(defn round-n-decimals [x n]
  (let [z (Math/pow 10 n)]
    (float
      (/
        (Math/round (float (* x z)))
        z))))

(defn readable-number [x]
  {:pre [(number? x)]} ; do not assert (>= x 0) beacuse when using floats x may become -0.000...000something
  (if (or
        (> x 5)
        (approx-numbers x (int x) 0.001)) ; for "2.0" show "2" -> simpler
    (int x)
    (round-n-decimals x 2)))

(defn variance-val-str [[mi mx]]
  (str (readable-number mi) "-" (readable-number mx)))

(defn variance-val
  "(variance-val 10 0.9) is [1 19] (floating point! real result may be [0.99999998 19.0]) "
  [avg variance]
  {:pre [(>= variance 0) (<= variance 1)]}
  [(* avg (- 1 variance))
   (* avg (inc variance))])

(defn get-ratio
  ([{:keys [current max]}] (get-ratio current max))
  ([current max] (/ current max)))

(defn min-max-val [n] {:max n :current n})

(defn lower-than-max? [{:keys [current max]}]
  (< current max))

(defn rest-to-max [{:keys [current max]}]
  (- max current))

(defn set-to-max [data] (assoc data :current (:max data)))

(defn increase-min-max-val ; bound-inc ?
  [{current :current mx :max :as data} by]
  {:pre [(>= by 0)]}
  (update-in data [:current] + (min by (- mx current))))

; just bound-inc ? and pass (- val) ?
(defn inc-or-dec-max [min-max-val f by] ; TODO apply-max !
  (let [ratio (get-ratio min-max-val)
        new-max (f (:max min-max-val) by)]
    (assoc min-max-val
      :max new-max
      :current (* ratio new-max))))


(defprotocol Grid2D
  (transform [this f])
  (posis [this])
  (cells [this])
  (width [this])
  (height [this]))

(defn- transform-values [data width height f]
  (mapv
    (fn [x] (loop [v (transient (get data x))
                   y 0]
              (if (not= y height)
                (recur (assoc! v y (f [x y] (get v y)))
                       (inc y))
                (persistent! v))))
    (range width)))

(deftype VectorGrid [data]
  Grid2D
  (transform [this f]
    (VectorGrid. (transform-values data (width this) (height this) f)))
  (posis [this]
    (for [x (range (width this))
          y (range (height this))]
      [x y]))
  (cells [this] (apply concat data))
  (width [this] (count data))
  (height [this] (count (data 0)))

  clojure.lang.ILookup
  (valAt [this p]  ; {x 0 y 1} or [x y] is much slower
    (-> data
        (nth (nth p 0) nil)
        (nth (nth p 1) nil)))

  clojure.lang.IFn
  (invoke [this p] (.valAt this p))

  clojure.lang.Seqable
  (seq [this]
    (map #(vector %1 %2) (posis this) (cells this)))

  clojure.lang.IPersistentCollection
  (equiv [this obj]
    (and (= VectorGrid (class obj))
         (= (.data ^VectorGrid obj) data)))

  clojure.lang.Associative
  (assoc [this p v]
    (VectorGrid. (assoc-in data p v)))
  (containsKey [this [x y]]
    (and (contains? data x)
         (contains? (data 0) y)))

  Object
  (hashCode [this] (.hashCode data))
  (equals [this obj]
    (and (= VectorGrid (class obj))
         (.equals (.data ^VectorGrid obj) data)))
  (toString [this]
    (str "width " (width this) ", height " (height this))))

(defn- vector2d [w h f]
  (mapv (fn [x] (mapv (fn [y] (f [x y]))
                      (range h)))
        (range w)))

(defn create-grid
  [w h xyfn]
  {:pre [(>= w 1) (>= h 1)]}
  (VectorGrid. (vector2d w h xyfn)))

(defn print-grid [grid & {print-cell :print-cell
                          :or {print-cell
                               #(print (case % :wall "#" :ground "_" "?"))}}]
  (doseq [y (range (height grid))]
    (doseq [x (range (width grid))]
      (print-cell (grid [x y])))
    (println)))

(defn mapgrid->vectorgrid
  [grid calc-newgrid-value]
  (let [posis (keys grid)
        xs (map #(% 0) posis)
        min-x (apply min xs)
        max-x (apply max xs)
        ys (map #(% 1) posis)
        min-y (apply min ys)
        max-y (apply max ys)
        width (inc (- max-x min-x))
        height (inc (- max-y min-y))
        convert (fn [[x y]] [(- x min-x -1)
                             (- y min-y -1)])]
    ; +2 so there are walls on all borders around the farthest ground cells
    [(create-grid (+ width 2) (+ height 2)
                  (fn [[x y]]
                    ; new grid starts 1 left/top of leftest cell
                    (calc-newgrid-value (get grid [(+ x min-x -1)
                                                   (+ y min-y -1)]))))
     convert]))

(defn get-4-neighbour-positions [[x y]]
  [[(inc x) y]
   [(dec x) y]
   [x (inc y)]
   [x (dec y)]])

(defn get-8-neighbour-positions [[x y]]
  (for [tx (range (dec x) (+ x 2))
        ty (range (dec y) (+ y 2))
        :when (not= [x y] [tx ty])]
    [tx ty]))


(defn blocks-component [state-blocks-map]
  {:pre [(map? state-blocks-map)]}
  {:blocked-comptype nil
   :state-blocks state-blocks-map})

(defn not-blocked? [{blocks :blocks :as component}]
  (if blocks
    (do
      (assert (>= blocks 0))
      (zero? blocks))
    true))

(defn- check-assoc-blocks [component]
  (if (contains? component :blocks)
    component
    (assoc component :blocks 0)))

(defn add-block [entity ctype]
  (if ctype
    (-> entity
      (update-in [ctype] check-assoc-blocks)
      (update-in [ctype :blocks] inc))
    entity))

(defn remove-block [entity ctype]
  (if ctype
    (do
      (assert (contains? (ctype entity) :blocks))
      (assert (pos? (get-in entity [ctype :blocks])) (str "Blocks=" (get-in entity [ctype :blocks]) " (Must be >0 for removing.)"))
      (update-in entity [ctype :blocks] dec))
    entity))

(defn add-blocks [entity ctypes] (reduce add-block entity ctypes))

(defn remove-blocks [entity ctypes] (reduce remove-block entity ctypes))

(defn- switch-blocking
  "removes old block and adds a new block if required."
  [entity {:keys [blocked-comptype state-blocks] :as component} new-state]
  (let [new-blocked-type (get state-blocks new-state)]
    (-> entity
      (remove-block blocked-comptype)
      (assoc-in [(:type component) :blocked-comptype] new-blocked-type)
      (add-block new-blocked-type))))

(defn switch-state [entity ctype new-state]
  (-> entity
    (assoc-in [ctype :state] new-state)
    (switch-blocking (ctype entity) new-state)))

(comment
  (map
    (fn [c] [(:type c) (:blocks c)])
    (filter :updatefn (vals @player-body)))
)

;;

(defn- modify-delta
  "delta must remain integer. becuz some java counters for example animation works with int/long only."
  [delta]
  (int (* delta 0.4)))

(defn try-slowdown-delta [delta component]
  (if (:slowed component) (modify-delta delta) delta))

(defn mark-slowed [component] (assoc component :slowed true))

(defn unmark-slowed [component] (dissoc component :slowed))


(defprotocol Session ; State
  (load-session     [_ data]) ; set-state
  (save-session     [_])      ; current-value
  (new-session-data [_]))     ; initial-value

(deftype AtomSessionState [a initial]
  Session
  (load-session [_ data]
    (reset! a data))
  (save-session [_]
    @a)
  (new-session-data [_]
    initial))

(deftype NoSaveAtomSessionState [a initial]
  Session
  (load-session [_ _]
    (reset! a initial))
  (save-session [_])
  (new-session-data [_]))

(defn atom-session [a & {save :save-session :or {save true}}]
  {:pre [(instance? clojure.lang.Atom a)]}
  (if save
    (AtomSessionState.       a @a)
    (NoSaveAtomSessionState. a @a)))

;; Serialisation

(defn- pr-type [x]
  (or (get (meta x) :pr) (class x)))

(defmulti load-from-disk pr-type)
(defmulti write-to-disk  pr-type)

(defmethod load-from-disk :default [x] x)
(defmethod write-to-disk  :default [x] x)

(comment
  ; An alternative to load-from-disk and write-to-disk would be print-dup,
  ; it retains metadata and type info but is less readable than walk-term
  ; But anyhow without pretty print and with more and more entities, it isnt readable anyway.
  ; Another disadvantage that it only dispatches on class, so it doesnt work for item.
  (binding [*print-dup* true]
    (pr-str (Color. 1 1 1)))
  (defmethod print-dup Color [c out]
    (.write out
            (str "#="
                 `(Color. ~(.r c) ~(.g c) ~(.b c) ~(.a c))))))


(declare ^:private config)

(defn init-config! [file]
  (alter-var-root #'config (fn [_]
                             (-> file slurp read-string))))

(defn get-setting [k]
  (let [value (get-in config
                      (if (sequential? k) k [k])
                      ::not-found)]
    (if (= value ::not-found)
      (throw (IllegalArgumentException. (str "Could not find key " k " in config")))
      value)))

; TODO this stuff should not be here ? ..
(def screen-width 384)
(def screen-height 288)
(def screen-scale 2)

(def version "alpha3")

;;

(def debug-mode (atom false))

;;

(def tile-width 16)
(def tile-height 16)

(defn in-tiles [pixel] (/ pixel tile-width))
(defn in-pixel [tiles] (* tiles tile-height))

(assert (= tile-width tile-height)) ; da oft einfach (in-tiles pxradius) & auch movement ist sonst weird -> tilew/h gleich voraussetzung!

(def half-screen-w (/ screen-width 2))
(def half-screen-h (/ screen-height 2))

(def display-width-in-tiles (/ screen-width tile-width))
(def display-height-in-tiles (/ screen-height tile-height))

(def half-display-w-in-tiles (/ display-width-in-tiles 2))
(def half-display-h-in-tiles (/ display-height-in-tiles 2))

(def left-offset-in-tiles (int half-display-w-in-tiles))
(def top-offset-in-tiles (int half-display-h-in-tiles))


(def entities-always-in-los true)

(def show-body-debug-info false)

(def show-body-bounds true)

; todo show lightning active on debug yes no

(def show-float-mouse-pos false)

(def show-tile-mouse-pos true)

(def show-lightning-info false) ; does not take from cache ... => change ...

(def show-comps-count false)

(def show-contentfield false)

; map rendering

(def show-map-grid true)

(def show-blocked-cells false)
(def show-besetzte-cells false)
(def show-occupied-cells false)

(def show-potential-field false)

(def potential-field-following-mouseover-info false)

; TODO add ob lightning aktiv ist bei debug
; (def lightning-on true)
; bei debug-mode an lightning settings zu dem hier setzen?

(def player-noclip-info false)

(defn split-kvs-and-more [args]
  (let [pairs (partition-all 2 args)
        [kvpairs restpairs] (split-with #(keyword? (first %)) pairs)
        key-vals-map (apply hash-map (apply concat kvpairs))]
    [key-vals-map (apply concat restpairs)]))

(let [cnt (atom 0)]
  (defn get-unique-number [] (swap! cnt inc)))

(defn- time-of [expr n]
  `(do
     (println "Time of: " ~(str expr))
     (time (dotimes [_# ~n] ~expr))))

(defmacro compare-times [n & exprs]
  (let [forms (for [expr exprs]
                (time-of expr n))]
    `(do ~@forms nil)))

(defmacro deflazygetter [fn-name & exprs]
  `(let [delay# (delay ~@exprs)]
     (defn ~fn-name [] (force delay#))))

(defn when-apply [f & args]
  (when f (apply f args)))

(defn diagonal-direction? [[x y]]
  (and (not (zero? x))
       (not (zero? y))))

(defn int-posi [p] (mapv int p))

(defn log [& more]
  (println "~~~" (apply str more)))

;; Order

; TODO deforder?
(defn define-order [order-k-vector]
  (apply hash-map
    (interleave order-k-vector (range))))

(defn sort-by-order [coll get-item-order-k order]
  (sort-by #((get-item-order-k %) order) < coll))

(defn order-contains? [order k]
  ((apply hash-set (keys order)) k))

;;

(defn make-fn [argseq]
  (if (symbol? (first argseq))
    (first argseq)
    `(fn ~@argseq)))

(defn print-n-return [data] (println data) data)

(defn get-next-idx
  "returns the next index of a vector.
  if there is no next -> returns 0"
  [current-idx coll]
  (let [next-idx (inc @current-idx)]
    (reset! current-idx (if (get coll next-idx) next-idx 0))))

(defn translate-to-tile-middle
  "translate position to middle of tile becuz. body position is also @ middle of tile."
  [p]
  (mapv (partial + 0.5) p))

;;

(defn split-key-val-and-maps
  "For a args-seq of key-vals and maps -> creates a key-vals-map and a map that is all maps merged.
  for example [:a 1 :b 2 :c 3 {:d 4} {:e 5}] results in [{:a 1 :b 2 :c 3} {:d 4 :e 5}].
  When no values are supplied for key-vals and/or maps returns [{} {}]"
  [args]
  (let [[key-vals-map more] (split-kvs-and-more args)]
    [key-vals-map (apply merge {} more)]))

(comment
  (split-key-val-and-maps [:a 1])
  [{:a 1} {}]
  (split-key-val-and-maps [:a 1 :b 2 :c 3])
  [{:a 1, :c 3, :b 2} {}]
  (split-key-val-and-maps [:a 1 :b 2 :c 3 {:d 4} {:e 5}])
  [{:a 1, :c 3, :b 2} {:e 5, :d 4}]
  (split-key-val-and-more [:a 1 :b 2 :c 3 {:d 4} {:e 5}])
  [{:a 1, :c 3, :b 2} ({:d 4} {:e 5})]
  (split-key-val-and-more [:a 1 :b 2 :c 3 "this is a string"])
  [{:a 1, :c 3, :b 2} ("this is a string")]
  (split-key-val-and-more [:a 1 :b 2 :c 3 "this is a string" "and another"])
  [{:a 1, :c 3, :b 2} ("this is a string" "and another")])

;;

(defn boolperm [n]
  {:pre [(integer? n) (>= n 0)]}
  (if (zero? n) [[]]
    (mapcat (fn [tupel] [(conj tupel true) (conj tupel false)]) (boolperm (dec n)))))

(defn +perm [& numbers] ; TODO add an example to understand
  (map (fn [tupel]
         (apply +
           (map-indexed #(if-not %2 0 (nth numbers %1)) tupel)))
    (boolperm (count numbers))))

(defmacro xor [a b]
  `(or (and (not ~a) ~b)
       (and ~a (not ~b))))

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

(defn- position-of [elem coll]
  (first (positions #{elem} coll)))

(defn- comes-after? "checks if o1 comes after o2."
  [o1 o2]
  (some #{(:type o2)} (:after o1)))

(defn- has-correct-order? [elem a-seq]
  (if-not (:after elem)
    true
    (let [posis (positions #(comes-after? elem %) a-seq)]
      (cond
        (empty? posis) true

        (let [idx (position-of elem a-seq)]
          (every? #(> idx %) posis))
        true

        :else false))))

(defn- remove-one [item coll]
  (let [idx (position-of item coll)]
    (concat (take idx coll) (drop (inc idx) coll))))

(defn- fix [incorrect a-seq]
  (let [adjusted-seq (remove-one incorrect a-seq)
        correct-idx (inc (apply max
                           (positions #(comes-after? incorrect %) adjusted-seq)))]
    (concat (take correct-idx adjusted-seq) [incorrect] (drop correct-idx adjusted-seq))))

(defn order-maps
  "Orders a sequence of maps, where every map has obligatory key :type and optional key :after types.
   Cyclic orders lead to an error."
  ([a-seq]
    (order-maps a-seq (count a-seq)))
  ([a-seq iterations-left]
    (if (zero? iterations-left)
      (throw (Error. "May be a cyclic order! Should be finished after (count a-seq) iterations."))
      (if-let [incorrect (first (filter #(not (has-correct-order? % a-seq)) a-seq))]
        (order-maps (fix incorrect a-seq) (dec iterations-left))
        a-seq))))

; only change @player-death and switch-debug
(def running (atom true))
(def update-session (atom-session running :save-session false))

; das spiel soll bei 20fps noch "schnell" sein,d.h. net langsamer werden (max-delta wirkt -> game wird langsamer)
; 1000/20 = 50
(def max-delta 50)

(defn update-component [delta component entity]
  ((:updatefn component) delta component entity))

;-> mit active zusammen in einen namespace?
;-> und vielleicht auch mit blocks?
(defn update-active-components [delta entities]
  (doseq [entity entities
          component (vals @entity)
          :when (and (:updatefn component)
                     (not-blocked? component))]
    (try
      (update-component (try-slowdown-delta delta component) component entity)
      (catch Throwable t
        (println "Entity " (:id (meta entity)) " with component " (:type component) " fails: " t)
        (reset! running false)
        (throw t)))))

; Tempor�r hier- setze genau da wo geloopt wird hin!
(def iterating-map-dependent-comps (atom false))
(defn iterating-components? [] @iterating-map-dependent-comps)

(def maps-data-session (reify Session
               (load-session [_ _]
                 (def ^:private maps-data {})
                 (def added-map-order []))
               (save-session [_])
               (new-session-data [_])))

(defn add-map [k data]
  (alter-var-root #'maps-data assoc k data)
  (alter-var-root #'added-map-order conj k))

(defn get-map-keys [] (keys maps-data))

(defn get-map-data [map-name] (get maps-data map-name))

(defn get-pretty-name [map-key] (:pretty-name (get-map-data map-key)))

; TODO faster access when current-map references maps-data and not the key?
; test get-cells for example ...

; 1 Do not change this while @update-components
; -> or all comps of one map using grids of other maps etc.
; 2 use only do-in-map or set-map!
; 3 only set to a existing map - key
; TODO VAR DOC?
(def current-map (atom nil :validator (fn [_] (assert (not (iterating-components?)))
                                        true)))

(defn set-map! [new-map]
  {:pre [(contains? maps-data new-map)]}
  (reset! current-map new-map))

(defmacro do-in-map
  "Executes the exprs with current-map set to \"in\" and restores the previous current-map afterwards."
  [in & exprs]
  `(let [old# @current-map
         ~'_ (reset! current-map ~in)
         retrn# (do ~@exprs)]
     (reset! current-map old#)
     retrn#))

(defn get-current-map-data [] (get maps-data @current-map))

; primary keys because of cyclic dependencies between options menu and ingame-state
(def ingame  (get-unique-number))
(def options (get-unique-number))
(def minimap (get-unique-number))

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

(def ^:private idxvalues [1 128 64 2 32 4 8 16])

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

(def ^:private neighbour-idxvalue-to-transitiontile-key
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

(def ^:private transitiontile-keys
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

(defn get-transition-tile-value [posi grid & {:keys [neighbourvalues default-value count-neighbour?]}]
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

;; Random

;; rename to 'shuffle', rand and rand-int without the 's'-> just use with require :as.
;; maybe even remove the when coll pred?
;; also maybe *random* instead passing it everywhere? but not sure about that

(defn sshuffle
  "Return a random permutation of coll"
  ([coll random]
    (when coll
      (let [al (java.util.ArrayList. ^java.util.Collection coll)]
        (java.util.Collections/shuffle al random)
        (clojure.lang.RT/vector (.toArray al)))))
  ([coll]
    (sshuffle coll (Random.))))

(defn srand
  ([random] (.nextFloat ^Random random))
  ([n random] (* n (srand random))))

(defn srand-int [n random] (int (srand n random)))

(defn create-seed [] (.nextLong (Random.)))

(defn rand-int-between
  "returns a random integer between lower and upper bounds inclusive."
  ([[lower upper]]
    (rand-int-between lower upper))
  ([lower upper]
    (+ lower (rand-int (inc (- upper lower))))))

(defn rand-float-between [[lower upper]]
  (+ lower (rand (- upper lower))))

(defn percent-chance
  "perc is number between 0 and 100."
  ([perc random]
    (< (srand random) (/ perc 100)))
  ([perc]
    (percent-chance perc (Random.))))

(defmacro if-chance
  ([n then]
    `(if-chance ~n ~then nil))
  ([n then else]
    `(if (percent-chance ~n) ~then ~else)))

(defmacro when-chance [n & more]
  `(when (percent-chance ~n) ~@more))

(defn get-rand-weighted-item
  "given a sequence of items and their weight, returns a weighted random item.
 for example {:a 5 :b 1} returns b only in about 1 of 6 cases"
  [weights]
  (let [result (rand-int (reduce + (map #(% 1) weights)))]
    (loop [r 0
           items weights]
      (let [[item weight] (first items)
            r (+ r weight)]
        (if (> r result)
          item
          (recur r (rest items)))))))

(defn get-rand-weighted-items [n group]
  (repeatedly n #(get-rand-weighted-item group)))

(comment
  (frequencies (get-rand-weighted-items 1000 {:a 1 :b 5 :c 4}))
  (frequencies (repeatedly 1000 #(percent-chance 90))))

(defn high-weighted "for values of x 0-1 returns y values 0-1 with higher value of y than a linear function"
  [x]
  (- 1 (Math/pow (- 1 x) 2)))

(defn- high-weighted-rand-int [n]
  (int (* n (high-weighted (rand)))))

(defn high-weighted-rand-nth [coll]
  (nth coll (high-weighted-rand-int (count coll))))

; method of AppGameContainer does flip = true; -> so flip the icons vertical!
; dont supply a icon24.tga else windows will use it for ALT+TAB which needs a 32x32!
; uncompressed 32BIT TGA IMAGES
; from http://lwjgl.org/javadoc/org/lwjgl/opengl/Display.html#setIcon%28java.nio.ByteBuffer[]%29
; Sets one or more icons for the Display.
;
;    On Windows you should supply at least one 16x16 icon and one 32x32.
;    Linux (and similar platforms) expect one 32x32 icon.
;    Mac OS X should be supplied one 128x128 icon
;
;The implementation will use the supplied ByteBuffers with image data in RGBA and perform any conversions nescesarry for the specific platform.
(def ^:private icon-strings (into-array ["icon16.tga" "icon32.tga"]))

(defn create-and-start-app-game-container
  [& {:keys [game width height show-fps lock-framerate full-screen scale]}]
  (defn get-screen-width [] width)
  (defn get-screen-height [] height)
  (let [scale (if (= 1 scale) nil scale)
        container (AppGameContainer. (if scale (ScalableGame. game width height true) game) ; true = maintain aspect ratio
                                     (if scale (* scale width) width)
                                     (if scale (* scale height) height)
                                     (boolean full-screen))]

    (.setIcons container icon-strings)
    (.setShowFPS container (boolean show-fps))
    (when lock-framerate
      (doto container
        (.setTargetFrameRate 60)
        (.setVSync true)))
    (def app-game-container container)
    (.start (Thread.
              #(.start container)))))

(def init-fns (list))

(defn start-slick-basicgame
  [& {:keys [full-screen title width height init update render]
      :or {title "test" width 800 height 600
           update (fn [container delta])
           render (fn [container g])}}]
  (create-and-start-app-game-container
    :game (proxy [BasicGame] [title]
            (init [container]
              (init container))
            (update [container delta]
              (update container delta))
            (render [container g]
              (render container g)))
    :width width
    :height height
    :full-screen full-screen))

(defn fullscreen-supported?
  ([w h]
    (some #(and (= w (.getWidth %)) (= h (.getHeight %)))
          (Display/getAvailableDisplayModes)))
  ([]
    (let [current (Display/getDisplayMode)]
      (fullscreen-supported? (.getWidth current)
                             (.getHeight current)))))

; not (some #{displayMode} availables)
; because #<DisplayMode 1152 x 864 x 0 @0Hz> is not in AvailableDisplayModes for fullscreen... (different Hz)

(defn create-sound [spath]
  (Sound. ^String (str "sounds/" spath)))

(defn play-sound [spath]
  (.play ^Sound (create-sound spath)))

(defn playonce [^Sound sound]
  (when-not (.playing sound)
    (.play sound)))

(def allowed-characters (set (map char (range 32 126))))

; anuvverbubbla font has (range 32 91) & at draw-string make (.toUpperCase (str s))
; (alter-var-root #'cdq.core/init-fns conj
;   (fn []
;     (def ^:private anuvverbubbla (SpriteSheetFont. (SpriteSheet. "anuvverbubbla_8x8.png" 8 8) \space))))

(deflazygetter get-defaultfont
  (SpriteSheetFont. (SpriteSheet. "simple_6x8.png" 6 8) \space))
; using lazygetter instead of preload because other preload depends on this (make-frame => get text size)

(defn reset-font [g] (.setFont ^Graphics g (get-defaultfont)))

(defn- get-font []
  (.getFont ^Graphics (.getGraphics ^GameContainer app-game-container)))

(defn get-line-height
  ([font] (.getLineHeight ^Font font))
  ([] (get-line-height (get-font))))

; Workaround for text-height & width because "\n" is not registered @ font getHeight/getWidth of SpriteSheetFont

(defn- number-of-lines [s]  ; do not use (count (split-lines s)) because for example with "abc\n\n" is only split into ["abc"]
  (inc (count (filter #(= \newline %) s))))

(defn get-text-height
  ([text] (get-text-height text (get-font)))
  ([text font] (* (number-of-lines text) (get-line-height font))))

(defn get-text-width
  ([text font] (.getWidth ^Font font (apply max-key count (str/split-lines text))))
  ([text] (get-text-width text (get-font))))

(defn set-mouse-cursor [data hotspotx hotspoty]
  (.setMouseCursor app-game-container data hotspotx hotspoty))

(defprotocol Updateable
  (tick [_ delta]))

(defrecord ImmutableCounter [cnt maxcnt stopped?]
  Updateable
  (tick [this delta]
    (let [newcnt   (+ cnt delta)
          stopped? (>= newcnt maxcnt)]
      (assoc this
             :cnt (if stopped? (- newcnt maxcnt) newcnt)
             :stopped? stopped?))))

; if (> maxdelta maxcnt) it could happen that after tick it is again 'stopped?' in next tick
(defn make-counter [maxcnt]
  (ImmutableCounter. 0 maxcnt false))

(defn counter-ratio [{:keys [cnt maxcnt]}]
  (/ cnt maxcnt))

(defn tick-finally-merge [c k delta m]
  (let [counter (tick (k c) delta)]
    (merge (assoc c k counter)
           (when (:stopped? counter)
             m))))

(defn reset-counter [counter]
  (assoc counter :cnt 0))

; Call update-mousebutton-state each frame and run init-fns at game start.

(alter-var-root #'init-fns conj
  (fn []
    (def ^:private slick-input
      (.getInput app-game-container))))

(defn clear-key-pressed-record   [] (.clearKeyPressedRecord   ^Input slick-input))
(defn clear-mouse-pressed-record [] (.clearMousePressedRecord ^Input slick-input))

(defn get-mouse-pos []
  [(.getMouseX ^Input slick-input)
   (.getMouseY ^Input slick-input)])

(defn- to-mouse-key [k]
  (case k
    :left  Input/MOUSE_LEFT_BUTTON
    :right Input/MOUSE_RIGHT_BUTTON))

(defn- is-mouse-button-down? [k] (.isMouseButtonDown ^Input slick-input (to-mouse-key k)))
(defn- is-mouse-pressed?     [k] (.isMousePressed    ^Input slick-input (to-mouse-key k)))

(def is-leftbutton-down?  (partial is-mouse-button-down? :left))
(def is-rightbutton-down? (partial is-mouse-button-down? :right))

(def ^:private to-keyboard-key
  (memoize (fn [k]
             (eval (symbol (str "org.newdawn.slick.Input/KEY_" (name k)))))))

(defn is-key-pressed?
  "Since last call to this. So do not call this twice in one frame else it will return false."
  [k]
  (.isKeyPressed ^Input slick-input (to-keyboard-key k)))

(defn is-key-down? [k]
  (.isKeyDown ^Input slick-input (to-keyboard-key k)))

; when using is-...-pressed? it is probably useful also to check if is-...-consumed?
; for example a bug occured:
; waypoints menu opens with try-consume-..-pressed while is-...-pressed? closed it again in the same frame
; TODO maybe is-...-pressed? always checks if not consumed yet (so it is really 'consumed')

(def ^:private initial-mousebutton {:pressed  false
                                    :consumed false})

(def ^:private mousebutton-state (atom {:left  initial-mousebutton
                                        :right initial-mousebutton}))

(defn- mousebutton-pressed? [button] (-> @mousebutton-state button :pressed))

(defn is-leftm-pressed?  [] (mousebutton-pressed? :left))
(defn is-rightm-pressed? [] (mousebutton-pressed? :right))

(defn- mousebutton-consumed? [button] (-> @mousebutton-state button :consumed))

(defn is-leftm-consumed?  [] (mousebutton-consumed? :left))
(defn is-rightm-consumed? [] (mousebutton-consumed? :right))

(defn- check-if-pressed [mb-state button]
  (assoc-in mb-state [button :pressed] (is-mouse-pressed? button)))

(defn- resolve-consumed [mb-state button]
  (if (and (-> mb-state button :consumed)
           (not (is-mouse-button-down? button)))
    (assoc-in mb-state [button :consumed] false)
    mb-state))

(defn update-mousebutton-state []
  (swap! mousebutton-state #(-> %
                  (check-if-pressed :left)
                  (resolve-consumed :left)
                  (check-if-pressed :right)
                  (resolve-consumed :right))))

(defn- try-consume-mouse-pressed [button]
  (when (and (mousebutton-pressed? button)
             (not (mousebutton-consumed? button)))
    (swap! mousebutton-state assoc-in [button :consumed] true)))

(defn try-consume-leftm-pressed
  "If leftmouse was pressed this frame and not yet consumed, consumes it and returns true else returns nil.
   It is consumed as long as the leftmouse-button is down."
  []
  (try-consume-mouse-pressed :left))

(defn try-consume-rightm-pressed []
  (try-consume-mouse-pressed :right))

;; settings

(defn- running-as-jar? []
  (= "jar" (.getProtocol (clojure.java.io/resource "splash.gif"))))

(def jar-file? (running-as-jar?))

;; state-based game

(defn init-state-based-game [title states]
  (def state-based-game
    (proxy [StateBasedGame] [title]
      (initStatesList [_]
        (dorun (map #(.addState this %) states))))))

(defn enter-state [state]
  (.enterState state-based-game
    (if (number? state)
      (do (assert (integer? state)) state)
      (.getID state))))

;; colors

(def transparent Color/transparent)
(def white Color/white)
(def yellow Color/yellow)
(def red Color/red)
(def blue Color/blue)
(def green Color/green)
(def black Color/black)
(def gray Color/gray)
(def cyan Color/cyan)
(def darkGray Color/darkGray)
(def lightGray Color/lightGray)
(def pink Color/pink)
(def orange Color/orange)
(def magenta Color/magenta)

(defn rgbcolor [& {:keys [r g b a darker brighter]
                   :or {r 0 g 0 b 0 a 1 darker 0 brighter 0}}]
  (-> (Color. (float r) (float g) (float b) (float a))
      (.darker darker)
      (.brighter brighter)))

(defn set-color [^Graphics g color]
  (.setColor g color))

(def transparent-black (rgbcolor :a 0.8))

;; images

(defn get-dimensions [^Image image]
  [(.getWidth image)
   (.getHeight image)])

(defn draw-image
  ([image x y]
    (.draw ^Image image x y))
  ([image ^Float x ^Float y ^Float w ^Float h]
    (.draw ^Image image x y w h)))

(defn render-centered-image [image [x y]]
  (.drawCentered ^Image image x y))

(defn render-rotated-centered-image [^Graphics g image angle {x 0 y 1 :as position}]
  (.rotate g x y angle)
  (render-centered-image image position)
  (.rotate g x y  (- angle)))

(defn get-sub-image [^Image i x y w h]
  (.getSubImage i x y w h))

(defn get-scaled-copy
  ([^Image i value] (.getScaledCopy i value))
  ([^Image i w h]   (.getScaledCopy i w h)))

(defn create-image
  "org.newdawn.slick.Image data is cached(?) so if the same image path is loaded x2
  the second time it loads from the cached data much faster."
  [file & {:keys [transparent scale]}]
  ; Image constructor: String ref, boolean flipped, int f, Color transparent
  ; set this filter so there is no antialiasing effect when images are not at a perfect pixel position or rotated
  ; set in constructor not later in .setFilter so the image will not be loaded (deferred loading)
  (let [image (if transparent
                (Image. (str file) false Image/FILTER_NEAREST ^Color transparent)
                (Image. (str file) false Image/FILTER_NEAREST))]

    (cond
      (vector? scale) (apply get-scaled-copy image scale)
      (number? scale) (get-scaled-copy image scale)
      :else image)))

(defn create-empty-image [w h]
  (Image. (int w) (int h)))

;; cave generation

;Cave Algorithmus.
;http://properundead.com/2009/03/cave-generator.html
;http://properundead.com/2009/07/procedural-generation-3-cave-source.html
;http://forums.tigsource.com/index.php?topic=5174.0

(defn- cave-create-order [random]
  (sshuffle (range 4) random))

(defn- cave-get-in-order [v order]
  (map #(get v %) order))

(def ^:private cave-current-order (atom nil))

(def ^:private cave-turn-ratio 0.5)

(defn- cave-create-rand-4-neighbour-posis [posi n random] ; TODO does more than 1 thing
  (when (< (srand random) cave-turn-ratio)
    (reset! cave-current-order (cave-create-order random)))
  (take n
        (cave-get-in-order (get-4-neighbour-positions posi) @cave-current-order)))

(defn- cave-get-default-adj-num [open-paths random]
  (if (= open-paths 1)
    (case (int (srand-int 4 random))
      0 1
      1 1
      2 1
      3 2
      1)
    (case (int (srand-int 4 random))
      0 0
      1 1
      2 1
      3 2
      1)))

(defn- cave-get-thin-adj-num [open-paths random]
  (if (= open-paths 1)
    1
    (case (int (srand-int 7 random))
      0 0
      1 2
      1)))

(defn- cave-get-wide-adj-num [open-paths random]
  (if (= open-paths 1)
    (case (int (srand-int 3 random))
      0 1
      2)
    (case (int (srand-int 4 random))
      0 1
      1 2
      2 3
      3 4
      1)))

(def ^:private cave-get-adj-num
  {:wide cave-get-wide-adj-num
   :thin cave-get-thin-adj-num    ; höhle mit breite 1 überall nur -> cave-turn-ratio verringern besser
   :default cave-get-default-adj-num}) ; etwas breiter als 1 aber immernoch zu dünn für m ein game -> cave-turn-ratio verringern besser

; gute ergebnisse: :wide / 500-4000 max-cells / cave-turn-ratio 0.5
; besser 150x150 anstatt 100x100 w h
; TODO glaubich einziger unterschied noch: openpaths wird bei jeder cell neu berechnet?
; TODO max-tries wenn er nie über min-cells kommt? -> im let dazu definieren vlt max 30 sekunden -> in tries umgerechnet??
(defn cave-gridgen [random min-cells max-cells adjnum-type]
  (reset! cave-current-order (cave-create-order random))
  (let [start-posi [0 0]
        start-grid (assoc {} start-posi :ground) ; grid of posis to :ground or no entry for walls
        finished (fn [grid end-posi cell-cnt]
                   ;(println "Reached cells: " cell-cnt) ; TODO cell-cnt stimmt net genau
                   (if (< cell-cnt min-cells)
                     (cave-gridgen random min-cells max-cells adjnum-type) ; recur?
                     (let [[grid convert] (mapgrid->vectorgrid grid #(if (nil? %) :wall :ground))]
                       {:grid  grid
                        :start (convert start-posi)
                        :end   (convert end-posi)})))]
    (loop [posi-seq [start-posi]
           grid start-grid
           cell-cnt 0]
      (if (>= cell-cnt max-cells)
        (finished grid (last posi-seq) cell-cnt)
        (let [try-carve-posis (cave-create-rand-4-neighbour-posis
                                (last posi-seq)
                                ((cave-get-adj-num adjnum-type) (count posi-seq) random)
                                random)
              carve-posis (filter #(nil? (get grid %)) try-carve-posis)
              new-pos-seq (concat (drop-last posi-seq) carve-posis)]
          (if (not-empty new-pos-seq)
            (recur
              new-pos-seq
              (if (seq carve-posis)
                (assoc-ks grid carve-posis :ground)
                grid)
              (+ cell-cnt (count carve-posis)))
            (finished grid (last posi-seq) cell-cnt)))))))

;; cellular automata mapgen

(comment
"The basic idea is to fill the first map randomly, then repeatedly create new maps using the 4-5 rule:
a tile becomes a wall if it was a wall and 4 or more of its nine neighbors were walls,
or if it was not a wall and 5 or more neighbors were.
Put more succinctly, a tile is a wall if the 3x3 region centered on it contained at least 5 walls.
 Each iteration makes each tile more like its neighbors,
and the amount of overall 'noise' is gradually reduced: ")
; http://roguebasin.roguelikedevelopment.org/index.php?title=Cellular_Automata_Method_for_Generating_Random_Cave-Like_Levels

(defn- cellular-make-randmap [w h fillprob random]
  (create-grid w h (fn [_] (if (percent-chance fillprob random) :wall :ground))))

(defn- cellular-cached-get-adjacent-cells [grid cell]
  (if-let [result (:adjacent-cells @cell)]
    result
    (let [result (map grid (get-8-neighbour-positions (:posi @cell)))]
      (swap! cell assoc :adjacent-cells result)
      result)))

(defn- cellular-do-generation [grid wall-borders? generation]
  (let [k-access (if (zero? generation) :type (dec generation))]
    (doseq [[p cell] grid]
      (when-not (and wall-borders? (border-position? p grid))
        (swap! cell assoc generation
               (if (>
                     (count
                       (filter (fn [cell]
                                 (let [v @cell
                                       value (or (get v k-access) (get v :type))] ; border positions dont update their generations.. so access :type
                                   (or (= :wall value) (nil? value))))
                               (conj (cellular-cached-get-adjacent-cells grid cell) cell)))
                     5)
                 :wall
                 :ground))))))

; cool maps: 60/3
; a lot of islands: 70/3
; bigger islands 65/4
; even bigger islands 62/4
(defn cellular-automata-gridgen
  [w h & {:keys [fillprob generations wall-borders random]
          :or {random (java.util.Random.)}}]
  (let [randmap (cellular-make-randmap w h fillprob random)
        grid (if wall-borders
               (assoc-ks randmap (create-borders-positions randmap) :wall)
               randmap)
        grid (transform grid (fn [position value]
                               (atom {:type value
                                      :posi position})))]
    (dotimes [n generations]
      (cellular-do-generation grid wall-borders n))
    (transform grid (fn [p value] (let [v @value]
                                    (or
                                      (get v (dec generations))
                                      (get v :type))))))) ; border posis dont have generation key

;; regions/connecting

(defn flood-fill [grid start & {:keys [steps label]}]
  (loop [next-positions (if (coll? (first start)) start (list start)) ; collection of posis or just a posi
         labeled []
         labeled-ordered []
         grid grid]
    (if (and (seq next-positions)
             (not (and steps (= (count labeled-ordered) steps))))
      (recur (filter #(= (get grid %) :ground)
                     (distinct
                       (mapcat get-4-neighbour-positions next-positions)))
             (concat labeled next-positions)
             (conj labeled-ordered next-positions)
             (assoc-ks grid next-positions label))
      [grid labeled labeled-ordered])))


(defn calculate-regions [grid]
  (loop [next-positions (posis grid)
         grid grid
         label 0       ; does not need unique labels -> because if not ground is already unlabeled
         labeled-positions []]
    (if-not (seq next-positions)
      labeled-positions
      (let [posi (first next-positions)
            rest-posis (rest next-positions)]
        (if (= :ground (get grid posi))
          (let [[grid labeled _] (flood-fill grid posi :label label)]
            (recur rest-posis grid (inc label) (conj labeled-positions labeled)))
          (recur rest-posis grid label labeled-positions))))))

(defn- cellular-calc-path [[x1 y1] [x2 y2]]
  (concat
    (let [step (if (< x2 x1) -1 1)]
        (for [x (range (+ x1 step) x2 step)] ; + because starting step not part of path
          [x y1]))
      (let [step (if (< y2 y1) -1 1)] ; if no x movement then starting y step here inclusive at path
        (for [y (range y1 y2 step)]
          [x2 y]))))

(defn- cellular-pos-distance [[x1 y1] [x2 y2]]
  (Math/hypot (double (- x2 x1)) (double (- y2 y1))))

(defn- cellular-make-path [grid [a b] color]
  (assoc-ks grid (cellular-calc-path a b) color))

(defn- cellular-connect-two-close-regions [grid regions label]
  (let [regions (shuffle regions)
        posi-one (rand-nth (first regions))
        potential-points (map rand-nth (rest regions))
        posi-two (apply min-key #(cellular-pos-distance posi-one %) potential-points)]
    (cellular-make-path grid [posi-one posi-two] label)))

(defn rand-connect-two-regions [grid]
  (let [regions (calculate-regions grid)]
    (if-not (> (count regions) 1)
      grid
      (cellular-connect-two-close-regions grid regions white))))

(defn connect-regions [grid]
  (let [regions (calculate-regions grid)
        pairs (loop [regions (zipmap (range) (calculate-regions grid))
                     pairs []]
                (if (> (count regions) 1)
                  (let [label (rand-nth (keys regions))
                        posi-one (rand-nth (get regions label))
                        [closest-label posis] (apply min-key #(cellular-pos-distance posi-one (rand-nth (% 1)))
                                                     (dissoc regions label))]
                    (recur
                      (dissoc (update-in regions [label] concat posis) closest-label)
                      (conj pairs [posi-one (rand-nth posis)])))
                  pairs))]
    (reduce
      #(cellular-make-path %1 %2 :ground)
      grid pairs)))

;; nad = not-allowed-diagonals
; lead to problems:
; - melee attack goes through the wall because of range check only not wall-check
; - potential field generation must check for it; following must cut it
; - light shines through the edges

(defn- nad-corner? [grid [fromx fromy] [tox toy]]
  (and
    (= :ground (get grid [tox toy])) ; also filters nil/out of map
    (wall-at? grid [tox fromy])
    (wall-at? grid [fromx toy])))

(def ^:private nad-diagonal-steps [[-1 -1] [-1 1] [1 -1] [1 1]])

; TODO could be made faster because accessing the same posis oftentimes at nad-corner? check
(defn get-nads [grid]
  (loop [checkposis (filter (fn [{y 1 :as posi}]
                              (and (even? y)
                                   (= :ground (get grid posi))))
                            (posis grid))
         result []]
    (if (seq checkposis)
      (let [position (first checkposis)
            diagonal-posis (map #(mapv + position %) nad-diagonal-steps)
            nads (map (fn [nad] [position nad])
                      (filter #(nad-corner? grid position %) diagonal-posis))]
        (recur
          (rest checkposis)
          (doall (concat result nads)))) ; doall else stackoverflow error
      result)))

(defn- get-tiles-needing-fix-for-nad [grid [[fromx fromy]
                                           [tox toy]]]
  (let [xstep (- tox fromx)
        ystep (- toy fromy)
        cell1x (+ fromx xstep)
        cell1y fromy
        cell1 [cell1x cell1y]
        cell11 [(+ cell1x xstep) (+ cell1y (- ystep))]
        cell2x (+ cell1x xstep)
        cell2y cell1y
        cell2 [cell2x cell2y]
        cell21 [(+ cell2x xstep) (+ cell2y ystep)]
        cell3 [cell2x (+ cell2y ystep)]]
;    (println "from: " [fromx fromy] " to: " [tox toy])
;    (println "xstep " xstep " ystep " ystep)
;    (println "cell1 " cell1)
;    (println "cell11 " cell11)
;    (println "cell2 " cell2)
;    (println "cell21 " cell21)
;    (println "cell3 " cell3)
    (if-not (nad-corner? grid cell1 cell11)
      [cell1]
      (if-not (nad-corner? grid cell2 cell21)
        [cell1 cell2]
        [cell1 cell2 cell3]))))

(defn mark-nads [grid nads label]
  (assoc-ks grid (mapcat #(get-tiles-needing-fix-for-nad grid %) nads) label))

(defn fix-not-allowed-diagonals [grid]
  (mark-nads grid (get-nads grid) :ground))

;; populate / astar pathfinding

(defn- astar-pathposi-seq [^Path thepath]
  (map
    #(let [step (.getStep thepath %)]
       [(.getX step)
        (.getY step)])
    (range (.getLength thepath))))

(defn astar-find-path [start target grid pathblocked?]
  {:pre [(not (pathblocked? grid start))
         (not (pathblocked? grid target))]}
  (let [tilebasedmap (reify TileBasedMap
                       (getWidthInTiles [this] (width grid))
                       (getHeightInTiles [this] (height grid))
                       (pathFinderVisited [this x y] false)
                       (getCost [this context tx ty] (float 1))
                       (blocked [this context tx ty] (boolean (pathblocked? grid [tx ty]))))
        astarpathfinder (let [maxsearchdist (* (width grid) (height grid))
                              allowdiagmovement false]
                          (proxy [AStarPathFinder] [tilebasedmap maxsearchdist allowdiagmovement]))

        mover (reify Mover)
        [sx sy] start
        [tx ty] target]
    (astar-pathposi-seq (.findPath astarpathfinder mover sx sy tx ty))))


;;; populate

(defn get-rand-end-posi [grid start]
  (let [[_ _ labeled-ordered] (flood-fill grid start)]
    ; rest because start-posi should not marked as end-posi
    (rand-nth (high-weighted-rand-nth (rest labeled-ordered)))))

; TODO beim pfad genau nebeneinander die stuff-posis..
(defn get-populated-grid-posis [grid start-posi no-stuff]
  (let [end-posi (get-rand-end-posi grid start-posi)
        path-posis (set ; for checking.. if contains?
                     (astar-find-path start-posi end-posi grid wall-at?))
        [_ _ labeled-ordered] (flood-fill grid path-posis)
        stuff-posis (loop [labeled-ordered (rest labeled-ordered) ; dont put stuff directly on the path (first element)
                           stuff-cnt no-stuff
                           stuff-posis []]
                      (if (or (zero? stuff-cnt) (empty? labeled-ordered))
                        stuff-posis
                        (let [stuff-position (rand-nth (high-weighted-rand-nth labeled-ordered))
                              [_ posis _] (flood-fill grid stuff-position :steps 24) ; TODO flood-fill with 8 instead of 4 neighbours?
                              need-to-be-removed (set posis)
                              labeled-ordered (remove empty? (map (fn [posis]
                                                                    (remove #(contains? need-to-be-removed %) posis))
                                                                  labeled-ordered))]
                          (when (contains? path-posis stuff-position)
                            ; triggered at the end of a loop!
                            (println "stuff-position on path-posi - should not be !!"))
                          (recur labeled-ordered
                                 (dec stuff-cnt)
                                 (conj stuff-posis stuff-position)))))]
    {:end end-posi
     :path-posis path-posis
     :stuff-posis stuff-posis}))

;; geometry

(defn vec-posi
  "Takes Vector2f or [x y] and returns [x y]."
  [v]
  (if (instance? Vector2f v)
    [(.x ^Vector2f v)
     (.y ^Vector2f v)]
    v))

(defn rectangle
  ([[x y w h]]
   (rectangle x y w h))
  ([x y w h]
   (Rectangle. x y w h)))

(defn circle [[x y] radius]
  (Circle. x y radius))

(defn set-radius [circle pixels]
  (.setRadius ^Circle circle pixels))

(defn rotate-shape [shape angle x y]
  (.transform
   shape
   (Transform/createRotateTransform (Math/toRadians angle) x y)))

(defn vector2f
  ([x y]
   (Vector2f. x y))
  ([p]
   (if (instance? Vector2f p)
     p
     (let [[x y] p]
       (Vector2f. x y)))))

(defn scale-vector     [v n]   (.scale    ^Vector2f v n))
(defn normalise [v]     (.normalise ^Vector2f v))
(defn vector-add       [v1 v2] (.add ^Vector2f v1 ^Vector2f v2))
(defn vector-length    [v]     (.length ^Vector2f v))

(defn normalised? [v]
  (approx-numbers 1 (.length ^Vector2f v) 0.001))

(defn get-distance [p1 p2]
  (.distance ^Vector2f (vector2f p1) (vector2f p2)))

(defn get-normal-vectors [^Vector2f v]
  (let [x (.x v),y (.y v)]
    [(Vector2f. (- y) x)
     (Vector2f. y (- x))]))

(defn direction-vector [source target]
  {:post [(or (normalised? %) (zero? (.length ^Vector2f %)))]}
  (let [[sx sy] source
        [tx ty] target]
    (.normalise (Vector2f. (- tx sx) (- ty sy)))))

(defn get-angle-from-vector
  "converts theta of slick Vector2f to angle from top (top is 0 degree, moving right is 90 degree etc.)"
  [v]
  (let [angle (+ 90 (.getTheta ^Vector2f (vector2f v)))]
    (if (> angle 360)
      (- angle 360)
      angle)))

(defn vector-from-angle [angle]
  {:post [(normalised? %)]}
  (.add (Vector2f. 0 -1) (double angle)))

(defn collides? [^Shape a ^Shape b]
  (or (.intersects a b) (.contains a b) (.contains b a)))

(defn point-in-rect?
  ([posi [x y w h]]
    (point-in-rect? posi x y w h))
  ([[x y] rectx recty rectw recth]
    (point-in-rect? x y rectx recty rectw recth))
  ([x y rectx recty rectw recth]
    (and
      (>= x rectx)
      (>= y recty)
      (<= x (+ rectx rectw))
      (<= y (+ recty recth)))))

(defn rect-collision?
  "x,y centerposition."
  [[[x1 y1] half-w1 half-h1]
   [[x2 y2] half-w2 half-h2]]
  (and
    (let [x1 (float x1),x2 (float x2),half-w1 (float half-w1),half-w2 (float half-w2)]
      (< (Math/abs (- x1 x2)) (+ half-w1 half-w2)))
    (let [y1 (float y1),y2 (float y2),half-h1 (float half-h1),half-h2 (float half-h2)]
      (< (Math/abs (- y1 y2)) (+ half-h1 half-h2)))))

(defn tiles-inside-rect
  "x,y centerposition. tiles are integer positions. First tile is top left tile!"
  ; top left first ... because used in such a way @ prebuilt placement
  [[[x y] half-w half-h]]
  (let [topy (- y half-h)
        bottomy (+ y half-h)
        leftx (- x half-w)
        rightx (+ x half-w)
        top-tile (Math/floor topy)
        bottom-tile (Math/floor bottomy)
        left-tile (Math/floor leftx)
        right-tile (Math/floor rightx)]
    (for [x (range left-tile (inc right-tile)) ;include end-tile
          y (range top-tile (inc bottom-tile))]
      [(int x) (int y)]))) ; da sonst [3.0 4.0] z.B.

; noch schneller:
; test of xdist > range
; test ob ydist > range
; -> gar kein sqrd erstmal?
; TODO : ber�cksichtige gr�sse der bodies mit ein
(defn in-range?
  "collision of two circles."
  [[x1 y1] [x2 y2] range-square]
  (let [x1 (float x1)
        x2 (float x2)
        xdist (Math/abs (- x1 x2))
        xdist-sqr (* xdist xdist)]
    (if (> xdist-sqr range-square)
      false
      (let [y1 (float y1)
            y2 (float y2)
            ydist (Math/abs (- y1 y2))
            ydist-sqr (* ydist ydist)]
        (if (> ydist-sqr range-square)
          false
          (<= (+ xdist-sqr ydist-sqr) range-square))))))

(defn- get-touched-tiles-small
  "for <1 cell bodies, touching maximum 4 tiles"
  [[x y] half-w half-h]
  (let [x (float x)
        y (float y)
        half-w (float half-w)
        half-h (float half-h)
        r (int (+ x half-w))
        l (int (- x half-w))
        t (int (- y half-h))
        b (int (+ y half-h))]
    (distinct [[l t] [l b] [r t] [r b]])))

(defn- get-touched-tiles-big
  "iterates from top-left to bottom-right tile. works for >1 cell bodies"
  [[x y] half-w half-h]
  (let [x (float x)
        y (float y)
        half-w (float half-w)
        half-h (float half-h)
        r (int (+ x half-w))
        l (int (- x half-w))
        t (int (- y half-h))
        b (int (+ y half-h))]
    (distinct (for [x (range l (inc r))
                    y (range t (inc b))]
                [x y]))))

(defn does-not-fit-in-a-tile? [half-w half-h]
  (or
    (>= half-w 0.5)
    (>= half-h 0.5)))

(defn get-touched-tiles [posi half-w half-h]
  (if (does-not-fit-in-a-tile? half-w half-h)
    (get-touched-tiles-big posi half-w half-h)
    (get-touched-tiles-small posi half-w half-h)))

(defn add-or-subtract-angle
  "man will current in richtung new rotieren. muss man dazu add or subtract machen f�r den k�rzesten weg?
  returns add or subtract-function"
  [current-angle new-angle]
	(if (<= current-angle 180)
   (if (or (<= new-angle current-angle) (>= new-angle (+ 180 current-angle)))
     -
     +)
   (if (and (>= new-angle (- current-angle 180)) (<= new-angle current-angle))
     -
     +)))

(defn get-angle-to-position
  "Treat Position a the middle of the coordinate sytem and the angle is from the top
  so if target is right of origin -> 90� below origin -> 180�"
  [{originx 0 :as origin} {targetx 0 :as target}]
  (let [v (Vector2f. 0 -1)
        originv (vector2f origin)
        targetv (vector2f target)
        differencev (.normalise (.sub ^Vector2f targetv ^Vector2f originv))
        angle (int (Math/toDegrees (Math/acos (.dot v differencev))))]
     ; 0-180� ok -> target rechts von origin ansonsten adjust
    (if (< targetx originx)
      (- 360 angle)
      angle)))

(defn degrees? [n] (and (>= n 0) (<= n 360)))

(defn smallest-distance [a b]
	{:pre [(degrees? a) (degrees? b)]
	 :post [(degrees? %)]}
  (let [dist (Math/abs (float (- a b)))]
    (if (> dist 180) (- 360 dist) dist)))

(defn rotate-angle-to-angle [current-angle new-angle rotationspeed delta]
  (let [adjustment (* delta rotationspeed)]
    (if (>=
          adjustment
          (smallest-distance current-angle new-angle))
      new-angle
      (let [change-fn (add-or-subtract-angle current-angle new-angle)
            adjusted-angle (change-fn current-angle adjustment)]
        (cond
          (< adjusted-angle 0) (+ 360 adjusted-angle)
          (> adjusted-angle 360) (- adjusted-angle 360)
          :else adjusted-angle)))))

(defn degree-add [degree by] (rem (+ degree by) 360))

(defn degree-minus [degree by] (rem (+ 360 (- degree by)) 360))

(defn get-vector-to-mouse-coords
  ([]
    (get-vector-to-mouse-coords (get-mouse-pos)))
  ([posi]
    (direction-vector [half-screen-w half-screen-h] posi)))

;; prebuilt placement

(defn- prebuilt-is-5x5-walls-space? [center grid]
  (let [tiles (tiles-inside-rect [center 2 2])] ; room half width&height = 2 => 5x5
    (when (every? #(wall-at? grid %) tiles)
      tiles)))

(defn- prebuilt-flipy [data]
  (map #(vec (reverse %)) data))

(defn- prebuilt-rotate-grid-90-degrees [grid]
  (VectorGrid. (vec (reverse (apply map vector (.data ^VectorGrid grid))))))

(defn- prebuilt-rotate-grid-180-degrees [grid]
  (VectorGrid. (vec (reverse (prebuilt-flipy (.data ^VectorGrid grid))))))

(defn- prebuilt-rotate-grid-270-degrees [grid]
  (VectorGrid. (vec (prebuilt-flipy (apply map vector (.data ^VectorGrid grid))))))

(comment
  (let [grid (VectorGrid.[[1 4 7]
                          [2 5 8]
                          [3 6 9]])]
    (print-grid grid :print-cell print)
    (println)
    (print-grid (prebuilt-rotate-grid-90-degrees grid) :print-cell print)
    (println)
    (print-grid (prebuilt-rotate-grid-180-degrees grid) :print-cell print)
    (println)
    (print-grid (prebuilt-rotate-grid-270-degrees grid) :print-cell print)))

(defn is-5x5-walls-entrance?
  "returns false or a vector of [5x5-tiles open-entrance-tile center-tile rotatefn]"
  [[x y] grid]
  (let [a [x (- y 3)]
        b [x (+ y 3)]
        c [(- x 3) y]
        d [(+ x 3) y]]
    (if (wall-at? grid [x y])
      false
      (if-let [tiles (prebuilt-is-5x5-walls-space? a grid)]    ; 1. konfiguration => a => so wie das tiled file => eingang nach oben
        [tiles [x (dec y)] a identity]
        (if-let [tiles (prebuilt-is-5x5-walls-space? b grid)]
          [tiles [x (inc y)] b prebuilt-rotate-grid-180-degrees]
          (if-let [tiles (prebuilt-is-5x5-walls-space? c grid)]
            [tiles [(dec x) y] c prebuilt-rotate-grid-270-degrees]
            (if-let [tiles (prebuilt-is-5x5-walls-space? d grid)]
              [tiles [(inc x) y] d prebuilt-rotate-grid-90-degrees]
              false)))))))


;; entity components

(def id-entity-map (atom {}))

;; Component
; Component = map with :type. Special keys:
; :depends [:a :b :c] -> entity checks at creation if those components exist.
; :init, :destruct -> (fn [entity]) at creation / removal.

;; Entity

(defn- entity-dependency-ok? [component types]
  (if-let [dependencies (:depends component)]
    (every? #(some #{%} types) dependencies)
    true))

(defn- entity-dependencies-ok? [components]
  (let [types (map :type components)]
    (every? #(entity-dependency-ok? % types) components)))

(defn add-component [entity component]
  {:pre [component (:type component)]}
  (let [ctype (:type component)
        types (map :type (vals @entity))]
    (assert (not-any? #{ctype} types))
    (assert (entity-dependency-ok? component types))
    (swap! entity assoc-in [ctype] component)
    (when-apply (:init component) entity)
    entity))

(defn- entity-init! [entity]
  (swap! id-entity-map assoc (:id (meta entity)) entity)
  (doseq [c (vals @entity)]
    (when-let [init (:init c)]
      (init entity)))
  entity)

(defn create-entity-no-init [& components]
  (let [components (remove nil? components)]
    (assert (seq components))
    (assert (every? :type components))
    (assert (distinct-seq? (map :type components)))
    (assert (entity-dependencies-ok? components))
    (atom (zipmap (map :type components) components)
          :meta {:id (get-unique-number)})))

(defn create-entity [& components]
  (entity-init!
    (apply create-entity-no-init components)))

;; Removelist

(def ^:private entity-removelist (atom nil))

(defn- entity-munge-id [entity]
  (if (number? entity) entity (:id (meta entity))))

(defn add-to-removelist [entity]  ; arglist entity-or-id
  (swap! entity-removelist conj (entity-munge-id entity)))

(defn destruct-entity
  "do not call this while update-components is active - use add-to-removelist instead.
  because calling this while update-components is running could lead to NullPointerE"
  [entity]
  (when (and entity (get @id-entity-map (:id (meta entity))))
    (swap! id-entity-map dissoc (:id (meta entity)))
    (dorun (map #(when-apply (:destruct %) entity) (vals @entity)))))

(defn update-removelist []
  (dorun (map #(destruct-entity (get @id-entity-map %)) @entity-removelist))
  (reset! entity-removelist #{}))

(declare player-body)

(defn is-player? [entity]
  (= (:id (meta entity)) (:id (meta player-body))))

(defmacro active
  "Use: (active updatefn) or (active [delta component] fbody)
  Should be independent of update-order and changes will probably take effect in next frame
  because of snapshot order."
  [& args]
  `(let [f# ~(make-fn args)]
     (assert (fn? f#))
     {:updatefn f#}))

(defn- entity-get-active-component-types [mapentity]
  (map :type (filter :updatefn (vals mapentity))))

(defn block-active-components [entity]
  (add-blocks entity (entity-get-active-component-types entity)))

(defn unblock-active-components [entity]
  (remove-blocks entity (entity-get-active-component-types entity)))

(defn reset-component-state-after-blocked [entity]
  (doseq [{:keys [block-effect-reset-state] :as c} (vals @entity)
          :when block-effect-reset-state]
    (block-effect-reset-state entity c)))

;;

(comment
  (let [a (create-entity {:type :a :updatefn 1}
                         {:type :b :updatefn 1})]
    (println @a)
    (unblock-active-components
      (block-active-components @a))))

;;

(defn update-counter! [entity delta c & [counterkey & _]]
  (let [k (or counterkey :counter)
        counter (tick (k c) delta)]
    (swap! entity assoc-in [(:type c) k] counter)
    (:stopped? counter)))

;;

(defn- entity-save-single-session [entity]
  (when-let [{:keys [constructor args]} (:session @entity)]
    {:constructor constructor
     :args args
     :components (for [{:keys [type serialize] :as component} (vals @entity)
                       :when serialize]
                   [type (select-keys component serialize)])}))

(def entities-session
  (reify Session
    (save-session [_]
      (->> id-entity-map
           deref
           vals
           (keep entity-save-single-session)))

    (load-session [_ entity-saves]
      (doseq [{:keys [constructor args components]} entity-saves
              :let [f (resolve (symbol constructor))
                    entity (apply f args)]]
        (doseq [[ctype m] components]
          (swap! entity update-in [ctype] merge m))
        (entity-init! entity)))

    (new-session-data [_])))

;; -> all entities are loaded in the first-map currently and this will not work with rand generated maps
;; because the random generation would also be done with the same random seed so the same map is generated

;; order of initialisation ok? -> listeners befire entitiy add?
;;
;; also be careful with randomisation -> create-item-body with argument string creates a random image for the cyber implants for example!
;; properties INITIAL & COMPONENT SESSION SHOULD 100% re-create same values.
;; -> for entities that you want to save write down documentation for example input completely defines the state no randomisation (same item instance same img etc)

(def clear-entities-session (reify
                              Session
                              (load-session [_ _]
                                (reset! entity-removelist #{})
                                (swap! id-entity-map filter-map
                                       #(:ingame-loop-entity @%)))
                              (save-session [_])
                              (new-session-data [_])))

;; cell grid

(defn get-cell-grid [] (:cell-grid (get-current-map-data)))
(defn get-jcell-grid [] (force (:j-cell-grid (get-current-map-data))))

(defn create-cell [position blocks]
  {:pre [(#{#{}
            #{:ground}
            #{:ground :air}} blocks)]}
  (atom
    {:posi position
     :middle (translate-to-tile-middle position)
     :blocks blocks
     :body-ids #{}
     :occupied #{}}))

(defn occupied-by-other?
  "returns true if there is some solid body with center-tile = this cell
   or a multiple-cell-size body which touches this cell."
  [cell body]
  (seq (filter #(:solid (:body @%)) (disj (:occupied @cell) body))))

(defn cell-blocked? [cell movement-type] ; TODO cell-blocks?
  (or
    (not cell)
    (contains? (:blocks @cell) movement-type)))

(defn get-cell
  "returns nil when not existing cell (outside map bounds or undefined area (behind walls))"
  [posi]
  (get (get-cell-grid) (int-posi posi)))

(defn get-cells
  "see (doc get-cell). Use int-posis for this! Faster than (map get-cell posis)"
  [posis]
  (map (get-cell-grid) posis))

;TODO just use a delay/force?
(defn cached-get-adjacent-cells [cell]
  (if-let [result (:adjacent-cells @cell)]
    result
    (let [result (-> @cell :posi get-8-neighbour-positions get-cells)]
      (swap! cell assoc :adjacent-cells result)
      result)))

(def ^:private cell-jcell-movement-type :air)

(defn create-jcell-grid [grid]
  (let [j-cell-grid (make-array Boolean/TYPE (width grid) (height grid))]
    (doseq [[[x y] cell] grid]
      (aset j-cell-grid x y
            (boolean (cell-blocked? cell cell-jcell-movement-type))))
    j-cell-grid))

(defn get-map-w [] (width (get-cell-grid)))
(defn get-map-h [] (height (get-cell-grid)))

(defn inside-map? [tile] (get-cell tile))

(def ^:private cell-blocks-listeners (atom []))
(def cell-listeners-session (atom-session cell-blocks-listeners :save-session false))

(defn add-cell-blocks-changed-listener [f] (swap! cell-blocks-listeners conj f))

(defn cell-blocks-changed-update-listeners [] (dorun (map #(%) @cell-blocks-listeners))) ; TODO dorun/map eval ? memfn invoke

(defn change-cell-blocks
  "do not change to blocking while game running or bodies may be walled in.
   after changing update the listeners!!" ; vlt cell-seq ubergeben und danach wird hier update-listeners gecallt.
  [cell new-blocks]
  (swap! cell assoc-in [:blocks] new-blocks)
  (let [[x y] (:posi @cell)]
    (aset (get-jcell-grid) x y (boolean (cell-blocked? cell cell-jcell-movement-type))))) ; TODO same code as in create-jcell-grid

(defn get-body-ids [cell]
  (if cell (:body-ids @cell) #{}))

(defn- cell-get-body-ids-from-cells [cells]
  (distinct (mapcat get-body-ids cells)))

(defn get-bodies-from-cells [cells]
  (map #(get @id-entity-map %) (cell-get-body-ids-from-cells cells)))

;;

(defn- cell-contains-body? [cell body]
  (get (get-body-ids cell) (:id (meta body))))

(defn cell-add-body [cell body]
  {:pre [(not (cell-contains-body? cell body))]}
  (swap! cell update-in [:body-ids] conj (:id (meta body))))

(defn cell-remove-body [cell body]
  {:pre [(cell-contains-body? cell body)]}
  (swap! cell update-in [:body-ids] disj (:id (meta body))))

;;

; TODO use nad code?
(defn is-diagonal? [from to]
  (let [[fx fy] (:posi @from)
        [tx ty] (:posi @to)
        xdir (- tx fx)
        ydir (- ty fy)]
    (diagonal-direction? [xdir ydir])))

(def ^:private cell-allowed-gridkeys #{:undefined :ground :wall :airwalkable})

(defn create-grid-from-gen-grid
  "blockprops-map is a map of {:undefined :wall :ground} to the blocks like #{:wall :ground},
  can also be nil - then no cell is created."
  [grid blockprops-map calc-sprite-idx details-calc-sprite-idx]
  {:pre [(= cell-allowed-gridkeys (set (keys blockprops-map)))]}
  (let [ks (set (cells grid))]
    (assert (every? cell-allowed-gridkeys ks)
            (str "Allowed ks: " cell-allowed-gridkeys "\n and keys in grid: " ks)))
  (transform
    grid
    (fn [posi value]
      (when-let [blocks (get blockprops-map value)]
        (let [cell (create-cell posi blocks)]
          (swap! cell assoc
                 :sprite-posi (calc-sprite-idx grid posi value)
                 :details-sprite-posi (when details-calc-sprite-idx
                                        (details-calc-sprite-idx grid posi value)))
          cell)))))

;; spawn spaces

(defn- spawn-ground?
  "used so that get-spawn-spaces / get-spawn-space works for randgen-maps and realized maps"
  [cell]
  (if cell
    (if (keyword? cell)
      (= :ground cell)
      (not (cell-blocked? cell :ground)))))

(defn- spawn-is-3x3-space? [grid position]
  (every? spawn-ground? (get-3x3-cellvalues grid position)))

(defn- spawn-get-rand-3x3-space [quadranten-posis grid]
  (when quadranten-posis
    (first
      (filter #(spawn-is-3x3-space? grid %) (shuffle quadranten-posis)))))

(defn- get-spawn-spaces
  "partitions the grid in fields and tries to find a 'space' in every field."
  [grid]
  (let [field-w 13 ; displ-w-in-tiles
        field-h 10 ; displ-h-in-tiles
        get-field-of-posi (fn [[x y]] [(int (/ x field-w))
                                       (int (/ y field-h))])
        ; grid that tells for every position in original grid the field-position index
        field-posis-grid (create-grid (width grid) (height grid) get-field-of-posi)
        ; now create for every cell/quadrant in the fields the seq of positions
        quadranten-posis (atom {})]
    (doseq [[posi quadrant] field-posis-grid]
      (swap! quadranten-posis update-in [quadrant] conj posi))
    (remove nil?
      (map #(spawn-get-rand-3x3-space % grid)
        (vals @quadranten-posis)))))

(defn get-spawn-positions-groups
  "Returns a coll of positions and their adjacent-positions == 9 positions every item"
  [grid]
  (map #(cons % (get-8-neighbour-positions %)) (get-spawn-spaces grid)))

(defn mark-spawn-spaces [grid]
  (assoc-ks grid (get-spawn-spaces grid) :room))

(defn mark-3x3-spaces [grid]
  (let [rooms (filter #(spawn-is-3x3-space? grid %) (posis grid))]
    (assoc-ks grid rooms :room)))

;; graphics

(defn- graphics-center-shape [^Shape shape [x y]]
  (doto shape
    (.setCenterX x)
    (.setCenterY y)))

(defn draw-shape [^Graphics g shape]
  (.draw g shape))

(defn render-centered-shape
  ([g shape posi]
   (render-centered-shape g shape posi white))
  ([g shape posi color]
   (graphics-center-shape shape posi)
   (set-color g color)
   (draw-shape g shape)))

(defn fill-rect
  ([g [x y w h] color]
    (fill-rect g x y w h color))
  ([^Graphics g x y w h color]
    (set-color g color)
    (.fillRect g x y w h)))

(defn draw-rect
  ([g [x y w h] color]
    (draw-rect g x y w h color))
  ([^Graphics g x y w h color]
    (set-color g color)
    (.drawRect g x y w h)))

(defn draw-string [^Graphics g x y s]
  (.drawString g (str s) x y))

(defn- graphics-no-of-lines
  "for example lineheight 20 and textheight 1-20 -> 1 line; 21-40 -> 2 lines etc."
  [txtheight lineh]
  (int (Math/ceil (/ txtheight lineh))))

; workaround for drawstring does not detect \newline
; => seperate for the right number of newlines
; for example        ["abc\ncdef" :x "123" :a "4\n\n3"]
; needs to look like ["abc" "cdef" :x "123" :a "4" "" "3"]
; multiple \n\n in a row (at the end of the string) will not be detected by split-lines but it is okay
; because they will be counted in the height and not rendered anything in it anyway
; only problem: "Test\n\n" will be converted to "Test" with split-lines ...
; (partition-by #(= \newline %) "abc\n\n") also doesnt work...
(defn- graphics-seperate-newline-strings [coll]
  (reduce #(if (coll? %2) (vec (concat %1 %2)) (conj %1 %2))
          []
          (map #(if (string? %) (str/split-lines %) %) coll)))

(defn- graphics-render-colored-text [^Graphics g x y & colors-and-more]
  (let [lineh (get-line-height)
        y (atom y)
        colors-and-more (graphics-seperate-newline-strings ; draw-string of spritesheetfont does not understand \newline so we have to seperate them manually
                          (remove nil? colors-and-more))] ; we dont want newlines for nils
    (doseq [elem colors-and-more]
      (if (instance? Color elem)
        (.setColor g elem)
        (let [originals (str elem)
              s (apply str (filter allowed-characters originals))]
          (when-seq [not-allowed (remove allowed-characters originals)]
            (println "Characters not in font file: " (pr-str not-allowed) " of string: " (str elem)))
          (draw-string g x @y s)
          (swap! y + (* lineh
                        (graphics-no-of-lines (get-text-height originals) lineh)))))))) ; height of originals because \newlines would be filtered out

(defn- graphics-get-readable-renderx
  "if string goes out of screen-width bounds it is shifted to the left."
  [string x]
  (let [xpuffer (- (get-screen-width)
                   (+ x (get-text-width string)))]
    (if (neg? xpuffer) (- x (- xpuffer)) x)))

(defn- graphics-get-readable-rendery
  "if string goes out of screen-height bounds it is shifted to the top."
  [string y]
  (let [ypuffer (- (get-screen-height)
                   (+ y (get-text-height string)))]
    (if (<= ypuffer 0) (- y (- ypuffer)) y)))

; TODO shift and background set default to false because is not expected when programming gui texts...
(defn render-readable-text
  "textcolorseq consists of colors and the rest is str-ed. dont use keywords because of destructuring!"
  [g x y & args]
  (let [[{:keys [centerx centery bigfont above shift background]
          :or {shift true background true}}
         textcolorseq] (split-kvs-and-more args)
        whole-text (->> textcolorseq
                     (remove #(instance? Color %))
                     (remove nil?) ; so there will be no "\n" interposed between empty lines... (render-colored-text does not increse lineh with empty lines)
                     (map str)
                     (interpose "\n")
                     (apply str))
        _ (.setFont ^Graphics g (if bigfont (get-defaultfont) (get-defaultfont)))
        w (inc (get-text-width whole-text))
        h (get-text-height whole-text)
        x (if-not centerx x (- x (/ w 2))) ; before shifting calculate centered x/y
        y (if-not centery y (- y (/ h 2)))
        y (if-not above y (- y (get-text-height whole-text)))
        x (if shift (graphics-get-readable-renderx whole-text x) x)
        y (if shift (graphics-get-readable-rendery whole-text y) y)
        x (int x)
        y (int y)]
    (when background
      (fill-rect g x y w h transparent-black))
    (set-color g white)
    (apply graphics-render-colored-text g x y textcolorseq)
    (reset-font g)))

(defn fill-centered-circle [^Graphics g radius position color]
  (set-color g color)
  (let [^Shape shape (circle [-1 -1] radius)]
    (graphics-center-shape shape position)
    (.fill g shape)))

(defn draw-grid
  "Grid lines start at top left corner."
  [^Graphics g leftx topy gridw gridh cellw cellh]
  (let [w (* gridw cellw)
        h (* gridh cellh)
        buttomy (+ topy h)
        rightx (+ leftx w)]
    (doseq [idx (range (inc gridw))
            :let [linex (+ leftx (* idx cellw))]]
      (.drawLine g linex topy linex buttomy))
    (doseq [idx (range (inc gridh))
            :let [liney (+ topy (* idx cellh))]]
      (.drawLine g leftx liney rightx liney))))

(defn draw-line
  ([g [sx sy] [ex ey]]
   (draw-line g sx sy ex ey))
  ([^Graphics g x y ex ey]
   (.drawLine g x y ex ey)))

(defn graphics-translate [^Graphics g x y]
  (.translate g x y))

(defn reset-transform [^Graphics g]
  (.resetTransform g))

;; camera

(defn get-camera-position
  "returns the current center-of-screen-map-tile-position.
Rendering the map, minimap, map-entities depends on the camera position"
  []
  (:value (:position @player-body)))

;; tilemap

(defn tilepos-of-screenpos [[x y]]
  (let [[middlex middley] (get-camera-position)
        dist-to-middle-x (- x half-screen-w)
        dist-to-middle-y (- y half-screen-h)
        tile-x (+ middlex (/ dist-to-middle-x tile-width))
        tile-y (+ middley (/ dist-to-middle-y tile-height))]
    [tile-x tile-y]))

(defn screenpos-of-tilepos [[x y]]
    (let [[middlex middley] (get-camera-position)
          dist-to-middle-x (- x middlex)
          dist-to-middle-y (- y middley)
          screen-x (+ half-screen-w (* dist-to-middle-x tile-width))
          screen-y (+ half-screen-h (* dist-to-middle-y tile-height))]
      [screen-x screen-y]))

(defn get-mouse-tile-pos []
  (tilepos-of-screenpos (get-mouse-pos)))

(defn mouse-int-tile-pos []
  (int-posi (get-mouse-tile-pos)))

;; contentfields

; Contentfield Entities
; -> :position sollten sie haben
; Ansonsten :updatefn / :renderfn ansonsten ist sinnlos sie dazuzufugen.
; TODO entities dont save a contenfield in their :position component but just the idx (for printing..), also simpler here?
(let [field-w 14 ; in tiles
      field-h 10]

  (defn create-mapcontentfields [w h]
    (create-grid (inc (int (/ w field-w))) ; inc wegen randern
                      (inc (int (/ h field-h)))
                      (fn [idx]
                        {:idx idx, :entities (atom #{})})))

  (defn- contentfield-idx-of-position [[x y]]
    [(int (/ x field-w))
     (int (/ y field-h))]))

(defn- contentfield-get-fields [] (:contentfields (get-current-map-data)))

(defn get-content-field [entity] (:content-field (:position @entity)))

(defn remove-entity-from-content-field [entity] (swap! (:entities (get-content-field entity)) disj entity))

(defn put-entity-in-correct-content-field [entity]
  (let [old-field (get-content-field entity)
        new-field (get (contentfield-get-fields)
                       (contentfield-idx-of-position (:value (:position @entity))))]
    (when-not (= old-field new-field)
      (swap! (:entities new-field) conj entity)
      (swap! entity assoc-in [:position :content-field] new-field)
      (when old-field
        (swap! (:entities old-field) disj entity)))))

(defn get-player-content-field-idx [] (:idx (get-content-field player-body)))

(defn get-entities-in-active-content-fields
  "of current map"
  []
  (mapcat #(deref (:entities %)); (comp deref :entities) or #(... %) ?
    (remove nil?
            (map (contentfield-get-fields)  ; keep (get-contentfields)  ?  also @ potential field thing
                 (let [idx (get-player-content-field-idx)]
                   (cons idx (get-8-neighbour-positions idx)))))))

(defn get-all-entities-of-current-map []
  (mapcat #(deref (:entities %)) (cells (contentfield-get-fields))))

;; ingame-loop

(def ^:private ingame-loop-ids (atom #{}))

(defn ingame-loop-comp [& args]
  (create-entity
    {:type :ingame-loop-entity
     :init     #(swap! ingame-loop-ids conj (:id (meta %)))
     :destruct #(swap! ingame-loop-ids disj (:id (meta %)))}
    (apply merge {:type (first args)} (rest args))))

(defn get-ingame-loop-entities []
  (map #(get @id-entity-map %) @ingame-loop-ids))

(defmacro do-in-game-loop [& expr]
  `(ingame-loop-comp :temporary
     (active [delta# c# entity#]
       ~@expr
       (add-to-removelist entity#))))

(defn remove-ingame-loop-entity [ctype]
  (->>
    (get-ingame-loop-entities)
    (first (filter #(ctype @%)))
    add-to-removelist))

;; tiledmaps

(defn construct-tiledmap
  ([file]
   (TiledMap. ^String file))
  ([file lightmanager]
   (TiledMap. ^String file ^LightManager lightmanager)))

(defn get-layer-index [tiled-map layername]
  (let [idx (.getLayerIndex ^TiledMap tiled-map layername)]
    (when-not (= -1 idx) idx)))

; Conversion of TiledMap to cell-grid

(defn- tiled-get-blocks-property [^TiledMap tiled-map [x y] layer-index]
  (let [gid (.getTileId tiled-map x y layer-index)
        undefined (zero? gid)]
    (when-not undefined ; no tile placed at this location
      (case (.getTileProperty tiled-map gid "walkable" "default")
        "air" :airwalkable
        "ground" :ground
        "default" :wall))))

(defn create-grid-from-tiled-map [^TiledMap tiled-map]
  (let [ground-layer-idx (get-layer-index tiled-map "ground")
        details-layer-idx (get-layer-index tiled-map "details")]
    (create-grid (.getWidth tiled-map)
                 (.getHeight tiled-map)
                 (fn [p]
                   (or
                     (when details-layer-idx
                       (tiled-get-blocks-property tiled-map p details-layer-idx))
                     (tiled-get-blocks-property tiled-map p ground-layer-idx))))))

(defn create-cell-grid-from-tiled-map [tiled-map]
  (transform (create-grid-from-tiled-map tiled-map)
             (fn [p value]
               (when value
                 (create-cell p (case value
                                  :ground #{}
                                  :wall #{:air :ground}
                                  :airwalkable #{:ground}))))))

;;

; .getTileProperty returns The value assigned to the property on the tile (or the default value if none is supplied)
(defn get-tile-property-value [[x y] ^TiledMap tiled-map layer-idx property]
  (let [gid (.getTileId tiled-map x y layer-idx)
        value (.getTileProperty tiled-map gid property "def")]
    (when-not (= "def" value)
      value)))

(defn get-tileproperties [^TiledMap tiled-map layername property]
  (when-let [idx (get-layer-index tiled-map layername)]
    (for [x (range (.getWidth tiled-map))
          y (range (.getHeight tiled-map))
          :let [value (get-tile-property-value [x y] tiled-map idx property)]
          :when value] ; dont use when-let because then nil's will be in the result sequence
      [[x y] value])))

(defn get-player-entity-start-position [tiled-map]
  (let [tile-values (get-tileproperties tiled-map "entities" "player")]
    (cond (empty? tile-values) (throw (Error. "Player start position not found in \"entities\" layer of tiledmap."))
          (= (count tile-values) 1) (first (first tile-values))
          :else (throw (Error. "Multiple player start positions found in \"entities\" layer of tiledmap.")))))

;; misc components

(defn delete-after-duration-component [duration & {:keys [duration-over]}]
  (safe-merge
    {:type :delete-after-duration
     :counter (make-counter duration)
     :serialize [:counter]}
    (active [delta c entity]
      (when (update-counter! entity delta c)
        (add-to-removelist entity)
        (when duration-over
          (duration-over entity))))))

;;

(defn set-rotation-angle [entity angle] ; TODO has rotation component
  (swap! entity assoc-in [:rotation :angle] angle))

(defn- misc-rotate-to-vector [body v]
  (set-rotation-angle body (get-angle-from-vector v)))

(defn rotate-to-body [a b]
  (misc-rotate-to-vector a (direction-vector (:value (:position @a)) (:value (:position @b)))))

(defn rotate-to-player [body]
  (rotate-to-body body player-body))

(defn rotate-to-mouse [body]
  (misc-rotate-to-vector body (get-vector-to-mouse-coords)))

; bodies mit verschiedener w/h lieber nicht rotieren da die body-collision shape nicht mit rotiert.
; also rotation nur bei bodies mit gleicher w/h da sie dann in ihrer collision shape drinbleiben
(defn rotation-component []
  {:type :rotation
   :init #(assert (= (:half-width (:body @%)) (:half-height (:body @%))))
   :moved misc-rotate-to-vector
   :angle 0})

;;

(defn- misc-regenerate [data delta percent-reg-per-second]
  (increase-min-max-val data
                        (->
                          percent-reg-per-second
                          (/ 100)         ; percent -> multiplier
                          (* (:max data)) ; in 1 second
                          (/ 1000)        ; in 1 ms
                          (* delta))))

(defn regeneration-component [ctype ks percent-reg-per-second]
  (merge {:type ctype
          :reg-per-second percent-reg-per-second}
         (active [delta component entity]
           (swap! entity update-in ks misc-regenerate delta (:reg-per-second component)))))

(defn hp-regen-component [percent-reg-per-second]
  (regeneration-component :hp-regen [:destructible :hp] percent-reg-per-second))

(defn mana-regen-component [percent-reg-per-second]
  (regeneration-component :mana-regen [:skillmanager :mana] percent-reg-per-second))

;; animation

(defprotocol Animation
  (animation-stopped?  [_])
  (restart-animation   [_])
  (get-animation-duration [_])
  (get-animation-frame    [_]))

(defrecord ImmutableAnimation [frames frame-duration looping speed cnt maxcnt]
  Updateable
  (tick [this delta]
    (let [newcnt (+ cnt (* speed delta))]
      (assoc this :cnt (cond (< newcnt maxcnt) newcnt
                             looping           (min maxcnt (- newcnt maxcnt))
                             :else             maxcnt))))
  Animation
  (animation-stopped? [_]
    (and (not looping) (= cnt maxcnt)))
  (restart-animation [this]
    (assoc this :cnt 0))
  (get-animation-duration [_]
    maxcnt)
  (get-animation-frame [this]
    ; int because speed can make delta a float value.
    (get frames (int (quot (dec cnt) frame-duration)))))

(def default-frame-duration 33)

(defn create-animation [frames & {:keys [frame-duration looping]
                                  :or {frame-duration default-frame-duration looping false}}]
  (map->ImmutableAnimation
    {:frames (vec frames)
     :frame-duration frame-duration
     :looping looping
     :speed 1
     :cnt 0
     :maxcnt (* (count frames) frame-duration)}))

(defn render-centered-animation [animation position]
  (render-centered-image (get-animation-frame animation) position))

;; assets

(def ^:private assets-debug false)
(def ^:private assets-already-printed (atom #{}))

(defn- assets-debug-print-result [folder prefix result]
  (when assets-debug
    (when-not (contains? @assets-already-printed [folder prefix])
      (swap! assets-already-printed conj [folder prefix])
      (println "\n" [folder prefix] " : " result))))

(defn- assets-filter-prefix-and-sort [names prefix]
  (sort (if prefix
          (filter #(.startsWith ^String % prefix) names)
          names)))

; http://stackoverflow.com/questions/1429172/list-files-inside-a-jar-file
(defn- assets-get-jar-entries [filter-predicate]
  (let [src (.getCodeSource (.getProtectionDomain game.utils.RayCaster))
        zip (ZipInputStream. (.openStream (.getLocation src)))]
    (loop [entry (.getNextEntry zip)
           hits []]
      (if entry
        (recur (.getNextEntry zip)
               (let [entryname (.getName entry)]
                 (if (filter-predicate entryname)
                   (conj hits entryname)
                   hits)))
        hits))))

; TODO  only do with jar file?
; (log "getting *.png jar entries...")
(let [all-png-entries (assets-get-jar-entries #(.endsWith ^String % ".png"))]

  (defn get-sorted-pngs-in-jar [folder & {prefix :prefix}]
    (let [hits (filter #(.startsWith ^String % folder) all-png-entries)
          names (map #(subs % (inc (.lastIndexOf ^String % "/"))) hits)
          result (assets-filter-prefix-and-sort names prefix)]
      (assert (apply = (map #(.lastIndexOf ^String % "/") hits)))
      (assets-debug-print-result folder prefix result)
      result)))

"
entryname:
data/player/shooting/shoot0.png
data/player/shooting/shoot1.png
data/player/shooting/raw/weapon.png

startswith: data/player/shooting/
endswith: .png

result includes weapon.png

how to filter /raw ?
-> filter out if between starts and ends is another slash / ?
assert lastindexOf slash is the same for names in a folder?
"


(defn- assets-get-sorted-pngs [folder & {prefix :prefix}]
    (let [file (File. ^String (str "resources/" folder))
          listed-files (.listFiles file)
          pngs (filter #(.endsWith (.getPath ^File %) ".png") listed-files)
          names (map #(.getName ^File %) pngs)
          result (assets-filter-prefix-and-sort names prefix)]
      (assets-debug-print-result folder prefix result)
      result))

(def get-pngs (if jar-file? get-sorted-pngs-in-jar assets-get-sorted-pngs))

; TODO use & more for image arguments like :transparent , so for example :scale also becomes possible
(defn folder-frames [folder & {:keys [transparent prefix]}]
  (doall ; for pre-loading
    (map
      #(create-image (str folder %) :transparent transparent)
      (get-pngs folder :prefix prefix))))

(defn folder-animation
  "duration is duration of all frames together, will be split evenly across frames."
  [& {:keys [folder looping duration transparent prefix]}]
  (let [frames (folder-frames folder :transparent transparent :prefix prefix)]
    (create-animation frames
                      :frame-duration (if duration
                                        (int (/ duration (count frames)))
                                        default-frame-duration)
                      :looping looping)))

(defn spritesheet
  ([file tilew tileh]
    (SpriteSheet. ^String file (int tilew) (int tileh)))
  ([file tilew tileh more]
    (SpriteSheet. ^String file (int tilew) (int tileh) ^Color more)))

(defn get-sprite [^SpriteSheet sheet [x y]]
  (.getSprite sheet x y))

(defn- assets-get-sheet-frames [^SpriteSheet sheet]
  (for [y (range (.getVerticalCount sheet))
        x (range (.getHorizontalCount sheet))]
    (get-sprite sheet [x y])))

(defn spritesheet-frames [& more]
  (assets-get-sheet-frames (apply spritesheet more)))

;; position component

(defn position-component [p]
  {:type :position
   :value p
   :serialize [:value]
   :init put-entity-in-correct-content-field
   :destruct remove-entity-from-content-field
   :posi-changed put-entity-in-correct-content-field})

(defn get-entity-tile [entity]
  (int-posi (:value (:position @entity))))

(defn swap-position! [entity posi & {filter-body :filter-body}]
  (swap! entity assoc-in [:position :value] posi)
  (doseq [c (vals @entity)
          :when (not
                  (and
                    filter-body
                    (= :body (:type c))))]
    (when-apply (:posi-changed c) entity)))

;; raycast

; mit jcell-grid weil performanter als andere probierte losung: tilebasedmap zugriff auf get-cell bei jedem call to blocked.
; 10x so schnell mit jcell-grid
; Annahme: cell-grid andert net wahrend einer ray-blocked? anfrage; deswegen auch jcell-grid

(defn ray-blocked?
  ([start-x start-y target-x target-y]
    (RayCaster/rayBlocked start-x start-y target-x target-y (get-map-w) (get-map-h) (get-jcell-grid)))
  ([p1 p2]
    (let [[x1 y1] (vec-posi p1)
          [x2 y2] (vec-posi p2)]
      (ray-blocked? x1 y1 x2 y2))))

(defn ray-maxsteps
  [start-x start-y vector-x vector-y max-steps]
  (let [erg (RayCaster/castMaxSteps start-x start-y vector-x vector-y (get-map-w) (get-map-h) (get-jcell-grid) max-steps)]
    (if (= -1 erg) :not-blocked erg)))

(defn- raycast-create-double-ray-endpositions
  "path-w in tiles."
  [start-x,start-y,target-x,target-y,path-w]
  {:pre [(< path-w 0.98)]} ; wieso 0.98??
  (let [path-w (+ path-w 0.02) ;etwas grosser damit z.b. projektil nicht an ecken anstosst
        v (direction-vector [start-x start-y] [target-y target-y])
        [normal1 normal2] (get-normal-vectors v)
        normal1 (scale-vector normal1 (/ path-w 2))
        normal2 (scale-vector normal2 (/ path-w 2))
        start1  (vector-add (vector2f start-x  start-y)  normal1)
        start2  (vector-add (vector2f start-x  start-y)  normal2)
        target1 (vector-add (vector2f target-x target-y) normal1)
        target2 (vector-add (vector2f target-x target-y) normal2)]
    [start1,target1,start2,target2]))

(defn is-path-blocked?
  "path-w in tiles. casts two rays."
  ([start-x,start-y,target-x,target-y,path-w]
    (let [[start1,target1,start2,target2] (raycast-create-double-ray-endpositions start-x start-y target-x target-y path-w)]
      (is-path-blocked? start1 target1 start2 target2)))
  ([start1,target1,start2,target2]
    (or
      (ray-blocked? start1 target1)
      (ray-blocked? start2 target2))))

(defn- raycast-steplist
  [start-x start-y target-x target-y]
  (seq
    (RayCaster/castSteplist start-x start-y target-x target-y (get-map-w) (get-map-h) (get-jcell-grid))))

; STEPLIST TEST

(def raycast-current-steplist (atom nil))

(defn raycast-steplist-contains? [tilex tiley]
  (some
    (fn [[x y]]
      (and (= x tilex) (= y tiley)))
    @raycast-current-steplist))

(defn- raycast-render-line-middle-to-mouse [g color]
  (set-color g color)
  (let [[x y] (get-mouse-pos)]
    (draw-line g half-screen-w half-screen-h x y)))

(defn update-test-raycast-steplist []
  (let [[start-x start-y] (:value (:position @player-body))
        [target-x target-y] (get-mouse-tile-pos)
        steplist (map
                   (fn [step]
                     [(.x step) (.y step)])
                   (raycast-steplist start-x start-y target-x target-y))]
    (reset! raycast-current-steplist steplist)))

;; MAXSTEPS TEST

(def raycast-current-steps (atom nil))

(defn update-test-raycast-maxsteps []
  (let [[start-x start-y] (:value (:position @player-body))
        [target-x target-y] (get-mouse-tile-pos)
        vector-x (- target-x start-x)
        vector-y (- target-y start-y)
        maxsteps 10
        steps (ray-maxsteps start-x start-y vector-x vector-y maxsteps)]
    (reset! raycast-current-steps steps)))

(defn draw-test-raycast [g]
  (let [[start-x start-y] (:value (:position @player-body))
        [target-x target-y] (get-mouse-tile-pos)
        color (if (ray-blocked? start-x start-y target-x target-y)
                red
                green)]
    (raycast-render-line-middle-to-mouse g color)))

; PATH BLOCKED TEST

(defn draw-test-path-blocked [g]
  (let [[start-x start-y] (:value (:position @player-body))
        [target-x target-y] (get-mouse-tile-pos)
        [start1 target1 start2 target2] (mapv vec-posi
                                               (raycast-create-double-ray-endpositions start-x start-y target-x target-y 0.4))
        [start1screenx,start1screeny] (screenpos-of-tilepos start1)
        [target1screenx,target1screeny] (screenpos-of-tilepos target1)
        [start2screenx,start2screeny] (screenpos-of-tilepos start2)
        [target2screenx,target2screeny] (screenpos-of-tilepos target2)]
    (set-color g (if (is-path-blocked? start1 target1 start2 target2) red green))
    (doto g
      (draw-line start1screenx start1screeny target1screenx target1screeny)
      (draw-line start2screenx start2screeny target2screenx target2screeny))))

;; minimap

(defn- minimap-get-image  [] (:minimap-image  (get-current-map-data)))
(defn- minimap-get-bodies [] (:minimap-bodies (get-current-map-data)))

(let [alpha 0.9]
  (def ^:private minimap-colors
    (mapvals
      {:wall   [:r 1 :g 1 :b 1 :a 1]
       :ground [:r 0.5 :g 0.5 :b 0.5 :a alpha]
       :water  [:r 0.6 :g 0.6 :b 0.6 :a alpha]}
      #(apply rgbcolor %))))
; where is unexplored color defined?
; wall color chosen as light so you can spot frontiers between ground and unexplored quicker

(defn create-minimap-image [w h] (Image. (int w) (int h) Image/FILTER_NEAREST))

(defn- minimap-draw-on-image [posicolorseq]
  (let [g (.getGraphics ^Image (minimap-get-image))] ; TODO .getGraphics expensive?
    (doseq [[[x y] color] posicolorseq]
      (draw-rect g x y 0 0 color))
    (.flush g)))

(defn show-on-minimap [color] ; TODO component?
  {:type :minimap-icon
   :color color
   :depends [:body]})

(def ^:private minimap-scale 1)

(defn- minimap-render-bodies [g x y start-leftx start-topy width-in-tiles height-in-tiles]
  (doseq [body (reset! (minimap-get-bodies) (set/select #(get @id-entity-map (:id (meta %))) @(minimap-get-bodies))) ; swap?
          :let [{bodyx 0 bodyy 1 :as body-posi} (int-posi (:value (:position @body)))
                body-render-x (+ x (* minimap-scale (- bodyx start-leftx)))
                body-render-y (+ y (* minimap-scale (- bodyy start-topy)))]
          :when (point-in-rect? body-posi start-leftx start-topy width-in-tiles height-in-tiles)]
    (fill-rect g (- body-render-x 0.5) (- body-render-y 0.5) 2 2 (:color (:minimap-icon @body)))))

(defn- minimap-cut-sub-image
  "out of image borders the image repeats itself, which shows on small maps, so dont create a sub-image which shows parts outside of image bounds"
  [^Image image start-leftx start-topy width-in-tiles height-in-tiles shift-right shift-down]
  (let [[imgw imgh] (get-dimensions image)
        subimage-leftx (max 0 start-leftx)
        subimage-topy (max 0 start-topy)
        subimage-width (- width-in-tiles shift-right)
        subimage-height (- height-in-tiles shift-down)
        subimage-width (if (> (+ subimage-leftx subimage-width)
                           imgw)
                         (- imgw subimage-leftx)
                         subimage-width)
        subimage-height (if (> (+ subimage-topy subimage-height)
                               imgh)
                          (- imgh subimage-topy)
                      subimage-height)]
    (get-sub-image image subimage-leftx subimage-topy subimage-width subimage-height)))

(defn render-minimap [g]
  (let [puffer 2
        x puffer
        y puffer
        half-w-tiles (int (/ (- half-screen-w puffer) minimap-scale))
        half-h-tiles (int (/ (- half-screen-h puffer) minimap-scale))
        width-in-tiles (* 2 half-w-tiles)
        height-in-tiles (* 2 half-h-tiles)
        px-width (* minimap-scale width-in-tiles)
        px-height (* minimap-scale height-in-tiles)
        [camerax cameray] (int-posi (get-camera-position))
        start-leftx (- camerax half-w-tiles)
        start-topy (- cameray half-h-tiles)
        shift-right (if (neg? start-leftx) (Math/abs (float start-leftx)) 0)
        shift-down (if (neg? start-topy) (Math/abs (float start-topy)) 0)
        subimg (minimap-cut-sub-image (minimap-get-image) start-leftx start-topy width-in-tiles height-in-tiles shift-right shift-down)
        subimage (get-scaled-copy subimg minimap-scale)]
    (fill-rect g x y px-width px-height (rgbcolor :r 1 :g 1 :b 1 :a 0.3))
    (draw-image subimage (+ x (* minimap-scale shift-right)) (+ y (* minimap-scale shift-down)))
    (draw-rect g x y px-width px-height gray)
    (minimap-render-bodies g x y start-leftx start-topy width-in-tiles height-in-tiles)))

(defn update-minimap [sensed-tiles]
  (let [sensed-cells (get-cells sensed-tiles)]
    (swap! (minimap-get-bodies) set/union (set (filter #(:minimap-icon @%) (get-bodies-from-cells sensed-cells))))
    (minimap-draw-on-image (for [{:keys [posi blocks]} (map deref (remove nil? sensed-cells))]
                     [posi (case blocks
                             #{:air :ground} (:wall minimap-colors)
                             #{:ground} (:water minimap-colors)
                             #{} (:ground minimap-colors))]))))

;; lightning

(def ^:private lightning-entity-ambient 0.4)
(def ^:private lightning-tile-ambient 0.4)
(def ^:private lightning-explored-tile-color (rgbcolor :r lightning-tile-ambient :g lightning-tile-ambient :b lightning-tile-ambient))

; Currently only light sources of player allowed because
; - lightmap is simultaneously exploring (would need a check is this player light source or something else)
; - other light sources would be visible even if not in line of sight of player
;           => only tile-corners in line of sight of player are brightened up by light sources
;           => we are already calculating these visible tile corners with the lightmap but only in radius
;                 entities are already checked for line of sight
;            => neat trick: light radius of player is screen width/height => all rendered tiles in LOS are in radius
;                  => also used for minimapupdate only in light radius but still all in LOS (problematic when light radius << screen size => minimap not really updated much)

(defn- lightning-linear-falloff [i dist r]
  (let [i (float i),dist (float dist), r (float r)]
    (* i (- 1 (/ dist r)))))

(defn- lightning-get-intensity [{radius :radius intensity :i falloff :falloff} distance]
  (cond
    (> distance radius) 0
    (not falloff)  (lightning-linear-falloff intensity distance radius)
    :else (let [max-intensity-radius (- radius falloff)]
            (cond
              (< distance max-intensity-radius) intensity
              :else
              (lightning-linear-falloff intensity (- distance max-intensity-radius) falloff)))))

(defn- lightning-calc-color
  [posi {:keys [color radius] :as light} light-posi]
  (let [distance (get-distance posi light-posi)]
    (when (< distance radius) ; TODO does not take into account width/height of an entity! maybe for square ones (+ radius half-w)
      (.scaleCopy ^Color color (lightning-get-intensity light distance)))))

(defn- lightning-get-corner-posi [[x y] corner]
  (let [x (int x),y (int y)]
    (cond
      (= corner Image/TOP_LEFT) [x y]
      (= corner Image/TOP_RIGHT) [(+ x 1) y]
      (= corner Image/BOTTOM_LEFT) [x (+ y 1)]
      (= corner Image/BOTTOM_RIGHT) [(+ x 1) (+ y 1)])))
; todo just use this and (mapv + posi (get corners-posi corner))
; condp = !
;{Image/TOP_LEFT [0 0]
; Image/TOP_RIGHT [1 0]
; Image/BOTTOM_LEFT [0 1]
; Image/BOTTOM_RIGHT [1 1]}

(def image-corners
  [Image/TOP_LEFT
   Image/TOP_RIGHT
   Image/BOTTOM_LEFT
   Image/BOTTOM_RIGHT])

(defn- lightning-create-lightmap
  "A map of int-tile-positions in radius and as value all corner-colors."
  [light-posi {:keys [radius] :as light}]
  (merge
    {:posi light-posi}
    (let [explored-tile-corners (:explored-tile-corners (get-current-map-data))
          corner-colors (into {}
                          (for [tileposi (tiles-inside-rect [light-posi radius radius])
                                :when (inside-map? tileposi)
                                :let [color (when-not (ray-blocked? light-posi tileposi)
                                              (lightning-calc-color tileposi light light-posi))]]
                            (do
                              (when color
                                ; tile inside rect & inside map & not ray blocked & < distance radius
                                ; TODO? simpler => not tiles-inside-rect sondern get touched tiles of radius circle
                                ; then no need for distance check @ calc-color
                                (swap! explored-tile-corners assoc tileposi true))
                              [tileposi color])))
          tile-corner-colors (into {}
                                   (for [tileposi (keys corner-colors)]
                                     [tileposi
                                      (genmap image-corners ; TODO make vector or map color (range 4) ? faster?
                                              (fn [corner]
                                                (get corner-colors (lightning-get-corner-posi tileposi corner))))]))]
      (update-minimap (map first (filter (fn [[tile corner-colors]]
                                           (some #(not (nil? %)) (vals corner-colors)))
                                         tile-corner-colors)))
      tile-corner-colors)))

(comment
  (let [posi (:value (:position @player-body))
        light (:light @player-body)]
    (compare-times 100 (lightning-create-lightmap posi light))))

(def ^:private lightning-px-dist-dirty 5)

(def ^:private lightning-sources (atom #{}))
(def lightning-session (atom-session lightning-sources :save-session false))

; TODO map changed sets dirty
(defn- lightning-update-sources []
  (let [sources (filter #(:light @%) (get-entities-in-active-content-fields))]
    (doseq [entity sources
            :let [current-posi (:value (:position @entity))
                  light (:light @entity)
                  old-position (-> light :lightmap :posi)
                  dirty (or
                          (:dirty light)
                          (and
                            (not= old-position current-posi)
                            (>
                              (in-pixel
                                (get-distance old-position current-posi))
                              lightning-px-dist-dirty)))]]
      (when dirty
        (swap! entity assoc-in [:light :dirty] false)
        (swap! entity assoc-in [:light :lightmap] (lightning-create-lightmap current-posi light))))
    (reset! lightning-sources (doall sources)))) ; doall here because entities might be removed later

; TODO als :posi-changed machen dann neue lightmap
; allerdings dann schwierig zu pmappen
(ingame-loop-comp :update-light-sources
  (active [delta _ _]
    (lightning-update-sources)))

; TODO rename add-color-to-corner
; also .addColor is unsafe image corners have to be initialized first with setImageColor(can do with 0,0,0,0)
(defn- lightning-apply-corner-color [^Image image corner color]
  (when color ; TODO not necessary already checked before but safer...
    (.addColor image corner color)))

(defn- lightning-get-corner-colors [^Image image]
  (map #(.getCornerColor image %) (range 4)))

; not yet implemented: if image color has reached max-value (>1.0f for all corners), then you can stop applying lightsources
; because image is at maximum brightness..
; -> happends a lot but filtering is more expensive!
; -> maybe do it in java?
(defn- lightning-fully-lit? [^Color c]
  (and
    (>= (.r c) 1)
    (>= (.g c) 1)
    (>= (.b c) 1)))

; previously for every image corner the color was calculated & a ray casted, but the difference for most small agents is
; very small almost not noticeable, so only the color for the center is calculated which gives a performance boost and is simpler
; and also when rotation the corner positions are not correct anymore
(defn- lightning-apply-lightsource [image position light-source]
  (let [light (:light @light-source)
        light-posi (:value (:position @light-source))
        ; dont check if ray-blocked because
        ; already checked if entities are in-line-of-sight,
        ; so no need to cast another ray
        color (lightning-calc-color position light light-posi)]
    ;(when color
      ; Option 1: set-corner-colors transparent before applying lightsources
      ; then change the transparency here according to the visibility => are invisible on explored&unexplored background
      ;(set! (.a color) (.r color))

      ; Option 2: set-corner-colors transparent before applying lightsources
      ; suddenly black color like from the pitch darkness but ambient 0.4 behind them = Strange
      ;(set! (.a color) 1)

      ; Option 3 (currently): always visible in ambient color also outside light radius in unexplored terrain (when still in LOS)
      ; == player light radius is not visiblity radius
      ; => just set-corner-colors to an ambient color
     ;)
    (doseq [corner image-corners]
      (lightning-apply-corner-color image corner color))))

(defn- lightning-apply-lightsource-cached [image position light-source]
  (when-let [cached-colors (get
                             (:lightmap (:light @light-source))
                             (int-posi position))]
    (doseq [corner image-corners
            :let [cached-color (get cached-colors corner)]]
      ;(when (every? lightning-fully-lit? (lightning-get-corner-colors image))
      ;  (println "fully-lit and applying " [ix iy]))
      (when cached-color
        (lightning-apply-corner-color image corner cached-color)))))

(defn- lightning-set-corner-colors [image value]
  (.setImageColor ^Image image value value value))

(defn- lightning-reset-brightness [image] (lightning-set-corner-colors image 1))

(def active-lightning (atom true))

(defn set-brightness [image position]
  (if @active-lightning
    (do
      (lightning-set-corner-colors image lightning-entity-ambient)
      (dorun (map #(lightning-apply-lightsource image position %) @lightning-sources)))
    (lightning-reset-brightness image)))

(defn tile-explored? [corner-tile-position]
  (get @(:explored-tile-corners (get-current-map-data)) corner-tile-position))
; is expensive always accessing current map data?
; maybe when render-grid dereference it 1 time and then just (get explored-tile-corners corner-position)
; but how to do at set-cached-brightness of tiledmaps? => just use a different explored? ...

(defn set-cached-brightness [image position] ; TODO name?
  (if @active-lightning
    (do
      (lightning-set-corner-colors image (if @debug-mode lightning-tile-ambient 0))
      (doseq [corner image-corners
              :when (tile-explored? (lightning-get-corner-posi position corner))]
        (lightning-apply-corner-color image corner lightning-explored-tile-color))
      (dorun (map #(lightning-apply-lightsource-cached image position %) @lightning-sources)))
    (lightning-reset-brightness image)))

(def lightmanager (reify LightManager
                         (setBrightness [this image x y]
                           (set-cached-brightness image [x y]))))

(defn light-component [& {:keys [intensity radius falloff color]
                          :or {color white}}]
  {:type :light
   :color color
   :i intensity
   :radius radius
   :falloff falloff
   :depends [:position]
   :dirty true
   :init (fn [entity]
           (add-cell-blocks-changed-listener
             #(swap! entity assoc-in [:light :dirty] true)))})

(defn map-lightsource [position]
  (create-entity
    (position-component position)
    (light-component :intensity 1 :radius 4)))

;; render components

(defn- render-make-rendering-props [order fnargs]
  `{:rendering true
    :order ~order
    :renderfn ~(make-fn fnargs)})

; can not check if order is keyword because order might also be a symbol and not a keyword literal
(defn- render-destructure-args
  "four cases:
  order renderfn
  renderfn
  order [graphics component] &body
  [graphics component] &body"
  [default & more]
  (cond
    (= 1 (count more)) [default more]
    (vector? (first more)) [default more]
    :else [(first more) (rest more)]))

(def render-on-map-order (define-order [:is-ground :on-ground :default :air :top-level]))

(defmacro render-on-map [& more]
  (let [[order fnargs] (apply render-destructure-args :default more)]
    `(do
       (assert (order-contains? render-on-map-order ~order))
       ~(render-make-rendering-props order fnargs))))

; :hpmana below gui because tooltips should be over them ... then we have tooltips which is another type of tooltips...
(def render-map-indep-order (define-order [:below-gui :hpmanabar :selfmade-gui :gui :tooltips :above-gui]))

; maybe rename to match render-on-map -> render-on-gui?
(defmacro rendering [& more]
  (let [[order fnargs] (apply render-destructure-args :gui more)]
    `(do
       (assert (order-contains? render-map-indep-order ~order))
       ~(render-make-rendering-props order fnargs))))

;;

(defn translate-position [[x y]]
  [(* x tile-width),(* y tile-height)])

(defn rect-render-component [pxw pxh]
  (safe-merge
    {:type :rect-render
     :shape (rectangle -1 -1 pxw pxh)}
    (render-on-map [g _ c render-posi]
      (render-centered-shape g (:shape c) render-posi))))

(defn- render-rotated-lighted-image [g entity render-posi image apply-light]
  (when apply-light
    (if (:always-in-sight @entity)
      (set-cached-brightness image (int-posi (:value (:position @entity)))) ; brightness like map tiles explored/unexplored
      (set-brightness image (:value (:position @entity)))))
  (if-let [angle (:angle (:rotation @entity))]
    (render-rotated-centered-image g image angle render-posi)
    (render-centered-image image render-posi)))

(defn image-render-component [image & {:keys [order apply-light]
                                       :or {order :default apply-light true}}]
  (safe-merge
    {:type :image-render
     :depends [:position]
     :image image}
    (render-on-map order [g entity c render-posi]
      (render-rotated-lighted-image g entity render-posi (:image c) apply-light))))

(defn- render-update-animation
  "control is a function with (fn [entity]) which must return the new animation key and can also update some stuff"
  [delta {:keys [akey control] :as component} entity]
  (let [last-akey akey
        new-akey (control entity)
        _ (assert (keyword? new-akey))
        new-animation (cond-> (new-akey component)
                        (not= last-akey new-akey) restart-animation
                        :then                     (tick delta))]
    (swap! entity update-in [(:type component)]
                assoc
                :akey    new-akey
                new-akey new-animation)))

(defn current-animation [entity]
  (let [{:keys [akey] :as c} (:animation @entity)]
    (akey c)))

(defn- render-animation-comp [g entity {:keys [apply-light] :as component} render-posi]
  ; can be nil when mapchange -> render happends before update for a component (else update happends before render)
  (when-not (:akey component)
    (render-update-animation 1 component entity))
  (render-rotated-lighted-image g entity render-posi (get-animation-frame (current-animation entity)) apply-light))

; types-animations is a map of keyword to animation and control returns a keyword which animation to play next.
(defn animation-component [control types-animations & {:keys [order apply-light]
                                                       :or {order :default apply-light true}}]
  (safe-merge
    {:type :animation
     :control control
     :depends [:position]
     :akey nil
     :apply-light apply-light}
    (render-on-map order render-animation-comp)
    (active render-update-animation)
    types-animations))

(defn single-animation-component [animation & more]
  (apply animation-component (constantly :default) {:default animation} more))

(defn animation-entity
  [& {:keys [position animation order] :or {order :air}}]
  (create-entity
    (position-component position)
    {:type :always-in-sight}
    (single-animation-component animation :order order :apply-light false)
    (merge {:type :delete-after-stopped}
           (active [_ _ entity]
             (when (-> (:animation @entity) :default animation-stopped?)
               (add-to-removelist entity))))))

; FIXME animation that is passed to reload session args == initial one with cnt = 0 ....
; animation component 'current' ?

;;;

(defn- render-circle-effect-components [position radius color duration]
  (let [shape (circle [-1 -1] (in-pixel radius))] ; TODO shape rendering component?
    [{:type :session
      :constructor "cdq.core/create-circle-render-effect*"
      :args [position radius color duration]}
     (position-component position)
     (delete-after-duration-component duration)
     (merge {:type :visual}
            (render-on-map [g _ c render-posi]
                           (render-centered-shape g shape render-posi color)))]))

(defn create-circle-render-effect* [position radius color duration]
  (apply create-entity-no-init (render-circle-effect-components position radius color duration)))

(defn create-circle-render-effect [position radius color duration]
  (apply create-entity (render-circle-effect-components position radius color duration)))

(defn- render-draw-fat-line [g [x y] [ex ey]]
  (doseq [x  [(dec x)  x  (inc x)]
          y  [(dec y)  y  (inc y)]
          ex [(dec ex) ex (inc ex)]
          ey [(dec ey) ey (inc ey)]]
    (draw-line g x y ex ey)))

(defn create-lines-render-effect [healer healed-bodies duration]
  (create-entity
    (position-component (:value (:position @healer)))
    (delete-after-duration-component duration)
    {:type :always-in-sight}
    (merge {:type :visual}
           (render-on-map :air [g _ component start]
                          (set-color g green)
                          (doseq [end (map #(translate-position (:value (:position @%))) healed-bodies)]
                            (render-draw-fat-line g start end))))))

(defn- create-line-render-effect* [start end duration color thin]
  (create-entity
    (position-component start)
    (delete-after-duration-component duration)
    {:type :always-in-sight}
    (merge {:type :visual}
           (render-on-map :air [g _ component start]
                          (set-color g color)
                          (if thin
                            (draw-line   g start (translate-position end))
                            (render-draw-fat-line g start (translate-position end)))))))

(defn create-line-render-effect [start end duration & {color :color thin :thin}]
  (create-line-render-effect* start end duration color thin))

(defn render-above-body [g body [x y] image & {ypuffer :ypuffer :or {ypuffer 0}}]
  (let [posi [x
              (- y (:half-pxh (:body @body)) (/ (second (get-dimensions image)) 2) ypuffer)]]
    (render-centered-image image posi)))

(defn show-string-effect [body duration color string]
  (let [tilepos (:value (:position @body))]
    (ingame-loop-comp :string-effect
      {:counter (make-counter duration)
       :color (Color. ^Color color)}  ; copy before changing it @ set!
      (active [delta {:keys [color counter] :as c} entity]
        (set! (.a ^Color color) (- 1 (counter-ratio counter)))
        (when (update-counter! entity delta c)
          (add-to-removelist entity)))
      (rendering :below-gui [g {:keys [color counter] :as c}]
        (let [[rx ry] (screenpos-of-tilepos tilepos)]
          (render-readable-text g rx (- ry (:half-pxh (:body @body)) (* 50 (counter-ratio counter)))
            :shift false
            :above true
            :centerx true
            :background false
            color string))))))

(defn show-gains-hp-effect [body gained]
  (show-string-effect body 2000 (rgbcolor :g 1 :r 0.2 :b 0.2) (str "+" (readable-number gained))))

(defn show-gains-mana-effect [body gained]
  (show-string-effect body 2000 (rgbcolor :g 0.2 :r 0.2 :b 1) (str "+" (readable-number gained))))

(defn circle-around-body-render-comp
  "target entity is not the entity of this component (used at sub-entities that only share position)"
  [target color order]
  (merge {:type :visuals}
         (render-on-map order [g _ c render-posi]
           (let [radius (+ 2 (:half-pxw (:body @target)))]
             (fill-centered-circle g radius render-posi color)))))

;; body component

(defn get-body-bounds [body]
  [(:value (:position @body)) (:half-width (:body @body)) (:half-height (:body @body))])

(defn calc-touched-cells
  ([body]
    (apply calc-touched-cells (get-body-bounds body)))
  ([position half-w half-h]
    (get-cells
      (get-touched-tiles position half-w half-h))))

(defn inside-cell? [body cell]
  (let [touched-cells (calc-touched-cells body)]
    (and (= 1 (count touched-cells))
         (= cell (first touched-cells)))))

(defn- body-set-occupied-cell [body]
  (if (:is-multiple-cell (:body @body))
    (let [cells (calc-touched-cells body)]
      (dorun (map #(swap! % update-in [:occupied] conj body) cells))
      (swap! body assoc-in [:body :occupied-cells] cells))
    (let [cell (get-cell (:value (:position @body)))]
      (assert cell)
      (swap! cell update-in [:occupied] conj body)
      (swap! body assoc-in [:body :occupied-cell] cell))))

(defn- body-remove-from-occupied-cell [body]
  (if (:is-multiple-cell (:body @body))
    (dorun (map #(swap! % update-in [:occupied] disj body) (:occupied-cells (:body @body))))
    (swap! (:occupied-cell (:body @body)) update-in [:occupied] disj body)))

(defn update-occupied-cell [body]
  (body-remove-from-occupied-cell body)
  (body-set-occupied-cell body))

(defn- body-set-touched-cells
  ([body]
    (body-set-touched-cells body (calc-touched-cells body)))
  ([body new-cells]
    {:pre [(not-any? nil? new-cells)]}
    (swap! body assoc-in [:body :cached-touched-cells] new-cells)
    (dorun (map #(cell-add-body % body) new-cells))))

(defn- body-remove-from-touched-cells
  ([body]
    (body-remove-from-touched-cells body (:cached-touched-cells (:body @body))))
  ([body cached-cells]
    (dorun (map #(cell-remove-body % body) cached-cells))))

(defn update-touched-cells
  ([body]
    (update-touched-cells body (calc-touched-cells body)))
  ([body new-cells]
    (let [cached-cells (:cached-touched-cells (:body @body))]
      (when-not (= new-cells cached-cells)
        (body-remove-from-touched-cells body cached-cells)
        (body-set-touched-cells body new-cells)))))

; fur mouseoverbodies damit man gut klicken kann
; solid da fur projektile soll es net gelten
; entweder dies oder max-speed muss festgelegt werden.
(def min-solid-pxsize 8)
(def half-min-solid-pxsize (/ min-solid-pxsize 2))

(declare body-info-renderfn)

(defn body-component [solid side half-pxw half-pxh mouseover-outline]
  {:pre [(= (class solid) java.lang.Boolean)
         (#{:monster :player :no-side} side)
         (>= half-pxw (if solid half-min-solid-pxsize 0))
         (>= half-pxh (if solid half-min-solid-pxsize 0))
         (< (/ half-pxw tile-width) 2)
         (< (/ half-pxh tile-height) 2)]} ; wg. rendering on screen (max ungetestet - ist etwas kleiner als 3 die tatsachliche maximale size)
  (let [half-w (/ half-pxw tile-width)
        half-h (/ half-pxh tile-height)]
    (safe-merge
      {:type :body
       :solid solid
       :side side
       :half-pxw half-pxw
       :half-pxh half-pxh
       :mouseover-outline mouseover-outline
       :half-width half-w
       :half-height half-h
       :is-multiple-cell (does-not-fit-in-a-tile? half-w half-h)
       :depends [:position]
       :init (if-not solid
               body-set-touched-cells
               (fn [entity]
                 (body-set-touched-cells entity)
                 (body-set-occupied-cell entity)))
       :destruct (if-not solid
                   body-remove-from-touched-cells
                   (fn [entity]
                     (body-remove-from-touched-cells entity)
                     (body-remove-from-occupied-cell entity)))
       :posi-changed (if-not solid
                       update-touched-cells
                       (fn [entity] ; posi changed of solid entities @ update-movement manually called
                         (update-touched-cells entity)
                         (update-occupied-cell entity)))}
      (render-on-map :air body-info-renderfn))))

(defn create-body
  [& {:keys [solid pxw pxh dimensions mouseover-outline side]
      :or {side :no-side}}]
  (let [pxw (or pxw (dimensions 0))
        pxh (or pxh (dimensions 1))]
    (assert (and pxw pxh))
    (body-component solid side (/ pxw 2) (/ pxh 2) mouseover-outline)))

;; Body Geometry

(defn- body-rect [body] ; cache in body and update @ :posi-changed?
  (let [[[x y] half-w half-h] (get-body-bounds body)]
    [(- x half-w) ; 22% faster with 4 float casts
     (- y half-h)
     (* 2 half-w)
     (* 2 half-h)]))

; cache in body and update @ :posi-changed?
(def rect-shape (comp rectangle body-rect))

(defn circle-collides? [p radius body]
  (collides? (circle p radius)
                  (rect-shape body)))

(defn get-bodies-at-position [position]
  (when-let [cell (get-cell position)]
    (filter #(point-in-rect? position (body-rect %))
            (map #(get @id-entity-map %) (get-body-ids cell)))))

(defn bodies-in-range? [b1 b2 range-sqr]
  (in-range? (:value (:position @b1)) (:value (:position @b2)) range-sqr))

; circle-collides creates a new Circle shape for every check -> could be optimized
(defn get-touched-bodies
  [posi radius]
  (filter #(circle-collides? posi radius %)
          (get-bodies-from-cells
            (calc-touched-cells posi radius radius))))

(defn get-other-solid-bodies
  ([position half-w half-h body-id touched-cells]
    (filter
      #(and
         (not= (:id (meta %)) body-id)
         (:solid (:body @%))
         (rect-collision? [position half-w half-h] (get-body-bounds %)))
      (get-bodies-from-cells touched-cells)))
  ([position half-w half-h touched-cells]
    (get-other-solid-bodies position half-w half-h -1 touched-cells))
  ([position body touched-cells]
    (get-other-solid-bodies position (:half-width (:body @body)) (:half-height (:body @body)) (:id (meta body)) touched-cells)))

(defn colliding-with-other-solid-bodies? [body]
  (boolean
    (not-empty
      (get-other-solid-bodies (:value (:position @body)) body (:cached-touched-cells (:body @body))))))

(defn blocked-location?
  ([posi half-w half-h movement-type]
    (let [touched-cells (calc-touched-cells posi half-w half-h)]
      (or
        (some #(cell-blocked? % movement-type) touched-cells)
        (seq (get-other-solid-bodies posi half-w half-h touched-cells)))))
  ([posi body movement-type]
    (blocked-location? posi (:half-width (:body @body)) (:half-height (:body @body)) movement-type))
  ([posi body]
    (blocked-location? posi body (:movement-type (:movement @body)))))

; TODO adjacent-cells could be nil -> :middle @cell -> NPE
(defn- body-find-nearby-valid-location [body posi]
  (when-let [cell (first (filter #(not (blocked-location? (:middle @%) body))
                              (cached-get-adjacent-cells (get-cell posi))))]
    (:middle @cell)))

(defn teleport
  "searches nearby positions for a free one if target is blocked
   if none exists -> just teleports to the blocked position."
  [body posi]
  (swap-position! body
                  (if-not (blocked-location? posi body)
                    posi
                    (if-let [valid (body-find-nearby-valid-location body posi)]
                      valid
                      posi))))
;;

(defn is-burrowed? [entity]
  (if-let [c (:burrow @entity)]
    (:burrowed c)))

(defn is-affectable? [entity]
  (not (is-burrowed? entity)))

(defn- body-is-visible? [entity]
  (not (is-burrowed? entity)))

; hier entity mit position component benotigt - kein body
; Has to work with entities who have :position aber kein :body!
; in sight of player-body -> ray from player-body! ; not in sight of some light-source ..
(defn on-screen-and-in-sight? [entity]
  (and
    ; TODO posi-in-rect hier? / does not take into account body size
    (let [[x y] (:value (:position @entity))
          x (float x)
          y (float y)
          [cx cy] (get-camera-position)
          px (float cx)
          py (float cy)
          xdist (Math/abs (- x px))
          ydist (Math/abs (- y py))]
      (and
        (<= xdist (inc half-display-w-in-tiles))
        (<= ydist (inc half-display-h-in-tiles))))
    (or
      (when @debug-mode entities-always-in-los)
      (:always-in-sight @entity)
      (and (body-is-visible? entity)
           (not (ray-blocked? (:value (:position @player-body)) (:value (:position @entity))))))))

; Assert: not jumping over tiles, so speed is <1 tile at max-delta

(defn- body-get-direction-fn [d] (if (pos? d) + -))

(defn- body-get-to-check-tiles-big [body [x y] xdir ydir]
  (let [half-w (:half-width (:body @body))
        half-h (:half-height (:body @body))
        x (float x),y (float y),half-w (float half-w),half-h (float half-h)]
    (set
      (concat
        (when-not (zero? xdir)
          (let [x (int ((body-get-direction-fn xdir) x half-w))
                t (int (- y half-h))
                b (int (+ y half-h))]
            (for [y (range t (inc b))]
              [x y])))
        (when-not (zero? ydir)
          (let [y (int ((body-get-direction-fn ydir) y half-h))
                l (int (- x half-w))
                r (int (+ x half-w))]
            (for [x (range l (inc r))]
              [x y])))))))

(defn- body-get-to-check-tiles-small [body [x y] xdir ydir]
  (let [half-w (:half-width (:body @body))
        half-h (:half-height (:body @body))
        x (float x),y (float y),half-w (float half-w),half-h (float half-h)]
    (set
      (concat
        (when-not (zero? xdir)
          (let [x (int ((body-get-direction-fn xdir) x half-w))
                t (int (- y half-h))
                b (int (+ y half-h))]
            [[x t] [x b]]))
        (when-not (zero? ydir)
          (let [y (int ((body-get-direction-fn ydir) y half-h))
                l (int (- x half-w))
                r (int (+ x half-w))]
            [[l y] [r y]]))))))

(defn get-to-check-tiles [body new-posi xdir ydir]
  (if (:is-multiple-cell (:body @body))
    (body-get-to-check-tiles-big body new-posi xdir ydir)
    (body-get-to-check-tiles-small body new-posi xdir ydir)))

;;

(defn get-other-bodies-in-adjacent-cells [entity]
  (->> (:value (:position @entity))  ; TODO could be faster using occupied-cell instead of get-cell...
    get-cell
    cached-get-adjacent-cells
    get-bodies-from-cells
    (remove #(= (:id (meta %)) (:id (meta entity))))))

;;

(defn get-dist-to-player
  "only works for single cell bodies, may return nil when outside of potential-field"
  [entity]
  (:dist-to-player @(:occupied-cell (:body @entity))))

;; msg-to-player

(def ^:private msg-to-player-message (atom nil))
(def msg-to-player-message-session (atom-session msg-to-player-message :save-session false))

(def ^:private msg-to-player-counter (create-counter 4000))
(def msg-to-player-counter-session (atom-session msg-to-player-counter :save-session false))

(defn show-msg-to-player [& more]
  (reset-counter! msg-to-player-counter)
  (reset! msg-to-player-message (apply str more)))

(ingame-loop-comp :msg-to-player
                  ; noch 1 stufe hoher als :above-gui -> uber den tooltips! ->:tooltip / :above-gui?
                  ; -> call it the way it is not "above-gui" but "important messages"
  (rendering :above-gui [g _]
    (when-let [msg @msg-to-player-message]
      (render-readable-text g half-screen-w (/ half-screen-h 2) :centerx true :background false :bigfont true msg)))
  (active [delta _ _]
    (when (and @msg-to-player-message (update-counter msg-to-player-counter delta))
      (reset! msg-to-player-message nil))))

;; sleeping

(def ^:private sleeping-half-aggro-width-tiles 11)
(def ^:private sleeping-half-aggro-height-tiles 8.5)

; > wakeup-range-rectangle
; (because wakeup checks with rectangle collision..
; and this is center to center distance ...)
; RADIUS needs to be at least half the diagonal of the rectangle!
; so no entities will be awoken which are at corner...
(def start-position-safezone-radius (+ 2.2 sleeping-half-aggro-width-tiles))

(defn- sleeping-in-wakeup-range? [body]
  (rect-collision? (get-body-bounds body)
                        [(get-camera-position)
                         sleeping-half-aggro-width-tiles
                         sleeping-half-aggro-height-tiles]))
; TODO not camera but player ... if we seperate them at some point...

(defn- sleeping-entity? [entity] (:sleeping (:sleeping @entity)))

(defn sleeping-component []
  {:type :sleeping
   :depends [:body]
   :sleeping true
   :init #(swap! % block-active-components)})

(defn wake-up [entity]
  (when (sleeping-entity? entity)
    (swap! entity #(-> % 
         unblock-active-components
         (assoc-in [:sleeping :sleeping] false)))
    (->> entity
      get-other-bodies-in-adjacent-cells
      (filter sleeping-entity?)
      (map wake-up)
      dorun)))

(def ^:private sleeping-counter (create-counter 1000))
(def sleeping-session (atom-session sleeping-counter :save-session false))

(def ^:private sleeping-show-range false)

(ingame-loop-comp :wakeup

  (active [delta c _]
    (when (update-counter sleeping-counter delta)
      (dorun (map wake-up
              (filter sleeping-in-wakeup-range?
                      (filter sleeping-entity?
                              (get-entities-in-active-content-fields)))))))
  (rendering :above-gui [g c]
    (when sleeping-show-range
      (render-centered-shape
        g
       (rectangle 0 0
                  (in-pixel (* 2 sleeping-half-aggro-width-tiles))
                  (in-pixel (* 2 sleeping-half-aggro-height-tiles)))
        [(/ screen-width 2)
         (/ screen-height 2)])
;      (render-centered-shape g
;                             (circle [0 0]
      ;                             (in-pixel start-position-safezone-radius))
;                             [(/ screen-width 2)
;                              (/ screen-height 2)])
      (render-readable-text g 50 100
                            (->> (get-entities-in-active-content-fields)
                                 (filter sleeping-entity?)
                                 (filter sleeping-in-wakeup-range?)
                                 count
                                 (str "In wakeup range&sleeping: "))))))

;; body-effects

(defn get-sub-entities [entity]
  (when-let [subids (:ids (:sub-entities @entity))]
    (map #(get @id-entity-map %) subids)))

(defn sub-entities-component []
  {:type :sub-entities
   :depends [:position]
   :ids #{}
   :destruct (fn [entity]
              (dorun (map destruct-entity (get-sub-entities entity))))
   :posi-changed (fn [entity]
                   (let [p (:value (:position @entity))]
                     (dorun (map #(swap-position! % p) (get-sub-entities entity)))))})

(defn body-effect-entity
  "Effect-entities are deleted when target entity is deleted.
  They have a position component that is getting updated when the target position changes.
  They also call 'wake-up' on the target."
  [& args]
  (let [[{:keys [target duration type undofn dofn]
          :or {type :no-type
               undofn (fn [])
               dofn (fn [])}}
         additional-components] (split-kvs-and-more args)]
    (wake-up target)
    (apply create-entity
           (position-component (:value (:position @target)))
           {:type :body-effect
            :target target
            :effect-type type
            :init (fn [this]
                    (dofn)
                    (when-not (:sub-entities @target)
                      (add-component target (sub-entities-component)))
                    (swap! target update-in [:sub-entities :ids] conj (:id (meta this))))
            ; first disj id before undofn is called!
            ; undofn may access get-effect-entities and this one is already not existing!
            :destruct (fn [this]
                        (swap! target update-in [:sub-entities :ids] disj (:id (meta this)))
                        (undofn))}
           (when duration
             (delete-after-duration-component duration))
           additional-components)))
; -> could move delete-after-duration/wake-up(as obligatory-key)/undo/do to implementation

; fix psi-charger set-durations @ undo -> remove this 'first disj' comment
; -> set-durations of psi-charger?? index 0,1,2 even if only 1 less?
; better: durations not set when one is undo but at creation ? 20/40/60?

(defmacro defeffectentity [namesym argvec & more]
  `(defn ~namesym ~argvec
     (body-effect-entity :type ~(keyword namesym)
                         ~@more)))

(defn get-certain-effect-entities [entity effect-type]
  (filter #(= (:effect-type (:body-effect @%)) effect-type)
          (get-sub-entities entity)))

(comment
  (map #(:effect-type (:body-effect @%))
       (get-sub-entities (get @id-entity-map 31))))

(comment
  (with-redefs [position-component (fn [p] {:type :position :p p})
                get-other-bodies-in-adjacent-cells (fn [entity] ())]

    (let [testcoll (atom [])
          entity (create-entity {:type :position}
                                {:type :body}
                                (sleeping-component))]
      (println "sleep? " (#'sleeping-entity? entity))
      (body-effect-entity :target entity
                          :dofn #(swap! testcoll conj 1)
                          :undofn #(swap! testcoll conj 2))
      (destruct-entity entity)
      (println "sleep? " (#'sleeping-entity? entity))
      @testcoll)))

;; shield

(def ^:private shield-rotation-speed (/ 360 2000)) ; 360 degrees in 2 seconds = 2000 ms

(defn shield-component [regeneration-duration]
  (safe-merge
    {:type :shield
     :is-active true
     :image (create-image "effects/shield.png")
     :angle 0
     :counter (make-counter regeneration-duration)}
    (render-on-map :on-ground [g _ {:keys [is-active image angle] :as c} render-posi]
      (when is-active
        (render-rotated-centered-image g image angle render-posi)))
    (active [delta {:keys [is-active counter] :as c} entity]
      (swap! entity assoc-in [(:type c)]
                 (if is-active
                   (update-in c [:angle] degree-add (* delta shield-rotation-speed))
                   (tick-finally-merge c :counter delta
                                         {:angle 0 :is-active true}))))))

(defeffectentity ^:private shield-hit [body]
  :target body
  :duration 100
  (circle-around-body-render-comp body (rgbcolor :g 1 :r 0.5 :a 0.5) :air))

(defn shield-try-consume-damage [body]
  (when-let [shield (:shield @body)]
    (when (:is-active shield)
      (swap! body assoc-in [:shield :is-active] false)
      (shield-hit body)
      true)))

;; gui

; Nett:
; variable characterframe textlabel (text) function or jsut when text=function (text) instead of text
; also free skill points : ...
; is-visible check of frame deutet auf guicomponent mit :parent benutzen hin!
; z.b. ausgrauen == enabled false/true !
; also skill tooltips!
; mixup of refcomponent/component ...
; leftm consume auf button A und dann move auf button B => B hat is-leftm-consumed color!

(defn gui-mouseover? [component] (point-in-rect? (get-mouse-pos) (:bounds component)))

;;;;;;;;;;;

(def ^:private gui-mouseover-darkgray (rgbcolor :r 0.3 :g 0.3 :b 0.3 :a 0.5))
(def ^:private gui-mouseover-lightgray (rgbcolor :r 0.7 :g 0.7 :b 0.7 :a 0.5))

(defn- gui-render-label [g {:keys [text bounds]}]
  (let [[x y w h] bounds]
    (render-readable-text g x y :background false :shift false text)))

(defn- gui-render-pressable-rect [g {:keys [bounds] :as component}]
  (let [[x y w h] bounds]
    (when (gui-mouseover? component)
      (fill-rect g (dec x) (dec y) (+ w 1) (+ h 1) (if (is-leftm-consumed?) gui-mouseover-lightgray gui-mouseover-darkgray)))
    (draw-rect g (dec x) (dec y) (+ w 1) (+ h 1) white)))

(defn- gui-render-textbutton [g component]
  (gui-render-label g component)
  (gui-render-pressable-rect g component))

(alter-var-root #'init-fns conj
  (fn []
    (def ^:private gui-selected-img (create-image "selected.png"))))

(defn- gui-render-checkbox [g {:keys [bounds] :as component}]
  (let [[x y w h] bounds]
    (gui-render-textbutton g component)
    (when (:selected component)
      (draw-image gui-selected-img (+ x (+ w 2)) y))))

(defn- gui-render-imgbutton [g {:keys [bounds image] :as component}]
  (let [[x y w h] bounds]
    (draw-image image x y))
  (gui-render-pressable-rect g component))

(def ^:private gui-framebg (rgbcolor :r 0.05 :g 0.05 :b 0.05 :a 1))
(def ^:private gui-frameborder (rgbcolor :r 0.2 :g 0.2 :b 0.2 :a 1))

(defn- gui-render-frame [g {:keys [bounds] :as component}]
  (fill-rect g bounds gui-framebg)
  (draw-rect g bounds gui-frameborder))

;;;;;;;;;

(defn gui-set-visible [component bool] (swap! component assoc :visible bool))

(defn gui-switch-visible [component] (swap! component update-in [:visible] not))

(defn gui-visible? [component] (:visible @component))

(defn gui-get-absolute-posi [component]
  (let [[x y w h] (:bounds @component)]
    [x y]))

(defn gui-get-bounds [component] (:bounds @component))

;;;;;;;

(defn- gui-checkbox? [component] (contains? component :selected))

(defn- gui-update-button [refcomponent]
  (let [{:keys [pressed visible] :as component} @refcomponent]
    (when (and visible
               (gui-mouseover? @refcomponent)
               (try-consume-leftm-pressed))
      (if (gui-checkbox? @refcomponent)
        (pressed (:selected (swap! refcomponent update-in [:selected] not)))
        (pressed)))))

(defn- gui-update-frame [refcomponent]
  (when (is-key-pressed? (:hotkey @refcomponent))
    (gui-switch-visible refcomponent)))

;;;;;;;;;;

(defn make-guidisplay [] (atom {:children (list)}))

(defn remove-guicomponent [display component]
  (swap! display update-in [:children] (fn [children] (remove #(= % component) children))))

(defn- gui-translate-by-parent-bounds [[x y w h] [px py pw ph]]
  [(+ px x) (+ py y) w h])

(defn- gui-component
  "Can also have :name"
  [{:keys [parent visible] :or {visible true} :as data}]
  {:pre [(every? #(contains? data %) [:parent :bounds])]}
  (let [component (atom (assoc data :visible visible))]
    (swap! parent update-in [:children] conj component)
    (when (:bounds @parent)
      (swap! component update-in [:bounds] gui-translate-by-parent-bounds (:bounds @parent)))
    component))

(defn- gui-get-textbounds [{x 0 y 1} text]
  [x y (get-text-width text (get-defaultfont)) (get-text-height text (get-defaultfont))])

(defn make-label [& {:keys [text location] :as data}]
  {:pre [(every? #(contains? data %) [:location :text])]}
  (gui-component (merge data
                       {:render gui-render-label
                        :bounds (gui-get-textbounds location text)})))

(defn make-textbutton [& {:keys [text location] :as data}]
  {:pre [(every? #(contains? data %) [:location :text :pressed])]}
  (gui-component (merge data
                       {:render gui-render-textbutton
                        :update gui-update-button
                        :bounds (gui-get-textbounds location text)})))

(defn make-checkbox [& {:keys [text location] :as data}]
  {:pre [(every? #(contains? data %) [:location :text :pressed :selected])]}
  (gui-component (merge data
                       {:render gui-render-checkbox
                        :update gui-update-button
                        :bounds (gui-get-textbounds location text)})))

(defn make-imgbutton [& {:keys [image location] :as data}]
  {:pre [(every? #(contains? data %) [:location :image :pressed])]}
  (gui-component (merge data
                       {:render gui-render-imgbutton
                        :update gui-update-button
                        :bounds (let [[x y] location
                                      [w h] (get-dimensions image)]
                                  [x y w h])})))

(deflazygetter ^:private gui-get-closeimg (create-image "close.png"))
; using lazygetter instead of preload because other preload depends on this (make-frame)

(defn make-frame [& data]
  (let [data (apply hash-map data)]
    (assert (every? #(contains? data %) [:bounds :hotkey]))
    (let [[x y w h] (:bounds data)
          frame (gui-component (merge data
                                     {:is-frame true
                                      :render gui-render-frame
                                      :update gui-update-frame}))]
      (make-imgbutton :image (gui-get-closeimg)
                      :location [(- w 6) 2]
                      :pressed #(gui-set-visible frame false)
                      :parent frame)
      frame)))

;;;;

(defn render-guicomponent [g display]
  (doseq [refcomponent (reverse (sort-by #(:is-frame @%) (:children @display)))
          :let [{:keys [tooltip visible render] :as component} @refcomponent]
          :when visible]
    (render g component)
    (when (and tooltip (gui-mouseover? component))
      (let [[x y] (gui-get-absolute-posi refcomponent)]
        (render-readable-text g x (dec y) :shift true :background true :above true tooltip)))
    (when (:children component)
      (render-guicomponent g refcomponent))))

(defn update-guicomponent [display]
  (doseq [refcomponent (:children @display)
          :let [{:keys [update children]} @refcomponent]
          :when update]
    (update refcomponent)
    (when children
      (update-guicomponent refcomponent))))

;; screenshake

(def ^:private screenshake-delay 45) ; Delay in ms between each new shake.

(def ^:private screenshake-decay 0.25) ; The rate at which the shake "dies down." 0.01 = 1 percent every new shake

(def ^:private screenshake-intensity 6) ; How far the shake should extend in pixels.

(def ^:private screenshake-snap false)  ; Whether to snap pixels to integers (translating to a decimal number
                       ; will result in subtle blurring!).

; screenshake-snap is not required when we set the shakex shakey translations to 0 after a finished shaking
; else they will be at values shakex = 0,00..something and the image is blurred permanently after screenshakes
; because we always translate-shake-before-render...

(def ^:private screenshake-time (atom screenshake-delay))
(def ^:private screenshake-amt (atom 0))
(def ^:private screenshake-x (atom 0))
(def ^:private screenshake-y (atom 0))

(defn- screenshake-step []
  (reset! screenshake-x (* @screenshake-amt (Math/random)))
  (reset! screenshake-y (* @screenshake-amt (Math/random)))
  (when screenshake-snap
    (swap! screenshake-x int)
    (swap! screenshake-y int))
  (reset! screenshake-time screenshake-delay)
  (swap! screenshake-amt - (* screenshake-decay screenshake-intensity))
  (when (<= @screenshake-amt 0)
    (reset! screenshake-x 0)
    (reset! screenshake-y 0)
    (reset! screenshake-amt 0)))

(defn shake-screen []
  (reset! screenshake-amt screenshake-intensity)
  (screenshake-step))

(defn update-shake [delta]
;  (when (is-key-pressed? :M)
;    (shake-screen))
  (when (> @screenshake-amt 0)
    (swap! screenshake-time - delta)
    (when (<= @screenshake-time 0)
      (screenshake-step))))

(defn translate-shake-before-render [g]
  (when (and
          (not= 0 @screenshake-x)
          (not= 0 @screenshake-y))
    (graphics-translate g @screenshake-x @screenshake-y)))

(defn translate-shake-after-render [g]
  (when (and
          (not= 0 @screenshake-x)
          (not= 0 @screenshake-y))
    (graphics-translate g (- @screenshake-x) (- @screenshake-y))))

;; skills

(defn is-ready?        [{state :state}] (= state :ready))
(defn is-attacking?    [{state :state}] (= state :attacking))
(defn is-cooling-down? [{state :state}] (= state :cooldown))

(defn get-skill [body skilltype]
  (or
    (skilltype @body)
    (-> (:skillmanager @body) :skills skilltype)))

(defn get-mana [entity] (:mana (:skillmanager @entity)))

(defn enough-mana? [{cost :cost :as skill} {mana :mana :as skillmanager}]
  (<= cost (:current mana)))

(defn set-mana-to-max [body]
  (when (lower-than-max? (get-mana body))
    (show-gains-mana-effect body (rest-to-max (get-mana body))))
  (swap! body update-in [:skillmanager :mana] set-to-max))

(defn- skill-update-cooldown [skill delta]
  (if (is-cooling-down? skill)
    (tick-finally-merge skill :cooldown-counter delta
                          {:state :ready})
    skill))

(defn- skill-update-cooldowns [skillmanager delta]
  (update-in skillmanager [:skills] mapvals #(skill-update-cooldown % delta)))

(defn reset-skills [skills]
  (mapvals skills
           #(-> %
                (update-in [:cooldown-counter] reset-counter)
                (assoc :state :ready))))

(defn- skill-update-counter! [entity delta skill & [counterkey & _]]
  (let [k (or counterkey :counter)
        counter (tick (k skill) delta)]
    (swap! entity assoc-in [:skillmanager :skills (:type skill) k] counter)
    (:stopped? counter)))

; Warning:
; entities which attacktime is driven through their animation and .isStopped
; check they dont have an :attack-counter!
; and entities which attack through :attack-counter and have :animation component are ok because first :Attack-counter is checked
(defn- skill-attack-finished? [entity skill delta]
  (if (:attack-counter skill)
    (if (:skillmanager @entity)
      (skill-update-counter! entity delta skill :attack-counter)
      (update-counter! entity delta skill :attack-counter))
    (animation-stopped? (current-animation entity))))

(defn- skill-do-effect [entity {:keys [do-skill] :as skill}]
  (do-skill entity skill))

(defn- skill-try-attack [entity skill delta]
  (when (skill-attack-finished? entity skill delta)
    (skill-do-effect entity skill)
    (when-let [s (:shoot-sound skill)]
      (play-sound s))
    true))

(defn is-usable? [entity skill]
  (if-let [check-usable (:check-usable skill)]
    (check-usable entity skill)
    true))

(defn get-active-skill [{:keys [active-type skills] :as skillmanager}]
  (get skills active-type))

(let [skill-casting-mouse-state (atom nil)]

  (defn- skill-save-casting-mouse-state []
    (reset! skill-casting-mouse-state {:position (get-mouse-pos)
                   :tile-position (get-mouse-tile-pos)
                   :button (cond
                             (is-leftbutton-down?) :left
                             (is-rightbutton-down?) :right)}))

  (defn get-skill-use-mouse-pos [] (:position @skill-casting-mouse-state))
  (defn get-skill-use-mouse-tile-pos [] (:tile-position @skill-casting-mouse-state))
  (defn get-skill-use-mouse-button [] (:button @skill-casting-mouse-state)))

(defn- skill-pay-cost [component cost]
  (update-in component [:mana :current] - cost))

(defn- skill-try-set-active-cooldown
  "Check contains because grenade may remove skill from skillmanager and assoc-in does assoc
   all keys that are not there, which would lead to a skill with just {:state :cooldown}"
  [{active-type :active-type :as skillmanager}]
  (if (contains? (:skills skillmanager) active-type)
    (assoc-in skillmanager [:skills active-type :state] :cooldown)
    skillmanager))

(def ^:private skill-denied-sound (create-sound "bfxr_denied.wav"))

; skillmanager:        READY <-> ATTACKING
; skillmanager skills: READY <-> COOLDOWN
(defn update-component-skillmanager
  [delta {:keys [state choose-skill rotate-after-attack cast-sound] :as component} entity]
  (swap! entity update-in [:skillmanager] skill-update-cooldowns delta)
  (let [is-player (is-player? entity)
        skillmanager (:skillmanager @entity)] ; after update cooldowns.
    (case state
      :ready
      (let [active-type (choose-skill entity skillmanager delta)
            active-skill (get (:skills skillmanager) active-type)]
        (when
          (and
            active-skill
            (if is-player (skill-save-casting-mouse-state) true)
            (is-ready? active-skill)
            (is-usable? entity active-skill)
            (let [enough (enough-mana? active-skill skillmanager)]
              (if (and (not enough) is-player)
                (do
                  (playonce skill-denied-sound)
                  (show-msg-to-player "Not enough mana!")))
              enough))
          (when (and cast-sound (not (:is-melee active-skill)))
            (play-sound cast-sound))
          (swap! entity #(-> % 
            (assoc-in [:skillmanager :active-type] active-type)
            (update-in [:skillmanager] skill-pay-cost (:cost active-skill))
            (switch-state :skillmanager :attacking)))
          (when rotate-after-attack
            (rotate-after-attack entity))))
      :attacking
      (when (skill-try-attack entity (get-active-skill skillmanager) delta)
        (swap! entity #(-> % 
          (update-in [:skillmanager] skill-try-set-active-cooldown)
          (switch-state :skillmanager :ready)))))))

(defn skillmanager-component
  "rotatefn may be nil"
  [& args]
  (let [[{:keys [rotatefn choosefn mana skills cast-sound]} props] (split-key-val-and-maps args)]
    (merge {:type :skillmanager
            :rotate-after-attack rotatefn
            :choose-skill choosefn    ; must return not-nil if attack should be tried
            :mana (min-max-val mana)
            :skills (zipmap (map :type skills) skills)
            :state :ready
            :active-type nil
            :cast-sound cast-sound
            :block-effect-reset-state (fn [entity skillm]
                                        (when (is-attacking? skillm)
                                          (when (:attack-counter (get-active-skill skillm))
                                            (swap! entity update-in [:skillmanager :skills (:active-type skillm) :attack-counter] reset-counter))
                                          (swap! entity #(-> %
                                               (assoc-in [:skillmanager :skills (:active-type skillm) :state] :cooldown)
                                               (switch-state :skillmanager :ready)))))}
           (active update-component-skillmanager)
           props)))

(defn update-component-skill
  [delta {:keys [state] :as skill} entity]
  (when-let [new-state (case state
                         :ready (when (is-usable? entity skill)
                                  :attacking)
                         :attacking (when (skill-try-attack entity skill delta)
                                      :cooldown)
                         :cooldown (when (update-counter! entity delta skill :cooldown-counter)
                                     :ready))]
    (swap! entity switch-state (:type skill) new-state)))

(defn- skill-create-props [cooldown attacktime]
  (merge
    {:state :ready
     :cooldown-counter (make-counter cooldown)}
    (when attacktime
      {:attack-counter (make-counter attacktime)})))

(defn standalone-skill
  [& {:keys [stype cooldown props attacktime state-blocks]}]
  (merge {:type stype
          :block-effect-reset-state (fn [entity skill]
                                      (when (is-attacking? skill)
                                        (when (:attack-counter skill)
                                          (swap! entity update-in [(:type skill) :attack-counter] reset-counter))
                                        (swap! entity switch-state (:type skill) :cooldown)))}
         (active update-component-skill)
         (if state-blocks (blocks-component state-blocks) {})
         (skill-create-props cooldown attacktime)
         props))

; TODO player-skillmanager-skill mit : animation, mousebutton, icon
; (einfach das was benotigt WIRD und was optional ist bei allen item , skillmanager, learnable-skills)
(defn skillmanager-skill [& more]
  (let [[{:keys [stype cooldown cost attacktime]} props] (split-key-val-and-maps more)]
    ; assert has stype cooldown cost? obligatory keys
    ; assert all keys are stype cooldown cost or attacktime? all keys in obligatory or optional?
    (merge {:type stype :cost cost}
           (skill-create-props cooldown attacktime)
           props)))

;; destructible

(defeffectentity create-leech-visuals [body img]
  :target body
  :duration 150
  (image-render-component img :order :air))

(defn calc-effective-spell-dmg [[basemin basemax] percent-modify]
  {:pre [(number? basemin) (number? basemax)]}
  (let [modifier (/ percent-modify 100)]
    [(int (* modifier basemin))
     (int (* modifier basemax))]))

(defn calc-effective-melee-dmg [[basemin basemax] {:keys [min-inc max-inc percent-modify]}]
  {:pre [(number? basemin) (number? basemax)]}
  (let [new-min (+ basemin min-inc)
        new-max (+ basemax max-inc)
        modifier (/ percent-modify 100)]
    [(int (* modifier new-min))
     (int (* modifier (max new-min new-max)))]))

(defn- destructible-try-leech [dmg-dealt]
  (let [{:keys [hp-leech mana-leech]} (:item-boni @player-body)]

    (when-not (zero? hp-leech)
      (let [value (* (/ hp-leech 100) dmg-dealt)]
        (show-gains-hp-effect player-body value)
        (swap! player-body update-in [:destructible :hp] increase-min-max-val value)) ; same code as in hp pot
      (create-leech-visuals player-body (create-image "effects/hp_leech.png")))

    (when-not (zero? mana-leech)
      (let [value (* (/ mana-leech 100) dmg-dealt)]
        (show-gains-mana-effect player-body value)
        (swap! player-body update-in [:skillmanager :mana] increase-min-max-val value)) ; same code as in mana pot
      (create-leech-visuals player-body (create-image "effects/mana_leech.png")))))

;;

(def player-standard-dmg 10) ; entspricht 1.0 multiplier monster hp also 1 schlag tot (durchschnitt)

(def player-start-hp 100)

(def monster-standard-dmg 5) ; 20 default hits to kill player

(let [armor-point-avg-reduce 0.1]

  (defn- destructible-armor-variant-dmg-reduce [armor]
    (variance-val
      (* armor-point-avg-reduce armor)
      0.2))

  (defn get-armor-reduce-info [armor]
    (str "Damage reduced by "
      (variance-val-str
        (destructible-armor-variant-dmg-reduce armor))))

  (defn- destructible-monster-armor [perc-reduce]
    (int ; TODO wieso int hier und bei player-armor net?
      (*
        (/ perc-reduce 100)
        player-standard-dmg
        (/ 1 armor-point-avg-reduce))))

  (defn get-player-armor "Reduces perc of standard-monsterdmg"
    [perc-reduce]
    (*
      (/ perc-reduce 100)
      monster-standard-dmg
      (/ 1 armor-point-avg-reduce))))

; TODO depends body because destructible are applied body-hit-effects?
(defn destructible-component [hp armor]
  {:pre [(>= hp 0) (>= armor 0)]}
  {:type :destructible
   :hp (min-max-val hp)
   :armor armor
   :init (fn [entity]
           ; hp-bar brightness corresponds to image corner brightness
           (assert (or
                     (:image-render @entity)
                     (:animation @entity))))})

(defn destructible? [body] (:destructible @body))

(defn deal-dmg-allowed? [body affected-side] ; TODO also the same @ get-destructible-bodies... -> side always only 1 ??
  (and (destructible? body)
       (= (:side (:body @body)) affected-side)
       (is-affectable? body)))

(defn attackable-by-player? [body]
  (deal-dmg-allowed? body :monster))

; TODO is-affectable ... why here? I want destructible bodies.,.. does too many things...
(defn get-destructible-bodies [position radius side]
  (let [affects-side (if (keyword? side) #{side} (set side))]
    (filter #(and (destructible? %)
                  (affects-side (:side (:body @%)))
                  (is-affectable? %))
      (get-touched-bodies position radius))))

(defn get-hp [body] (:hp (:destructible @body)))
(defn get-armor [body] (:armor (:destructible @body)))

(defn is-dead? [body] (:is-dead (:destructible @body)))

; TODO make 2 functions -> simpler? like this more complected
(defn set-hp-to-max [body]
  (when (lower-than-max? (get-hp body))
    (show-gains-hp-effect body (rest-to-max (get-hp body))))
  (swap! body update-in [:destructible :hp] set-to-max))

(defn monster-destructible [hp-multiplier armor-perc-reduce]
  (destructible-component
    (rand-float-between
      (variance-val (* hp-multiplier player-standard-dmg) 0.5))
    (destructible-monster-armor armor-perc-reduce)))

(alter-var-root #'init-fns conj
  (fn []
    (def explosion-frames (spritesheet-frames "effects/expsmall.png" 20 20))))
(alter-var-root #'init-fns conj
  (fn []
    (def ^:private destructible-blood-frames (spritesheet-frames "effects/blood.png" 20 20))))

(defn- destructible-create-hit-effect [body]
  (if-let [{trigger :trigger} (:hit-effect @body)]
    (trigger body)
    (animation-entity
      :position (:value (:position @body))
      :animation (create-animation (if (is-player? body)
                                     destructible-blood-frames
                                     explosion-frames)))))

(defn- destructible-show-dmg-effect [body dmg]
  (show-string-effect body 1000 (rgbcolor :r 1) (str (int dmg) "!")))

(defn- destructible-dealt-dmg-triggers [target-body actual-dmg is-crit-hit? lethal is-player-melee]

  ; hit something else triggers
  (when is-player-melee
    ((:dealt-melee-dmg-effect (:item-boni @player-body)) target-body)
    (destructible-try-leech actual-dmg))

  ; being hit by something else triggers
  (when-let [{trigger :do} (:dealt-dmg-trigger @target-body)]
    (trigger target-body lethal))
  (when-not lethal
    (when (is-player? target-body)
      (shake-screen))
    (destructible-create-hit-effect target-body)
    (play-sound
      (cond (is-player? target-body) "bfxr_playerhit.wav"
            is-crit-hit? "kabloom.wav"
            :else "bfxr_normalhit.wav"))))

(defn- destructible-critical-hit-dmg [dmg target-body]
  (let [dmg (* dmg 3)]
    (destructible-show-dmg-effect target-body dmg) ; TODO belongs to dealt-dmg-triggers & unexpected side effect ?
    dmg))

(defn- destructible-dmg-data-ok? [{mi 0 mx 1 :as dmg}]
  (and (vector? dmg)
       (= (count dmg) 2)
       (integer? mi)
       (integer? mx)
       (>= mi 0)
       (>= mx 0)
       (>= mx mi)))

(defn- destructible-calc-deal-dmg [dmg is-player-melee is-player-spell target-body]
  (let [dmg (cond (destructible-dmg-data-ok? dmg) dmg
                  (number? dmg) [dmg dmg]
                  :else (println "Dmg-data notokay: " dmg))
        {:keys [melee-crit spell-crit percent-modify-spell] :as item-boni} (:item-boni @player-body)
        ; +percent% increases
        dmg (if is-player-spell (calc-effective-spell-dmg dmg percent-modify-spell) dmg)
        dmg (if is-player-melee (calc-effective-melee-dmg dmg item-boni) dmg)
        ; random dmg
        dmg (rand-int-between dmg)
        ; critical hit check
        is-melee-crit (and is-player-melee (percent-chance melee-crit))
        dmg (if is-melee-crit (destructible-critical-hit-dmg dmg target-body) dmg)
        is-spell-crit (and is-player-spell (percent-chance spell-crit))
        dmg (if is-spell-crit (destructible-critical-hit-dmg dmg target-body) dmg)]
    [dmg (or is-melee-crit is-spell-crit)]))

(defn- destructible-apply-armors [dmg target-body]
  (if (shield-try-consume-damage target-body)
    (play-sound "bfxr_shield_consume.wav")
    (let [rest-dmg (->> target-body get-armor destructible-armor-variant-dmg-reduce rand-float-between (- dmg) (max 0))]
      (if (and (not (zero? dmg)) (zero? rest-dmg))
        (play-sound "bfxr_armorhit.wav")
        rest-dmg))))

(defn deal-dmg [dmg target-body & {:keys [is-player-melee is-player-spell]}]
  {:pre [(destructible? target-body)]}
  (when-not (is-dead? target-body) ; kill only once, call triggers only once
    (wake-up target-body)  ; hit-trigger even if armor absorbs dmg
    (let [[dmg is-crit-hit?] (destructible-calc-deal-dmg dmg is-player-melee is-player-spell target-body)]
      (when-let [dmg (destructible-apply-armors dmg target-body)]
        (let [current-hp (:current (get-hp target-body))
              new-hp (- current-hp dmg)
              lethal (<= new-hp 0)
              actual-dmg (if lethal current-hp dmg)]
          ; (show-dmg-effect target-body actual-dmg)
          (swap! target-body assoc-in [:destructible :hp :current] (max 0 new-hp))
          (destructible-dealt-dmg-triggers target-body actual-dmg is-crit-hit? lethal is-player-melee)
          (when lethal
            (swap! target-body assoc-in [:destructible :is-dead] true)
            (when-not (is-player? target-body)
              (add-to-removelist target-body))))))))

;; movement

(defn- movement-create-new-position
  [delta [vx vy] [old-x old-y] speed]
  [(+ old-x (* vx speed delta))
   (+ old-y (* vy speed delta))])

(defn try-move-projectile
  "returns true if blocked, nil if moved."
  [new-posi projectile]
  (let [id (:id (meta projectile))
        half-w (:half-width (:body @projectile))
        half-h (:half-height (:body @projectile))
        {:keys [hit-effects hits-side already-hit-bodies piercing hits-wall-effect]} (:projectile-collision @projectile)
        touched-cells (calc-touched-cells new-posi half-w half-h)
        hit-body (first (filter #(and (not (contains? already-hit-bodies %))
                                   (= hits-side (:side (:body @%))))
                             (get-other-solid-bodies new-posi half-w half-h id touched-cells)))
        movement-type (:movement-type (:movement @projectile))
        ; TODO PROJECTILE COLLISION to entityPROJECTILE and out of movement logic?
        blocked (cond hit-body
                      (do
                        (swap! projectile update-in [:projectile-collision :already-hit-bodies] conj hit-body)
                        (dorun (map #(% hit-body) hit-effects))
                        (not piercing))

                      (some #(cell-blocked? % movement-type) touched-cells) ; TODO mit x-axis und y-axis -> 2-3 abfragen anstatt 4. Lohnt?
                      (do
                        (hits-wall-effect new-posi)
                        true))]
    (when blocked (add-to-removelist projectile) true)))

(defn- movement-update-position-projectile
  [delta v x-direction y-direction speed body]
  (let [new-position (movement-create-new-position delta v (:value (:position @body)) speed)
        blocked (try-move-projectile new-position body)]
    (when-not blocked
      (swap-position! body new-position))
    blocked))

; TODO gleicher code wie bei components.body/blocked-location? -> fuge zusammen
(defn- movement-blocked-for-body? [body movement-type noclip position touched-cells]
  (or
    (some #(cell-blocked? % movement-type) touched-cells)
    (if-not noclip
      ; checks if colliding with other solid bodies -> rename?
      (seq (get-other-solid-bodies position body touched-cells)))))

(comment
  ; fur testen wie oft try-moved/update-posi-def average ist. -> so 1.4 ca.
  (def try-moved (atom 0))
  (def update-posi-def (atom 0))
  (require '[cdq.core :refer [ingame-loop-comp]])
  (require '[cdq.core :refer [rendering]])
  (ingame-loop-comp :debug-infos
    (rendering :above-gui [g c]
      (render-readable-text g 50 90 (str "try-moved  " @try-moved))
      (render-readable-text g 50 110 (str "update-posi-def " @update-posi-def))
      (render-readable-text g 50 130 (str "avg " (if-not (zero? @update-posi-def) (float (/ @try-moved @update-posi-def))))))))

; TODO get-cells performance... -> mit cached-adj-cells ersetzen wenns sein soll
; ~ 1,7 1,8 % der runtime ... _> could be improved...

; auch vlt testen ob dieses remaining... zeug nen unterschied macht
; ebenfalls testen ob threshhold nen unterschied macht...
; -> best. anzahl an gegnern spawnen und auch vorgegebenem weg den spieler bewegen & messen?
; -> um vergleichen zu konnen ... also erst 3x schauen ob gleiche ergebnisse ...
; oder erstmal so lassen .... und auf wichtigeres konzentrieren
(defn- movement-try-move [body v xdir ydir delta speed movement-type noclip]
  (let [new-posi (movement-create-new-position delta v (:value (:position @body)) speed)
        to-check-tiles (get-to-check-tiles body new-posi xdir ydir) ; rename new-tiles, new-cells ?
        to-check-cells (get-cells to-check-tiles)
        blocked (movement-blocked-for-body? body movement-type noclip new-posi to-check-cells)]
    (when-not blocked
      ; manual :posi-changed of solid body because of performance issues (calc-touched-cells)
      (let [all-tiles (apply get-touched-tiles (get-body-bounds body)) ; == all-previously-touched-tiles?
            remaining (get-cells (set/difference (set all-tiles) to-check-tiles))] ; == all that remain to be checked?
        ; to-check-cells not nil because not blocked
        ; remaining not nil because not moving there -> update-touched-cells
        (update-touched-cells body (concat remaining to-check-cells)))
      (swap-position! body new-posi :filter-body true) ; TODO does more than swap-position also calls posi-changed..
      (update-occupied-cell body) ; update after body has changed position! touched cells already uses cells corresponding to new position
      v)))

(defn- movement-calc-threshold
  "Until entity is in the tile next to target and does not need to slide anymore"
  [half-size] ; assert entity half-w = half-h
  (Math/abs (.y (normalise (vector2f
                              (- 0.5 (inc half-size))
                              (- 0.5 half-size))))))

(defn- movement-update-position-solid [delta {vx 0 vy 1 :as v} xdir ydir speed body]
  (let [threshold (movement-calc-threshold (:half-width (:body @body)))
        movement-type (:movement-type (:movement @body))
        noclip (:noclip (:movement @body))
        success (or
                  (movement-try-move body v xdir ydir delta speed movement-type noclip)
                  (when (or (is-player? body) (> (Math/abs (float vx)) threshold))
                    (movement-try-move body [xdir 0] xdir ydir delta speed movement-type noclip))
                  (when (or (is-player? body) (> (Math/abs (float vy)) threshold))
                    (movement-try-move body [0 ydir] ydir ydir delta speed movement-type noclip)))]
    (when success
      ; hier nicht rotiert um bewegten vektor(success) sondern um gewunschten vektor da sonst zittern die monster herum
      (when-let [rotation (:rotation @body)]
        ((:moved rotation) body v))
      (when (is-player? body)
          (swap! body assoc-in [:movement :play-move-animation] true)))))

(defn- movement-update-component
  [delta {:keys [control-update speed] :as c} body]
  (let [v (control-update body c delta)]
    (assert (or (nil? v)
                (zero? (vector-length v))
                (normalised? v)))
    (when (and v
               (not (zero? (vector-length v))))
      (let [{vx 0 vy 1 :as v} (vec-posi v)
            xdir (Math/signum (float vx))
            ydir (Math/signum (float vy))
            update-position (if (:projectile-collision @body)
                              movement-update-position-projectile
                              movement-update-position-solid)]
        (update-position delta v xdir ydir speed body)))))

; max speed damit kleinere bodies beim direkten druberfliegen nicht ubersprungen werden (an den ecken werden sie trotzdem ubersprungen..)
; schnellere speeds mussten in mehreren steps sich bewegen.
; 0.0108 tile/ms
; 520 px/s
(def max-speed (* 1000 (/ min-solid-pxsize max-delta)))

(defn in-tiles-per-ms [px-per-s] (/ px-per-s 1000 tile-width))

; speed is pixels/s
(defn movement-component [control speed movement-type]
  {:pre [(#{:air :ground} movement-type)
         (>= speed 1) ; just to check that its now in px/s
         (<= speed max-speed)]}
  (safe-merge
    {:type :movement
     :movement-type movement-type
;   :init (fn [entity]
;           ;  (not asserted becuz burrowed are burrowing @init... so not solid anymore)
;           (assert (or (:projectile-collision @entity) ;update-position-projectile
;                       (is-solid? entity)))) ;update-position-solid
     :play-move-animation false
     :speed (in-tiles-per-ms speed)}
    (active movement-update-component)
    control))

(defn projectile-movement-component [move-vector speed]
  (movement-component
    {:control-update (constantly move-vector)}
    speed
    :air))

;; ingame-gui

(def ^:private ingame-gui-controls-hotkey :H)
(def skillmenu-hotkey :S)
(def char-hotkey :C)
(def inventory-hotkey :I)
(def options-hotkey :ESCAPE) ; ist nicht nur options sondern auch wenn tod / throw item away hotkey

(def ingamestate-display (make-guidisplay))

(ingame-loop-comp :display
  (active [delta c _]
   (update-guicomponent ingamestate-display))
  (rendering :selfmade-gui [g c]
    (render-guicomponent g ingamestate-display)))

(def ingame-gui-background-color lightGray)
(def ingame-gui-foreground-color (.brighter ingame-gui-background-color 0.5))

(def frame-screenborder-distance 5)

(defn- ingame-gui-find-component [display name]
  (first (filter #(= (:name @%) name) (:children @display))))

(defn- ingame-gui-get-all-frames []
  (filter #(:is-frame @%) (:children @ingamestate-display)))

(defn some-visible-frame? []
  (some gui-visible? (ingame-gui-get-all-frames)))

(defn close-all-frames []
  (dorun (map #(gui-set-visible % false) (ingame-gui-get-all-frames))))

(defn mouse-inside-some-gui-component? []
  (some
    #(and (gui-visible? %) (gui-mouseover? @%))
    (:children @ingamestate-display)))

;; Help/Controls

; -> only open for first starting of the game or for every character only one time and save with save-game

(def ^:private ingame-gui-controls-text
  "* Moving: Leftmouse
* Use skills: Left & right mouse
* Set Skill hotkey: Press 0-9 while hovering
over a skill at the bottom left selection lists.
* Use items in actionbar: Q,W and E.
* Minimap: TAB")

(alter-var-root #'init-fns conj
  (fn []
    (def ^:private ingame-gui-controlsframe (let [w ;(+ 10 (get-text-width ingame-gui-controls-text)) TODO FIXME
                              320]
                          (make-frame :name :controls
                                      :bounds [frame-screenborder-distance
                                               212
                                               w
                                               (+ 2 (get-text-height ingame-gui-controls-text))]
                                      :hotkey ingame-gui-controls-hotkey
                                      :visible (get-setting :show-controls-frame)
                                      :parent ingamestate-display)))
    (make-label :location [2 2]
                :text ingame-gui-controls-text
                :parent ingame-gui-controlsframe)))

(def buttonx-start 60) ; rechts neben skill-selection-buttons
(def ingame-gui-x-dist 18) ; mit get-mouse-pos rausgefunden

(def buttonscale [16 16])

(defn- ingame-gui-hotkey-str [hotkey s]
  (let [hkname (name hotkey)
        markedhkname (str "[" hkname "]")]
    (if (.contains s hkname)
      (.replace s hkname markedhkname)
      (str markedhkname s))))

; -1 location: free sp button

(alter-var-root #'init-fns conj
  (fn []
    (make-imgbutton
      :image (create-image "icons/character.png" :scale buttonscale)
      :location [(+ buttonx-start (* ingame-gui-x-dist 0)) (- screen-height 18)]
      :pressed #(gui-switch-visible (ingame-gui-find-component ingamestate-display :character))
      :tooltip (ingame-gui-hotkey-str char-hotkey "Character Attributes")
      :parent ingamestate-display)

    (make-imgbutton
      :image (create-image "icons/skills.png" :scale buttonscale)
      :location [(+ buttonx-start (* ingame-gui-x-dist 1)) (- screen-height 18)]
      :pressed #(gui-switch-visible (ingame-gui-find-component ingamestate-display :skillmenu))
      :tooltip (ingame-gui-hotkey-str skillmenu-hotkey "Skills")
      :parent ingamestate-display)

    (make-imgbutton
      :image (create-image "icons/inventory.png" :scale buttonscale)
      :location [(+ buttonx-start (* ingame-gui-x-dist 2)) (- screen-height 18)]
      :pressed #(gui-switch-visible (ingame-gui-find-component ingamestate-display :inventory))
      :tooltip (ingame-gui-hotkey-str inventory-hotkey "Inventory")
      :parent ingamestate-display)

    (make-imgbutton
      :image (create-image "icons/controls.png" :scale buttonscale)
      :location [(+ buttonx-start (* ingame-gui-x-dist 3)) (- screen-height 18)]
      :pressed #(gui-switch-visible (ingame-gui-find-component ingamestate-display :controls))
      :tooltip (ingame-gui-hotkey-str ingame-gui-controls-hotkey "Game Controls/Hotkeys")
      :parent ingamestate-display)

    (make-imgbutton
      :image (create-image "icons/options.png" :scale buttonscale)
      :location [(+ buttonx-start (* ingame-gui-x-dist 4)) (- screen-height 18)]
      :pressed #(enter-state options)
      :tooltip (ingame-gui-hotkey-str options-hotkey "Options/Exit")
      :parent ingamestate-display)))

;; item grids

(defn- item-grid-create-empty-cell [posi allows-type grid equipment]
  {:item (atom nil)
   :is-equipment equipment
   :allows-type allows-type
   :posi posi
   :grid-type grid})

; 'inherits' from VectorGrid and just valAt changed...
; other ideas: item-grid data as metadata of VectorGrid
; protocol ItemGrid extends VectorGrid (get-rx (get-grid-type etc?
; separate [item-grid item-grid-data]
(deftype ItemGrid [grid2d m]
  Grid2D
  (cells [this] (cells grid2d))
  (width [this] (width grid2d))
  (height [this] (height grid2d))

  clojure.lang.Seqable
  (seq [this] (seq grid2d))

  clojure.lang.ILookup
  (valAt [this k]
    (if (keyword? k)
      (k m)
      (.valAt grid2d k))))

(def item-grids {})

(defn add-item-grid [& {:keys [w h rx ry allows-type grid-type is-equipment-cell visible-check]
                        :as argsmap}]
  {:pre [(not-any? #{grid-type} (keys item-grids))]}
  (alter-var-root #'item-grids assoc grid-type
                  (ItemGrid. (create-grid w h #(item-grid-create-empty-cell % allows-type grid-type is-equipment-cell))
                             (select-keys argsmap [:visible-check :grid-type :rx :ry :allows-type]))))

(def ^:private item-grid-inventory-cells-x 6)
(def ^:private item-grid-inventory-cells-y 4)

(def item-cell-w 17) ; cells = item-size+1 so items fit inside grid lines, else top and left pixel line of items not visible
(def item-cell-h 17)

(def ^:private item-grid-borderpx 2)
(def ^:private item-grid-inventory-width (+ (* 2 item-grid-borderpx)
                         (* item-grid-inventory-cells-x item-cell-w)))
(def inventory-height (+ (* 2 item-grid-borderpx)
                          (* 2 item-cell-h)
                          (* item-grid-inventory-cells-y item-cell-h)))

(def ^:private item-grid-inventoryrx (- screen-width item-grid-inventory-width frame-screenborder-distance))
(def inventoryry frame-screenborder-distance)

(alter-var-root #'init-fns conj
  (fn []
    (def inventory-frame (make-frame :name :inventory
                                     :bounds [item-grid-inventoryrx
                                              inventoryry
                                              item-grid-inventory-width
                                              inventory-height]
                                     :hotkey inventory-hotkey
                                     :visible false
                                     :parent ingamestate-display))))

(defn showing-player-inventory? [] (gui-visible? inventory-frame))

(add-item-grid
  :grid-type :belt
  :w 3 ; add cells -> add hotkeys
  :h 1
  :rx (- screen-width (* 3 item-cell-w) 1) ; -1 because rendering exactly at screen-height the bottom&right line will not be seen
  :ry (- screen-height item-cell-h 1)
  :allows-type :usable
  :is-equipment-cell true
  :visible-check (constantly true))

(add-item-grid
  :grid-type :inventory
  :w item-grid-inventory-cells-x
  :h item-grid-inventory-cells-y
  :rx (+ item-grid-inventoryrx item-grid-borderpx)
  :ry (+ inventoryry item-grid-borderpx (* 2 item-cell-h))
  :allows-type :all
  :is-equipment-cell false
  :visible-check showing-player-inventory?)

(alter-var-root #'init-fns conj
  (fn []
    (def item-background-icons {:torso (create-image "items/armorbg.png")
                           :hands (create-image "items/handsbg.png")
                           :implants (create-image "items/implantsbg.png")})))

(add-item-grid
  :grid-type :hands
  :w 1
  :h 1
  :rx (+ item-grid-inventoryrx item-grid-borderpx)
  :ry (+ inventoryry item-grid-borderpx)
  :allows-type :hands
  :is-equipment-cell true
  :visible-check showing-player-inventory?)

(add-item-grid
  :grid-type :torso
  :w 1
  :h 1
  :rx (+ item-grid-inventoryrx item-grid-borderpx item-cell-w)
  :ry (+ inventoryry item-grid-borderpx)
  :allows-type :torso
  :is-equipment-cell true
  :visible-check showing-player-inventory?)

(add-item-grid
  :grid-type :implants
  :w 1
  :h 1
  :rx (+ item-grid-inventoryrx item-grid-borderpx)
  :ry (+ inventoryry item-grid-borderpx item-cell-h)
  :allows-type :implant
  :is-equipment-cell true
  :visible-check showing-player-inventory?)

(def hks-cells
  {:Q [0 0]
   :W [1 0]
   :E [2 0]})

;; item cells

(defn get-equiped-hands-item []
  @(:item (get (:hands item-grids) [0 0]))) ; (-> item-grids :hands [0 0])

(defn cell-allows-item? [{allows :allows-type :as cell} item]
  (or
    (= :all allows)
    (= (:type item) allows)))

(defn cell-empty-and-allows-item? [cell item]
  (and
    (nil? @(:item cell))
    (cell-allows-item? cell item)))

(defn cell-filled-and-allows-item? [cell item]
  (and
    @(:item cell)
    (cell-allows-item? cell item)))

(defn inc-count-of-item? [item1 item2]
  (and
    item1
    (:count item1)
    (:count item2)
    (= (:name item1) (:name item2))))

(defn- item-cell-check-apply-boni [item cell k]
  (let [components player-body]
    (when (:is-equipment cell)
      (swap! components thread-through (map k (:equip-boni item)))
      (when-let [equip (k item)]
        (swap! components equip)))))

(defn add-item-to-cell [item cell]
  {:pre [(cell-empty-and-allows-item? cell item)]}
  (reset! (:item cell) item)
  (item-cell-check-apply-boni item cell :equip))

(defn remove-item-from-cell [cell]
  (let [item @(:item cell)] ; get the item bevor setting it nil so that :unequip works!
    (reset! (:item cell) nil)
    (item-cell-check-apply-boni item cell :unequip)))

(defn empty-all-item-grids []
  (doseq [grid (vals item-grids)
          cell (cells grid)]
      (remove-item-from-cell cell)))

(defn remove-one-item [cell]
  (let [item @(:item cell)]
    (if-let [cnt (:count item)]
      (if (> cnt 1)
        (swap! (:item cell) update-in [:count] dec)
        (remove-item-from-cell cell))
      (remove-item-from-cell cell))))

(defn remove-one-item-from [grid-type item-name]
  {:pre [(some #{item-name}
           (map #(:name @(:item %))
             (cells (:belt item-grids))))]}
  (remove-one-item
    (first (filter
      #(= item-name (:name @(:item %)))
      (cells (:belt item-grids))))))

(defn item-not-in-use? [cell]
  (let [item @(:item cell)
        skillmanager (:skillmanager @player-body)
        active-skill (get-active-skill skillmanager)
        in-use (and
                 (:is-equipment cell) item (is-attacking? skillmanager)
                 (or
                   (and (:skill item) (= active-skill (:skill item)))
                   (and (:melee-weapon item) (= (get-equiped-hands-item) item) (:is-melee active-skill))))]
    (not in-use)))

(defn item-in-use? [cell]
  (not (item-not-in-use? cell)))

(defn get-inventory-cells-with-item-name [item-name grid-type]
  (filter
    #(and
       @(:item %)
       (= (:name @(:item %)) item-name))
    (cells (grid-type item-grids))))

(defn- item-try-put-in
  "returns true when the item was picked up"
  [picked-item grid-type]
  (let [item-cells (cells (grid-type item-grids))
        cell-with-same-item (first
                              (get-inventory-cells-with-item-name (:name picked-item) grid-type))
        picked-up (if
                    (and
                      cell-with-same-item
                      (:count @(:item cell-with-same-item)))
                    (swap! (:item cell-with-same-item) update-in [:count] + (:count picked-item))
                    (if-let [free-cell (first (filter #(nil? @(:item %)) item-cells))]
                      (do
                        (add-item-to-cell picked-item free-cell)
                        true)
                      false))]
    picked-up))

(defn try-pickup-item [item]
  (or
    (when-let [grid-type (:grid-type
                           (first (filter #(= (:allows-type %) (:type item))
                             (vals item-grids))))]
      (item-try-put-in item grid-type)) ; 1. try
    (item-try-put-in item :inventory))) ; 2. try

;; mouseoverbody

(defn- mouseover-get-current []
  (when-not (mouse-inside-some-gui-component?)
    (let [tile-posi (get-mouse-tile-pos)
          hits (get-bodies-at-position tile-posi)]
      (when hits
        (let [; der als letztes gerenderte ist ganz am obersten
              ; jeder body hat :rendering also brauch net removen
              hits (filter on-screen-and-in-sight?
                           (reverse
                             (sort-by-order hits
                                            #(:order (first (filter :rendering (vals @%))))
                                            render-on-map-order)))
              ;_ (println "hits: " (map #(map :type (get-components %)) hits))
              hits (if (> (count hits) 1) (remove is-player? hits) hits)]
          (first hits))))))

(def ^:private mouseover-body-cache (atom nil)) ; cache exists so that only one time in a frame mouseover-get-current is calculated
(def mouseover-cache-session (atom-session mouseover-body-cache :save-session false))

; when played holds leftmouse down the mouseoverbody stays the same as long as mouse is down, even if it is not under the mouse anymore
(def saved-mouseover-body (atom nil))
(def saved-mouseover-body-session
  (atom-session saved-mouseover-body :save-session false))

(defn get-mouseover-body []
  (if-let [body @saved-mouseover-body]
    (when (get @id-entity-map (:id (meta body))) body)
    (if @debug-mode
      (mouseover-get-current)
      (when (and @mouseover-body-cache (get @id-entity-map (:id (meta @mouseover-body-cache)))) @mouseover-body-cache))))

; -> der map-indep-comp :check-pressable-mouseoverbody benutzt get-mouseover-body und konnte vor
; update-saved drankommen!
; deshalb nur returnt wenn er exists.

(defn- mouseover-keep-saved? [body]
  (and (is-leftbutton-down?)
       (get @id-entity-map (:id (meta body)))
       (on-screen-and-in-sight? body)))

(ingame-loop-comp :update-saved-and-cache
  (active [_ _ _]
    (reset! mouseover-body-cache (mouseover-get-current))
    (when (and @saved-mouseover-body
               (not (mouseover-keep-saved? @saved-mouseover-body)))
      (reset! saved-mouseover-body nil))
    (when-let [body @mouseover-body-cache]
      (when (and (is-leftbutton-down?)
                 (not @saved-mouseover-body)
                 (attackable-by-player? body))
        (reset! saved-mouseover-body body)))))

;; melee skills

;;; front-of-body-shape

(defn in-front-of-body-shape
  ([body height]
   (in-front-of-body-shape (:value (:position @body))
                           (:half-width (:body @body))
                           height
                           (:angle (:rotation @body))))
  ([[x y] half-w height angle]
   (rotate-shape
    (rectangle (- x half-w)
                    (- y half-w height)
                    (* 2 half-w)
                    height)
    angle x y)))

;;; melee

; TODO unabh von tiles machen ->in pixels?
(def melee-puffer 0.8)

(let [pixel 5]
  (def start-melee-puffer (in-tiles pixel)))

(comment
  "visual \"close-ness\" required for melee attacks
if an entity is very fast and the movement step in 1 frame is >as this then
it can not get in melee range -> in this case split up movement into multiple smaller steps
so the entity can get in melee range.")

(defn body-in-melee-range?
  "attacked-already weil wenn noch nicht @attacking state sondern schauen ob in range -> kleinerer abstand.
   spater grosserer abstand moglich weil sonst bewegt sich gegner 1 frame weg  und ausser range .."
  ([body target]
    (body-in-melee-range? body target true))
  ([body target attacked-already] ; TODO als :attacked-already key machen
    (let [puffer (if attacked-already melee-puffer start-melee-puffer)
          radius (+ puffer (:half-width (:body @body)))] ; TODO geht davon aus half-w = half-h von body (ansonsten komplizierter!)
      (circle-collides? (:value (:position @body)) radius target))))
; TODO vlt schneller einfach distance und den bounding radius der beiden abziehen
; == abstand und mit start-melee-puffer vergleichen

(defn- melee-target-in-range? [entity target-id & {:keys [attacked-already]}]
  (body-in-melee-range? entity (get @id-entity-map target-id) attacked-already))

(defn- melee-check-usable-player [entity {:keys [target-body-id] :as melee-comp}]
  (let [mouseover-target-id (when-let [body (get-mouseover-body)]
                              (when (attackable-by-player? body)
                                (:id (meta body))))]
    (case (get-skill-use-mouse-button)
      :left (when mouseover-target-id
              (swap! entity assoc-in [:skillmanager :skills (:type melee-comp) :target-body-id]
                         mouseover-target-id)
              (melee-target-in-range? entity mouseover-target-id :attacked-already false))
      :right (do
               (swap! entity assoc-in [:skillmanager :skills (:type melee-comp) :target-body-id]
                          (or mouseover-target-id nil))
               true))))

(defn- melee-check-usable-monster [entity melee-comp]
  (melee-target-in-range? entity (:target-body-id melee-comp) :attacked-already false))

(defn melee-weapon
  ([base-dmg hit-sound]
    {:base-dmg base-dmg :hit-sound hit-sound})
  ([base-dmg hit-sound animation]
    (assoc (melee-weapon base-dmg hit-sound) :animation animation)))

(def ^:private melee-fist-weapon
  (melee-weapon [1 2] (create-sound "bfxr_fisthit.wav") :fist))

(defn- melee-current-player-weapon []
  (or (:melee-weapon (get-equiped-hands-item)) melee-fist-weapon))

(defn- melee-weapon-for [entity melee-skill]
  (if (is-player? entity)
    (melee-current-player-weapon)
    (:melee-weapon melee-skill)))

(defn- melee-hit [entity meleecomp targetbody]
  (let [{:keys [base-dmg hit-sound]} (melee-weapon-for entity meleecomp)]
    (deal-dmg base-dmg targetbody :is-player-melee (is-player? entity))
    (dorun (map #(% targetbody) (:hit-effects meleecomp)))
    ;(play-sound hit-sound)
    ))

(defn- melee-try-hit-target
  "returns nil when target not in range or does not exist anymore"
  [entity {:keys [target-body-id] :as meleecomp}]
  (when target-body-id
    (let [targetbody (get @id-entity-map target-body-id)]
      (if (and targetbody
               (is-affectable? targetbody)
               (melee-target-in-range? entity target-body-id :attacked-already true))
        (melee-hit entity meleecomp targetbody)
        (comment "missed target!")))))

(defn- melee-create-props [target-body-id]
  {:target-body-id target-body-id
   :is-melee true
   :do-skill melee-try-hit-target})

(defn get-attackable-target-in-front [body]
  (let [half-w-max (+ (:half-width (:body @body)) melee-puffer)
        posi (:value (:position @body))
        cshape (in-front-of-body-shape body melee-puffer)]
    (first (filter #(and (attackable-by-player? %)
                      (collides? cshape (rect-shape %)))
                (get-bodies-from-cells
                  (calc-touched-cells posi half-w-max half-w-max))))))

(defn- melee-do-skill-player [entity {:keys [target-body-id] :as meleecomp}]
  (let [mouse (get-skill-use-mouse-button)]
    (case mouse
      :left
      (melee-try-hit-target entity meleecomp)

      :right
      (when-not (and target-body-id (melee-try-hit-target entity meleecomp))
        (when-let [target (get-attackable-target-in-front entity)]
          (melee-hit entity meleecomp target))))))

(defn player-melee-props [hit-effects]
  (merge
    (melee-create-props :undefined-yet)
    {:do-skill melee-do-skill-player
     :hit-effects hit-effects
     :check-usable melee-check-usable-player}))

(defn monster-melee-props [target-body-id melee-weapon]
  (merge
    (melee-create-props target-body-id)
    {:melee-weapon melee-weapon
     :check-usable melee-check-usable-monster}))

(defn get-current-player-melee-dmg []
  (-> (melee-current-player-weapon) :base-dmg (calc-effective-melee-dmg (:item-boni @player-body))))

(defn monster-melee-component
  [& {:keys [cooldown hit-sound target-id attacktime]}]
  (standalone-skill
    :stype :melee
    :cooldown cooldown
    :attacktime attacktime
    :state-blocks {:attacking :movement
                   :cooldown :movement}
    :props (monster-melee-props target-id (melee-weapon [4 6] hit-sound))))

(defn get-player-skill-animation-key []
  (let [skill (get-active-skill (:skillmanager @player-body))]
    (:animation
      (if (:is-melee skill) (melee-current-player-weapon) skill))))

;; body-render

(defn- body-render-melee-debug-info [g p body]
  (when-let [meleecomp (get-skill body :melee)]
    (let [target-body (get @id-entity-map (:target-body-id meleecomp))
          start-attack-puffer (in-pixel (+ start-melee-puffer (:half-width (:body @body))))
          hit-puffer (in-pixel (+ melee-puffer (:half-width (:body @body))))]
      (when target-body
        (set-color g (if (body-in-melee-range? body target-body false) red green))
        (draw-shape g (circle p start-attack-puffer))
        (draw-shape g (circle p hit-puffer))))))

(defn- body-render-debug-info [g [x y] body]
  (when (and @debug-mode show-body-debug-info)
    ;(render-readable-text g x y :shift false (str (get-id body)))
    ;(body-render-melee-debug-info g [x y] body)
;    (when-let [c (get-skill body :melee)]
;      (render-readable-text g x y :shift false (str (:state c))))
    (when-let [rangedcomp (get-skill body :ranged)]
      (render-readable-text g x y :shift false (str (:state rangedcomp))))))

(defn- body-render-bounds [g body [x y] color]
  (let [h-width (:half-pxw (:body @body))
        h-height (:half-pxh (:body @body))
        x (- x h-width)
        y (- y h-height)]
    (draw-rect g x y (* 2 h-width) (* 2 h-height) color)))

(def ^:private body-render-hpbar-colors
  {:green     (rgbcolor :g 1 :darker 0.2)
   :darkgreen (rgbcolor :g 1 :darker 0.5)
   :yellow    (rgbcolor :r 1 :g 1 :darker 0.5)
   :red       (rgbcolor :r 1 :darker 0.5)})

(defn- body-render-hpbar-color [ratio]
  (let [ratio (float ratio)
        color (cond
                (> ratio 0.75) :green
                (> ratio 0.5)  :darkgreen
                (> ratio 0.25) :yellow
                :else          :red)]
    (color body-render-hpbar-colors)))

(defn- body-render-bar
  ([g x y w h ratio color]
    (fill-rect g x y w h black)
    (fill-rect g (inc x) (inc y) (* ratio (- w 2)) (- h 2) color))
  ([g x y w h current-val max-val color]
    (body-render-bar g x y w h (get-ratio current-val max-val) color)))

(def ^:private body-render-info-bars-h 3)

(defn- body-render-info-bar [g body [x y] ratio color ybuffer]
  (let [half-w (int (:half-pxw (:body @body)))
        half-h (int (:half-pxh (:body @body)))
        h body-render-info-bars-h]
    (body-render-bar g (- x half-w) (- y half-h ybuffer) (* 2 half-w) h ratio color)))

(defn- body-render-hp-bar [g body render-position]
  (let [hp (get-hp body)
        ratio (get-ratio hp)
        color (body-render-hpbar-color ratio)
        color (if @active-lightning
                (.darker ^Color color
                  (let [image (or
                                (:image (:image-render @body))
                                (get-animation-frame (current-animation body)))
                        intensity (let [color (.getCornerColor ^Image image Image/TOP_LEFT)]
                                    (/ (+ (.r color) (.g color) (.b color)) 3))]
                    (- 1 intensity)))
                color)]
    (body-render-info-bar g body render-position ratio color body-render-info-bars-h)))

(defn- body-render-attacking-bar [g body skill render-position]
  (body-render-info-bar g body render-position (counter-ratio (:attack-counter skill)) blue (* 2 body-render-info-bars-h)))

(defn body-info-renderfn [g body component {x 0 y 1 :as render-position}]
  (let [tile-posi (:value (:position @body))]
    (when @debug-mode
      (when show-body-bounds
        (body-render-bounds g body render-position white))
      (body-render-debug-info g render-position body))
    (when (not (is-player? body))
      (when (destructible? body)
        (body-render-hp-bar g body render-position))
      (let [skillmanager (:skillmanager @body)
            active-type (:active-type skillmanager)]
        (when (and skillmanager
                   (#{:healing :monster-nova} active-type)
                   (is-attacking? skillmanager))
          (body-render-attacking-bar g body (get-active-skill skillmanager) render-position)))
      (when-let [component (first (filter #(and (:show-cast-bar %) (is-attacking? %)) (vals @body)))]
        (body-render-attacking-bar g body component render-position)))))

;;;

(alter-var-root #'init-fns conj
  (fn []
    (def ^:private body-render-outlines {:green {:topleft (create-image "outline/tl.png" :scale 0.5)
                                     :topright (create-image "outline/tr.png" :scale 0.5)
                                     :bottomleft (create-image "outline/bl.png" :scale 0.5)
                                     :bottomright (create-image "outline/br.png" :scale 0.5)}
                             :red {:topleft (create-image "outline/tl_r.png" :scale 0.5)
                                   :topright (create-image "outline/tr_r.png" :scale 0.5)
                                   :bottomleft (create-image "outline/bl_r.png" :scale 0.5)
                                   :bottomright (create-image "outline/br_r.png" :scale 0.5)}})))

(def body-outline-height 2)

; The corner is at pixel x4 y4 where the body outline corner is
(defn- body-render-draw-outline-shape [image x y]
  (draw-image image (- x 2) (- y 2)))

(defn- body-render-outline [g body [x y] color]
  (let [h-width (:half-pxw (:body @body))
        h-height (:half-pxh (:body @body))
        leftx (- x h-width)
        rightx (+ x h-width)
        topy (- y h-height)
        bottomy (+ y h-height)]
    (body-render-draw-outline-shape (:topleft (color body-render-outlines)) leftx topy)
    (body-render-draw-outline-shape (:topright (color body-render-outlines)) rightx topy)
    (body-render-draw-outline-shape (:bottomleft (color body-render-outlines)) leftx bottomy)
    (body-render-draw-outline-shape (:bottomright (color body-render-outlines)) rightx bottomy)))

(ingame-loop-comp :body-outline
  (rendering :below-gui  [g c]
    (when-let [body (get-mouseover-body)]
      (when (:mouseover-outline (:body @body))
        (let [p (screenpos-of-tilepos (:value (:position @body)))]
          (body-render-outline g body p
                               (if (and (not (is-player? body))
                                        (destructible? body))
                                 :red
                                 :green)))))))

; OR: current rendered image of body
; => make outline image => draw over it!? .. costs 20-30 ms to make that img would neet to cache it..!!

;; item in-hand

;;; mouse-cursor

(let [hotspotx 0
      hotspoty 0]

  (deflazygetter ^:private item-get-default-mouse-cursor
    (-> (CursorLoader/get) (.getCursor "cursor.png" hotspotx hotspoty)))

  (defn reset-default-mouse-cursor []
    (set-mouse-cursor (item-get-default-mouse-cursor) hotspotx hotspoty)))

(alter-var-root #'init-fns conj
  (fn []
    (reset-default-mouse-cursor)))

; (.setDefaultMouseCursor app-game-container) -> der normale

;;; in-hand

(def item-in-hand (atom nil))

(defn is-item-in-hand? [] @item-in-hand)

(defn set-item-in-hand [item]
  (reset! item-in-hand item)
  (set-mouse-cursor (get-scaled-copy (:image item) screen-scale) (* 8 screen-scale) (* 8 screen-scale)))

(defn empty-item-in-hand []
  (reset! item-in-hand nil)
  (reset-default-mouse-cursor))

;; player speed

(def player-move-speed 70) ; px/s

(def ^:private player-speed-animation-types
  {:movement  [:moving]
   :attacking [:sword :sword-small :sword-big :fist]
   :casting   [:casting :meditation]})

(defn- player-inc-speed [component value types]
  {:pre [(contains? player-speed-animation-types types)]}
  (reduce #(update-in %1 [%2 :speed] + value)
          component
          (get player-speed-animation-types types)))

(defn incr-attack-speed  [entity-hash value]
  (update-in entity-hash [:animation] player-inc-speed value :attacking))

(defn incr-casting-speed [entity-hash value]
  (update-in entity-hash [:animation] player-inc-speed value :casting))

(defn incr-move-speed [entity-hash modifier]
  (-> entity-hash
      (update-in [:animation] player-inc-speed modifier :movement)
      (update-in [:movement :speed] + (* (in-tiles-per-ms player-move-speed) modifier))))

;; pressable

(defn pressable-component [mouseover-text pressed & {color :color}]
  {:type :pressable
   :mouseover-text mouseover-text
   :pressed pressed
   :color color})

(ingame-loop-comp :render-mouseover-body-text
  (rendering :below-gui  [g _]
    (when-let [mouseover-body (get-mouseover-body)]
      (when-let [{:keys [mouseover-text color]} (:pressable @mouseover-body)]
        (when (seq mouseover-text) ; rendering "" leads to just background black line ...
          (let [[body-x body-y] (screenpos-of-tilepos (:value (:position @mouseover-body)))]
            (render-readable-text g
                                  body-x
                                  (- body-y (:half-pxh (:body @mouseover-body)) body-outline-height)
                                  :centerx true
                                  :above true
                                  (or color white)
                                  mouseover-text)))))))

(def ^:private pressable-click-distance-tiles 1.5)
(def ^:private pressable-click-dist-sqr (* pressable-click-distance-tiles pressable-click-distance-tiles))

(ingame-loop-comp :check-pressable-mouseoverbody
  (active [_ _ _]
    (when-let [body (get-mouseover-body)]
      (when-let [{pressedfn :pressed} (:pressable @body)]
        (when (and (bodies-in-range? player-body body pressable-click-dist-sqr)
                   (try-consume-leftm-pressed))
          (pressedfn body))))))

;; media

(alter-var-root #'init-fns conj
  (fn []
    (def ^:private items-spritesheet (spritesheet "items/items.png" 16 16))))

(defn get-itemsprite [p]
  (get-sprite items-spritesheet p))

;; defmonster

(defn create-monster
  "use defmonster not this for defining monsters"
  [position monster-type {hp :hp armor :armor :as props} & components] ; monster-type not used?! TODO was used for (:is-boss) or something like that
  (apply create-entity
         (position-component position)
         (create-body :solid true
                      :side :monster
                      :pxw (:pxw props)
                      :pxh (:pxh props)
                      :mouseover-outline true)
         (monster-destructible hp armor)
         (sleeping-component)
         components))

(def monster-defs {})

(defn get-monster-properties [type]
  (or (get monster-defs type)
      (throw (Error. (str "Could not find monster: " type)))))

(defmacro defmonster [monster-type props & components]
  `(let [props# ~props
         type# ~(keyword monster-type)]
     (alter-var-root #'monster-defs assoc type#
       {:create (fn [position#] (create-monster position# type# props# ~@components))
        :half-w (/ (:pxw props#) tile-width 2)
        :half-h (/ (:pxh props#) tile-height 2)
        :movement-type :ground})))

;; status-options

(defprotocol StatusCheckBox
  (status-get-text [this])
  (status-get-state [this])
  (status-set-state [this is-selected]))

(def status-check-boxes (atom []))

(defmacro status-check-box [& forms]
  `(swap! status-check-boxes conj
          (reify StatusCheckBox ~@forms)))

;(status-check-box
;  (status-get-text [this] "Music")
;  (status-get-state [this] (.isMusicOn app-game-container))
;  (status-set-state [this is-selected] (.setMusicOn app-game-container is-selected)))

(status-check-box
  (status-get-text [this] "Sound")
  (status-get-state [this] (.isSoundOn app-game-container))
  (status-set-state [this is-selected] (.setSoundOn app-game-container is-selected)))

(status-check-box
  (status-get-text [this] "Fullscreen")
  (status-get-state [this] (.isFullscreen app-game-container))
  (status-set-state [this is-selected]
    (when (fullscreen-supported?)
      (.setFullscreen app-game-container is-selected))))

(status-check-box
  (status-get-text [this] "Show FPS")
  (status-get-state [this] (.isShowingFPS app-game-container))
  (status-set-state [this is-selected] (.setShowFPS app-game-container is-selected)))

; config is not initialized before 'start'
#_(when (get-setting :lightning-options-menu-checkbox)
  (status-check-box
    (status-get-text [this] "Lightning effects")
    (status-get-state [this] @active-lightning)
    (status-set-state [this is-selected] (reset! active-lightning is-selected))))

(def status-options-session (reify Session
               (load-session [_ data]
                 (when data
                   (doseq [[text state] data]
                     (status-set-state
                       (first (filter #(= text (status-get-text %)) @status-check-boxes))
                       state))))
               (save-session [_]
                 (for [status @status-check-boxes]
                   [(status-get-text status) (status-get-state status)]))
               (new-session-data [_])))

;; maps.add

(defn add-maps-data
  [& {:keys [map-key cell-grid load-content pretty-name rand-item-max-lvl start-position
             tiled-map sprite-sheet details-sprite-sheet spawn-monsters]
      :as argsmap}]
  {:pre [(xor tiled-map sprite-sheet)
         (not-any? #{map-key} (get-map-keys))]}
  (let [w (width cell-grid)
        h (height cell-grid)]
    (add-map map-key
                  (merge
                    (dissoc argsmap :map-key)
                    {:j-cell-grid (delay (create-jcell-grid cell-grid))
                     :contentfields (create-mapcontentfields w h)
                     :minimap-image (create-minimap-image w h)
                     :minimap-bodies (atom #{})
                     :explored-tile-corners (atom (create-grid w h (constantly false)))})))
  (log "Finished creating " map-key)
  ;(check-not-allowed-diagonals cell-grid)
  )

(def ^:private maps-add-create-fns [])

(defmacro maps-add-create-fn [& args]
  `(alter-var-root #'maps-add-create-fns conj (fn [] ~@args)))

(def maps-add-session (reify Session
               (load-session [_ _]
                 (require (get-setting :map-impl-namespace))
                 (doseq [create maps-add-create-fns]
                   (create)))
               (save-session [_])
               (new-session-data [_])))

(defn tilemap-make-argmap [bodyseq]
  (let [temp (apply hash-map bodyseq)
        tiled-map (construct-tiledmap (str "maps/" (:file temp))
                                      lightmanager)]
    (merge (dissoc temp :file)
           {:start-position (translate-to-tile-middle (get-player-entity-start-position tiled-map))
            :tiled-map tiled-map
            :cell-grid (create-cell-grid-from-tiled-map tiled-map)})))

(defn generatedmap-make-argmap [bodyseq]
  (let [{:keys [grid blockprops calc-sprite-posi details-calc-sprite-posi] :as temp} (first bodyseq)]
    (merge (dissoc temp :grid :blockprops :calc-sprite-posi :details-calc-sprite-posi)
           {:cell-grid (create-grid-from-gen-grid grid blockprops calc-sprite-posi details-calc-sprite-posi)})))

(defn call-add-maps-data [mapkey make-argmap-fn & bodyseq]
  (apply add-maps-data
         (apply concat  ; concat so the map is a sequence of its elements
                (assoc (make-argmap-fn bodyseq)
                       :map-key mapkey))))

(defmacro deftilemap [mapkey & body]
  `(maps-add-create-fn
     (call-add-maps-data ~mapkey tilemap-make-argmap ~@body)))

(defmacro defgeneratedmap [mapkey body]
  `(maps-add-create-fn
     (call-add-maps-data ~mapkey generatedmap-make-argmap ~body)))

; --> mach prozedural generierte maps mit prostprocessing (fill-singles/set-cells-behind-walls-nil/remove-nads/..?)
;& assertions 0 NADS z.b. ...?
