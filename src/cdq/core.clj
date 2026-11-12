(ns cdq.core)

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

  (require '[game.components.core :refer [player-body]])
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
