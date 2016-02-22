(ns utils.core
  (:require (clojure [pprint :refer (pprint)]
                     set)
            [clojure.tools.macro :refer (name-with-attributes)])
  (:import java.util.zip.ZipInputStream))

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

(defn find-first ; from clojure.contrib.seq-utils (discontinued in 1.3)
  "Returns the first item of coll for which (pred item) returns logical true.
  Consumes sequences up to the first match, will consume the entire sequence
  and return nil if no match is found."
  [pred coll]
  (first (filter pred coll)))

(defmacro def- ; from clojure.contrib.def (discontinued in 1.3)
  "Same as def but yields a private definition"
  [name value]
  (list 'def (with-meta name (assoc (meta name) :private true)) value))

;;

(defn is-condition-map? [form]
  (and (map? form) (or (:pre form) (:post form))))

(defn condition-map-and-rest [args]
  (if (is-condition-map? (first args))
    [(first args) (rest args)]
    [nil args]))

(defn- split-args
  "(split-args [:a :b :c :opt :d :e :f :opt-def :g 3 :h 4 :i 5])
   => {:opt-def {:g 3, :i 5, :h 4}, :opt [:d :e :f], :obligatory [:a :b :c]}"
  [args]
  (loop [args args
         current :obligatory
         result {:obligatory [] :opt [] :opt-def []}]
    (if (empty? args)
      (assoc
        (select-keys result [:obligatory :opt])
        :opt-def (apply hash-map (:opt-def result)))
      (let [curr (first args)
            state (case curr
                    :opt :opt
                    :opt-def :opt-def
                    current)]
        (recur
          (rest args)
          state
          (if (not= state current)
            result
            (update-in result [state] conj curr)))))))

(defn- destructure-args-and-assert [keyword-params]
  (let [{:keys [obligatory opt opt-def]} (split-args keyword-params)
        sym-vals (into {} (for [[k v] opt-def]
                            [(symbol (name k)) v]))
        all-ks (vec (concat obligatory opt (keys opt-def)))
        all-ks-as-symbols (map (comp symbol name) all-ks)
        argsmap-sym 'argsmap
        kset (gensym "keyset")]
    [`{:keys [~@all-ks-as-symbols] :or ~sym-vals :as ~argsmap-sym}
     `(let [~kset (if ~argsmap-sym
                    (.keySet ~(vary-meta argsmap-sym assoc :tag 'java.util.Map))
                    #{})]
        (assert (and
                  (clojure.set/subset?   ~(set obligatory) ~kset)
                  (clojure.set/superset? ~(set all-ks)     ~kset))
                (str "Incorrect keys: " ~kset)))]))

(defn- split-params [parameters]
  (let [rest-param (when (some #{'&} parameters) ; TODO use the vector trick here?
                     (last parameters))
        [positional keyword-params] (split-with symbol?
                                                (if rest-param (drop-last 2 parameters) parameters))]
    [positional keyword-params rest-param]))

(defn split-kvs-and-more [args]
  (let [pairs (partition-all 2 args)
        [kvpairs restpairs] (split-with #(keyword? (first %)) pairs)
        key-vals-map (apply hash-map (apply concat kvpairs))]
    [key-vals-map (apply concat restpairs)]))

; TODO performance of assert ks? deactivate assertions for production?
; quicker if we let over the defn with obligatory and all-ks? they are created anew every time?
; also nice argslist for defnks with clojure style [positional? keywords-args? & more?]
; TODO in many cases {:pre [a b c]} is good enough to check if key is there AND not nil?
; TODO it is possible to supply a key multiple times
; TODO make 'fn' version
; TODO make macro version?
(defmacro defnks
  "Warning: Binds \"argsmap\" to the args-map.
  First positional args and then ks-args as keywords.
  ks-args are obligatory keys until a :opt or :opt-def; then optional keys as an
  alternating sequence of keywords and defaults values are expected.
  Asserts the obligatory keys are there and all keys are obligatory/optional in the arg-map.
  Allows a docstring and attrs-map after the fn-name and a condition-map with :pre
  (:post not yet implemented)."
  [fn-name & more]
  (let [[fn-name [parameters & more]] (name-with-attributes fn-name more)
        [condition-map fnbody] (condition-map-and-rest more)
        [positional keyword-params rest-param] (split-params parameters)
        [argmap arg-assert] (destructure-args-and-assert keyword-params)
        fnrest_param (gensym "fnrest_param")
        let-bindings (if rest-param
                       `[[~argmap ~rest-param] (split-kvs-and-more ~fnrest_param)]
                       `[~argmap                                   ~fnrest_param])]
    `(do
       (defn ~fn-name [~@positional & ~fnrest_param]
         (let ~let-bindings
           ~arg-assert
           ~@(map #(list 'assert %) (:pre condition-map))
           ~@fnbody))
       (alter-meta! #'~fn-name assoc :arglists '(~parameters))
       #'~fn-name)))

(comment
  (pexpand-1
    '(defnks foo
       "My foo function!"
       {:fooey true}
       [a b :c :opt :d :opt-def :e 123 & f]
       {:pre [(> a b c d)]}
       [a b c d e f]))
  (foo 1 2)
  (foo 1 2 :c 3 :o 5)
  (foo 100 11 :c 1.2 :d 0.1)
  (meta #'foo))

(defn runmap [& more] (dorun (apply map more)))

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

(defn when-apply [f & args]
  (when f (apply f args)))

(defn keywords-to-hash-map [keywords] ; other name
  (into {} (for [k keywords]
             [k (symbol (name k))])))

(defn assoc-in!  [a & args] (apply swap! a assoc-in  args))
(defn update-in! [a & args] (apply swap! a update-in args))

(defmacro ->! [a & forms]
  `(swap! ~a #(-> % ~@forms)))

(defn mapvals [m f]
  (into {} (for [[k v] m]
             [k (f v)])))

(def pexpand-1 (comp pprint macroexpand-1))
(def pexpand   (comp pprint macroexpand))

; geht das auch ohne lambda-fn und mit forms als 2. element wie -> ??
; mit forms ???
(defn thread-through
  "Threads the expr through the sequence of fns, each taking expr as arg and returning it. see '->'.
   The order of evaluation is backwards."
  [expr fns]
  ((apply comp fns) expr))

(defn genmap
  "function is applied for every key to get value. use memoize instead?"
  [ks f]
  (zipmap ks (map f ks)))

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

(defn find-prefixed-var
  "Example: (find-prefixed-var :namespace 'user :prefix \"item-\" :prefixed-type :sword) tries to find user/item-sword."
  [& {:keys [namespace prefix prefixed-type]}]
  (if-let [v (find-var
               (symbol
                 (name namespace)
                 (str prefix (name prefixed-type))))]
    (deref v)
    (throw (Error. (str "Could not find var for type: " prefixed-type)))))

(defn print-n-return [data] (println data) data)

(defn get-next-idx
  "returns the next index of a vector.
  if there is no next -> returns 0"
  [current-idx coll]
  (let [next-idx (inc @current-idx)]
    (reset! current-idx (if (get coll next-idx) next-idx 0))))

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

(defn translate-to-tile-middle
  "translate position to middle of tile becuz. body position is also @ middle of tile."
  [p]
  (mapv (partial + 0.5) p))

(defn variance-val-str [[mi mx]]
  (str (readable-number mi) "-" (readable-number mx)))

(defn variance-val
  "(variance-val 10 0.9) is [1 19] (floating point! real result may be [0.99999998 19.0]) "
  [avg variance]
  {:pre [(>= variance 0) (<= variance 1)]}
  [(* avg (- 1 variance))
   (* avg (inc variance))])

;;

(defn split-key-val-and-maps ; use defnks and apply merge manually? is nicer than defn and a manual defnks...?
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

(defn create-counter
  ([]        (atom {:current 0}))
  ([maxtime] (atom {:current 0,:max maxtime})))

(defn reset-counter! [counter]
  (assoc-in! counter [:current] 0))

(defn update-counter
  "updates counter. if maxtime reached, resets current to 0 and returns the last current value,
   else returns nil."
  [counter delta]
  (update-in! counter [:current] + delta)
  (let [{current :current maxtime :max} @counter]
    (when (and maxtime (>= current maxtime))
      (let [last-current current]
        (reset-counter! counter)
        last-current))))

;;

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
  {:pre [(pos? by)]}
  (update-in data [:current] + (min by (- mx current))))

; just bound-inc ? and pass (- val) ?
(defn inc-or-dec-max [min-max-val f by] ; TODO apply-max !
  (let [ratio (get-ratio min-max-val)
        new-max (f (:max min-max-val) by)]
    (assoc min-max-val
      :max new-max
      :current (* ratio new-max))))

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

;;

(defmacro when-seq [[aseq bind] & body]
  `(let [~aseq ~bind]
     (when (seq ~aseq)
       ~@body)))
;;

; http://stackoverflow.com/questions/1429172/list-files-inside-a-jar-file
(defn get-jar-entries [filter-predicate]
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

(defmacro xor [a b]
  `(or (and (not ~a) ~b)
       (and ~a (not ~b))))

;;

(defn assoc-ks [m ks v]
  (if (empty? ks)
    m
    (apply assoc m (interleave ks (repeat v)))))

(defn filter-map [m pred]
  (select-keys m (for [[k v] m :when (pred v)] k)))
