(ns utils.core
  (:require [clojure.pprint :refer (pprint)])
  (:import java.util.zip.ZipInputStream))

(defn is-condition-map? [form]
  (and (map? form) (or (:pre form) (:post form))))

(defn condition-map-and-rest [args]
  (if (is-condition-map? (first args))
    [(first args) (rest args)]
    [nil args]))

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

(defn keywords-to-hash-map [keywords] ; other name
  (into {} (for [k keywords]
             [k (symbol (name k))])))

(def pexpand-1 (comp pprint macroexpand-1))
(def pexpand   (comp pprint macroexpand))

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
