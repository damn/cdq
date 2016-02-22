(ns game.utils.order
  (:use [utils.core :only (positions)]))

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
      (if-let [incorrect (find-first #(not (has-correct-order? % a-seq)) a-seq)]
        (order-maps (fix incorrect a-seq) (dec iterations-left))
        a-seq))))