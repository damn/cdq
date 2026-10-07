(ns utils.numbers)

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
