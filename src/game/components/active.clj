(ns game.components.active)

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

  (use 'game.components.core)
  (map
    (fn [c] [(:type c) (:blocks c)])
    (filter :updatefn (get-components player-body)))
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

