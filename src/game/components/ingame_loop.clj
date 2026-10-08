(ns game.components.ingame-loop
  (:require
    [game.components.core :refer [active add-to-removelist create-entity id-entity-map]]))

(def ^:private ids (atom #{}))

(defn ingame-loop-comp [& args]
  (create-entity
    {:type :ingame-loop-entity
     :init     #(swap! ids conj (:id (meta %)))
     :destruct #(swap! ids disj (:id (meta %)))}
    (apply merge {:type (first args)} (rest args))))

(defn get-ingame-loop-entities []
  (map #(get @id-entity-map %) @ids))

(defmacro do-in-game-loop [& expr]
  `(ingame-loop-comp :temporary
     (active [delta# c# entity#]
       ~@expr
       (add-to-removelist entity#))))

(defn remove-entity [ctype]
  (->>
    (get-ingame-loop-entities)
    (first (filter #(ctype @%)))
    add-to-removelist))
