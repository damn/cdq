(ns game.components.ingame-loop
  (:require
    [game.components.core :refer [active add-to-removelist create-entity get-entity get-id]]))

(def ^:private ids (atom #{}))

(defn ingame-loop-comp [& args]
  (create-entity
    {:type :ingame-loop-entity
     :init     #(swap! ids conj (get-id %))
     :destruct #(swap! ids disj (get-id %))}
    (apply merge {:type (first args)} (rest args))))

(defn get-ingame-loop-entities []
  (map get-entity @ids))

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
