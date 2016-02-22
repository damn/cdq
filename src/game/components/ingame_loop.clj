(ns game.components.ingame-loop
  (:use
    [utils.core :only (find-first)]
    game.components.core))

(def ^:private ids (atom #{}))

(defentity ingame-loop-comp [& args]
  (create-comp :ingame-loop-entity
               {:init     #(swap! ids conj (get-id %))
                :destruct #(swap! ids disj (get-id %))})
  (apply create-comp args))

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
    (find-first #(get-component % ctype))
    add-to-removelist))
