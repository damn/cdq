(ns engine.statebasedgame
  (:import
    (org.newdawn.slick.state StateBasedGame)))

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
