(ns engine.statebasedgame
  (:require [utils.core :refer (get-unique-number)])
  (:import
    (org.newdawn.slick.state BasicGameState StateBasedGame)))

(defmacro defgamestate [nm & more]
  (let [id? (first more)
        custom-id (when (or (symbol? id?) (number? id?))
                    id?)]
    `(let [id# ~(or custom-id `(get-unique-number))]
       (def ~(symbol (str nm "-gamestate"))
         (proxy [BasicGameState] []
           (getID [] id#)
           ~@(if custom-id (rest more) more))))))

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
