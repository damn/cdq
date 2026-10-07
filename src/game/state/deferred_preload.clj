(ns game.state.deferred-preload
  (:import (org.newdawn.slick.loading LoadingList))
  (:require
    [engine.core :refer [init-all]]
    [engine.render.graphics :refer [render-readable-text]]
    [engine.statebasedgame :refer [defgamestate enter-state]]
    [game.state.mainmenu :refer [mainmenu-gamestate]]))

(def ^:private next-resource (atom nil))
(def ^:private loaded-resources (atom []))

(defgamestate deferred-preload
  (init [container statebasedgame]
    (LoadingList/setDeferredLoading true)
    (init-all))

  (update [container statebasedgame delta]
    (when-let [resource @next-resource]
      (.load resource)
      (swap! loaded-resources conj resource)
      (reset! next-resource nil))
    (when (> (.getRemainingResources (LoadingList/get)) 0)
      (reset! next-resource (.getNext (LoadingList/get))))
    (when (zero? (.getRemainingResources (LoadingList/get)))
      (enter-state mainmenu-gamestate)))

  (render [container statebasedgame g]
    (render-readable-text g 0 0 :shift true (apply str (interleave (map #(.getDescription %) @loaded-resources) (repeat "\n"))))))
