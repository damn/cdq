(ns game.state.deferred-preload
  (:import
    (org.newdawn.slick.loading LoadingList)
    org.newdawn.slick.state.BasicGameState)
  (:require
    [engine.render.graphics :refer [render-readable-text]]
    [engine.statebasedgame :refer [enter-state]]
    [game.state.mainmenu :refer [mainmenu-gamestate]]
    [utils.core :refer [get-unique-number]]))

(def ^:private next-resource (atom nil))
(def ^:private loaded-resources (atom []))

(def deferred-preload-gamestate
  (let [id (get-unique-number)]
    (proxy [BasicGameState] []
      (getID [] id)

      (init [container statebasedgame]
        (LoadingList/setDeferredLoading true)
        (doseq [f engine.core/fns]
          (f)))

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
        (render-readable-text g 0 0 :shift true (apply str (interleave (map #(.getDescription %) @loaded-resources) (repeat "\n"))))))))
