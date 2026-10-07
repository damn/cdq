(ns game.components.glittering
  (:require
    [engine.core :refer [make-counter update update-finally-merge]]
        [engine.render :refer [folder-animation is-stopped? render-centered-animation restart]]
    [game.components.core :refer [active defcomponent]]
    [game.components.render :refer [render-on-map]]))

(defcomponent glittering []
  {:counter (make-counter 2000)
   :active false
   :animation (folder-animation :folder "effects/itemglitter/"
                                :looping false
                                :duration 300)}
  (active [delta {:keys [active counter animation] :as c} entity]
    (swap! entity assoc-in [(:type c)]
               (if active
                 (let [animation (update animation delta)]
                   (if (is-stopped? animation)
                     (assoc c :active false :animation (restart animation))
                     (assoc c :animation animation)))
                 (update-finally-merge c :counter delta
                                       {:active true}))))
  (render-on-map :air [g _ {:keys [active animation] :as c} render-posi]
    (when active
      (render-centered-animation animation render-posi))))
