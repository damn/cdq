(ns game.tests.target-in-front
  (:require
    [engine.render.color :as color :refer [set-color]]
    [engine.render.graphics :refer [draw-shape]]
    [game.settings :refer [tile-width]]
    [utils.coll :refer [safe-merge]]
    game.components.body
    [game.components.render :refer [render-on-map translate-position]]
    [game.utils.front-of-body-shape :refer [in-front-of-body-shape]]
    [game.components.skills.melee :refer [get-attackable-target-in-front melee-puffer]]))

(defn- make-render-shape [body]
  (let [posi (-> (:value (:position @body)) translate-position)
        hbodyw (:half-pxw (:body @body))
        height (* melee-puffer tile-width)]
    (in-front-of-body-shape posi hbodyw height (:angle (:rotation @body)))))

(defn- render-it [g body c]
  (set-color g (if (get-attackable-target-in-front body) color/red color/green))
  (draw-shape g (make-render-shape body)))

(defn target-in-front-rect-render-component []
  (safe-merge
    {:type :target-in-front-rect-render}
    (render-on-map :air [g entity c render-posi]
                   (render-it g entity c))))








