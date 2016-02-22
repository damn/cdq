(ns game.player.movement
  (:use
    utils.core
    (engine input render)
    (game mouseoverbody settings [debug-settings :as debug])
    game.utils.geom
    (game.components core body movement ingame-loop render)
    (game.player settings)))

(defn- add-vs [vs]
  (normalise (reduce add (vector2f [0 0]) vs)))

(defn- calc-movement-v [body]
  (let [r (when (is-key-down? :RIGHT) [1 0])
        l (when (is-key-down? :LEFT) [-1 0])
        u (when (is-key-down? :UP) [0 -1])
        d (when (is-key-down? :DOWN) [0 1])]
    (cond
      @saved-mouseover-body
      (entity-direction-vector body @saved-mouseover-body)

      (and (not (is-leftm-consumed?)) (is-leftbutton-down?))
      (get-vector-to-mouse-coords)

;      (or r l u d)
;      (let [^Vector2f v (add-vs (map vector2f (remove nil? [r l u d])))]
;        (when (pos? (.length v))
;          v))

      )))

(ingame-loop-comp :player-noclip-info
  (rendering :above-gui [g component]
    (when debug/player-noclip-info
      (render-readable-text g 0 200
        (str "player noclip? " (colliding-with-other-solid-bodies? player-body))))))

(defn player-movement-component []
  (movement-component
    {:control-update (fn [body _ delta]
                       (assoc-in! body [:movement :noclip] (colliding-with-other-solid-bodies? body))
                       (calc-movement-v body))}
    player-move-speed
    :ground))
