(ns game.components.update
  (:use
    [utils.core :only (runmap)]
    game.session
    (game.components
      [core :only (get-components)]
      [active :only (not-blocked? try-slowdown-delta)])))

; only change @player-death and switch-debug
(def running (atom true))
(def session (atom-session running :save-session false))

; das spiel soll bei 20fps noch "schnell" sein,d.h. net langsamer werden (max-delta wirkt -> game wird langsamer)
; 1000/20 = 50
(def max-delta 50)

(defn update-component [delta component entity]
  ((:updatefn component) delta component entity))

;-> mit active zusammen in einen namespace?
;-> und vielleicht auch mit blocks?
(defn update-active-components [delta entities]
  (doseq [entity entities
          component (get-components entity)
          :when (and (:updatefn component)
                     (not-blocked? component))]
    (update-component (try-slowdown-delta delta component) component entity)))
