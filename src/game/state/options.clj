(ns game.state.options
  (:require [game.state.ids :as ids])
  (:use
    [utils.core :only (def-)]
    (engine
      [core :only (app-game-container initialize fullscreen-supported?)]
      [input :only (update-mousebutton-state is-key-pressed?)]
      render
      statebasedgame)
    (game settings status-options ingame-gui)))

(def- bx 5)
(def- by 20)

(def- ypuffer 12)

(initialize
  (def- display (make-guidisplay))
  (make-textbutton
    :text "Exit"
    :location [bx by]
    :pressed #(enter-state game.state.main/mainmenu-gamestate)
    :parent display)
  (make-textbutton
    :text "Resume"
    :location [bx (+ by (* ypuffer 2))]
    :pressed #(enter-state ids/ingame)
    :parent display)
  (dorun
    (map-indexed
      (fn [idx item]
        (make-checkbox :text (get-text item)
                       :location [100 (+ by (* ypuffer idx))]
                       :pressed #(set-state item %)
                       :selected (boolean (get-state item))
                       :parent display))
      @status-check-boxes))
  (when-not (fullscreen-supported?)
    (make-label :location [bx 150]
                :text "This resolution is not supported in fullscreen mode."
                :visible true
                :parent display)))

(defgamestate options ids/options
  (enter [container statebasedgame])

  (init [container statebasedgame])

  (update [container statebasedgame delta]
    (update-mousebutton-state)
    (update-guicomponent display)
    (when (is-key-pressed? :ESCAPE) ; hier anstatt als keyPressed @ proxy-fn da .isKeyPressed consumed und bei basicgame net
      (enter-state ids/ingame)))

  (render [container statebasedgame g]
    (render-guicomponent g display)))

