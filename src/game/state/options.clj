(ns game.state.options
  (:require
    [engine.core :refer [fullscreen-supported? initialize]]
    [engine.input :refer [is-key-pressed? update-mousebutton-state]]
    [engine.statebasedgame :refer [defgamestate enter-state]]
    [game.ingame-gui :refer [make-checkbox make-guidisplay make-label make-textbutton render-guicomponent update-guicomponent]]
    [game.state.ids :as ids]
    [game.state.mainmenu :refer [mainmenu-gamestate]]
    [game.status-options :refer [get-state get-text set-state status-check-boxes]]))

(def ^:private options-bx 5)
(def ^:private options-by 20)
(def ^:private options-ypuffer 12)

(initialize
  (def ^:private options-display (make-guidisplay))
  (make-textbutton
    :text "Exit"
    :location [options-bx options-by]
    :pressed #(enter-state mainmenu-gamestate)
    :parent options-display)
  (make-textbutton
    :text "Resume"
    :location [options-bx (+ options-by (* options-ypuffer 2))]
    :pressed #(enter-state ids/ingame)
    :parent options-display)
  (dorun
    (map-indexed
      (fn [idx item]
        (make-checkbox :text (get-text item)
                       :location [100 (+ options-by (* options-ypuffer idx))]
                       :pressed #(set-state item %)
                       :selected (boolean (get-state item))
                       :parent options-display))
      @status-check-boxes))
  (when-not (fullscreen-supported?)
    (make-label :location [options-bx 150]
                :text "This resolution is not supported in fullscreen mode."
                :visible true
                :parent options-display)))

(defgamestate options ids/options
  (enter [container statebasedgame])

  (init [container statebasedgame])

  (update [container statebasedgame delta]
    (update-mousebutton-state)
    (update-guicomponent options-display)
    (when (is-key-pressed? :ESCAPE)
      (enter-state ids/ingame)))

  (render [container statebasedgame g]
    (render-guicomponent g options-display)))
