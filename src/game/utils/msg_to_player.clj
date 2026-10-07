(ns game.utils.msg-to-player
  (:require
    [engine.render :refer [render-readable-text]]
    [game.settings :refer [half-screen-h half-screen-w]]
    [game.components.render :refer [rendering]]
    [utils.counter :refer [create-counter reset-counter! update-counter]]
    [game.session :refer [atom-session]]
    [game.components.core :refer [active]]
    [game.components.ingame-loop :refer [ingame-loop-comp]]))

(def ^:private message (atom nil))
(def message-session (atom-session message :save-session false))

(def ^:private counter (create-counter 4000))
(def counter-session (atom-session counter :save-session false))

(defn show-msg-to-player [& more]
  (reset-counter! counter)
  (reset! message (apply str more)))

(ingame-loop-comp :msg-to-player
                  ; noch 1 stufe h�her als :above-gui -> �ber den tooltips! ->:tooltip / :above-gui?
                  ; -> call it the way it is not "above-gui" but "important messages"
  (rendering :above-gui [g _]
    (when-let [msg @message]
      (render-readable-text g half-screen-w (/ half-screen-h 2) :centerx true :background false :bigfont true msg)))
  (active [delta _ _]
    (when (and @message (update-counter counter delta))
      (reset! message nil))))

