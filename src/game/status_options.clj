(ns game.status-options
  (:require
    [engine.core :refer [app-game-container fullscreen-supported?]]
    [game.settings :refer [get-setting]]
    [game.utils.lightning :refer [active-lightning]]
    game.session))

(defprotocol StatusCheckBox
  (get-text [this])
  (get-state [this])
  (set-state [this is-selected]))

(def status-check-boxes (atom []))

(defmacro status-check-box [& forms]
  `(swap! status-check-boxes conj
          (reify StatusCheckBox ~@forms)))

;(status-check-box
;  (get-text [this] "Music")
;  (get-state [this] (.isMusicOn app-game-container))
;  (set-state [this is-selected] (.setMusicOn app-game-container is-selected)))

(status-check-box
  (get-text [this] "Sound")
  (get-state [this] (.isSoundOn app-game-container))
  (set-state [this is-selected] (.setSoundOn app-game-container is-selected)))

(status-check-box
  (get-text [this] "Fullscreen")
  (get-state [this] (.isFullscreen app-game-container))
  (set-state [this is-selected]
    (when (fullscreen-supported?)
      (.setFullscreen app-game-container is-selected))))

(status-check-box
  (get-text [this] "Show FPS")
  (get-state [this] (.isShowingFPS app-game-container))
  (set-state [this is-selected] (.setShowFPS app-game-container is-selected)))

; config is not initialized before 'start'
#_(when (get-setting :lightning-options-menu-checkbox)
  (status-check-box
    (get-text [this] "Lightning effects")
    (get-state [this] @active-lightning)
    (set-state [this is-selected] (reset! active-lightning is-selected))))

(def session (reify game.session/Session
               (load-session [_ data]
                 (when data
                   (doseq [[text state] data]
                     (set-state
                       (first (filter #(= text (get-text %)) @status-check-boxes))
                       state))))
               (save-session [_]
                 (for [status @status-check-boxes]
                   [(get-text status) (get-state status)]))
               (new-session-data [_])))


