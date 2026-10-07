(ns game.state.mainmenu
  (:import
    (org.newdawn.slick Input Music)
    (org.newdawn.slick.gui TextField)
    (org.newdawn.slick.loading LoadingList))
  (:require
    [engine.core :refer [app-game-container get-defaultfont get-line-height initialize]]
    [engine.input :refer [update-mousebutton-state]]
    [engine.render :refer [render-readable-text]]
    [engine.statebasedgame :refer [defgamestate enter-state]]
    [game.gui :refer [make-guidisplay make-label make-textbutton remove-guicomponent render-guicomponent set-visible update-guicomponent]]
    [game.player.session-data :refer [current-character-name get-session-file-character-names]]
    [game.settings :refer [get-setting half-screen-w screen-height version]]
    [game.state.loading :refer [is-loaded-character loading-gamestate]]))

(def ^:private creditstxt
  "Created by Michael Sappler
  Devlog: http://resatori.com

  Graphics:
  - Icons by Lorc
  - lostgarden.com by Daniel Cook
  - AI Wars Graphics pack (arcen games):
  Chris Park, Daniel Cook, Philippe Chabot
  and Hans Martin Portmann.
  - AngbandTk Icons by David Gervais

  Music:
  - dungeon1.xm
  from http://modarchive.org/
  by Gammis of Lemonride ")

(def ^:private menu-buttons-x 5)
(def ^:private menu-second-column-x (+ menu-buttons-x 100))
(def ^:private menu-buttons-y 20)

(defn- init-textfield []
  (def ^:private textfield (TextField. app-game-container (get-defaultfont) menu-second-column-x menu-buttons-y (* (+ 15 3) 6) 10))
  (def ^:private textfield-visible (atom false))
  (.setConsumeEvents textfield false)
  (.setFocus textfield false)
  (.setMaxLength textfield 15))

(defn- start-loading-game [character-name & {new-character :new-character}]
  (.setFocus textfield false)
  (reset! is-loaded-character (not new-character))
  (reset! current-character-name character-name)
  (enter-state loading-gamestate))

(defn- try-create-character []
  (when-let [char-name (seq (.getText textfield))]
    (start-loading-game (apply str char-name) :new-character true)))

(def ^:private menu-display (make-guidisplay))

(declare load-saved-game-components)

(defn- init-load-saved-game-textbuttons []
  (when (bound? #'load-saved-game-components)
    (dorun (map #(remove-guicomponent menu-display %) load-saved-game-components)))
  (def ^:private load-saved-game-components (map-indexed
                                              (fn [idx char-name]
                                                (make-textbutton
                                                  :text char-name
                                                  :location [menu-second-column-x (+ menu-buttons-y (* idx 12))]
                                                  :pressed #(start-loading-game char-name :new-character false)
                                                  :visible false
                                                  :parent menu-display))
                                              (get-session-file-character-names))))

(declare set-visiblity-state)

(initialize
  (init-textfield)
  (def ^:private create-char-button (make-textbutton :location [menu-second-column-x (+ menu-buttons-y 30)]
                                                     :text "create"
                                                     :pressed try-create-character
                                                     :parent menu-display))
  (def ^:private credits-label (make-label :location [menu-second-column-x menu-buttons-y]
                                           :text creditstxt
                                           :parent menu-display))
  (make-textbutton :location [menu-buttons-x menu-buttons-y]
                   :text "New Character"
                   :pressed (fn []
                              (.setText textfield "")
                              (.start (Thread. (fn []
                                                 (Thread/sleep 100)
                                                 (.setFocus textfield true))))
                              (set-visiblity-state :new-char))
                   :parent menu-display)
  (make-textbutton :text "Load Character"
                   :location [menu-buttons-x (+ menu-buttons-y 25)]
                   :pressed #(set-visiblity-state :load-char)
                   :parent menu-display)
  (make-textbutton :text "Credits"
                   :location [menu-buttons-x (+ menu-buttons-y 50)]
                   :pressed #(set-visiblity-state :credits)
                   :parent menu-display)
  (make-textbutton :text "Exit Game"
                   :location [menu-buttons-x (+ menu-buttons-y 75)]
                   :pressed #(.exit app-game-container)
                   :parent menu-display))

(let [visibility {:none      [false false false false]
                  :new-char  [false true true false]
                  :load-char [false false false true]
                  :credits   [true false false false]}]
  (defn- set-visiblity-state [vis-state]
    (let [current (vis-state visibility)]
      (set-visible credits-label (current 0))
      (reset! textfield-visible (current 1))
      (set-visible create-char-button (current 2))
      (dorun (map #(set-visible % (current 3)) load-saved-game-components)))))

(defn- reset-menu-state []
  (init-load-saved-game-textbuttons)
  (set-visiblity-state :none))

(def ^:private menu-skipped (atom false))

(def ^:private music nil)

(initialize
  (alter-var-root #'music (constantly
                            (doto (Music. "sounds/dungeon1.xm" true)
                              (.setVolume (float 1))))))

(defgamestate mainmenu
  (enter [container statebasedgame]
    (LoadingList/setDeferredLoading false)
    (when music
      (.loop ^Music music))
    (reset-menu-state))

  (keyPressed [int-key chr]
    (when (and @textfield-visible (= int-key Input/KEY_ENTER))
      (try-create-character))
    (when (= int-key Input/KEY_ESCAPE)
      (.exit app-game-container)))

  (leave [container statebasedgame])

  (init [container statebasedgame])

  (update [container statebasedgame delta]
    (update-mousebutton-state)
    (update-guicomponent menu-display)
    (when (and (get-setting :skip-main-menu-at-startup)
               (not @menu-skipped))
      (reset! menu-skipped true)
      (start-loading-game "Testchar" :new-character true)))

  (render [container statebasedgame g]
    (render-guicomponent g menu-display)
    (when @textfield-visible
      (.render textfield container g))
    (render-readable-text g half-screen-w 0 :centerx true :background false :bigfont true "Cyber Dungeon Quest")
    (render-readable-text g half-screen-w (- screen-height (get-line-height)) :centerx true :background false version)))
