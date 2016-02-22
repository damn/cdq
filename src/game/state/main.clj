(ns game.state.main
  (:use utils.core
        (engine core input statebasedgame render)
        (game settings ingame-gui [mouse-cursor :only (reset-default-mouse-cursor)])
        [game.state.load-session :only (is-loaded-character loading-gamestate)]
        [game.player.session-data :only (current-character-name get-session-file-character-names)])
  (:import org.newdawn.slick.gui.TextField
           org.newdawn.slick.Input
           (org.newdawn.slick.loading LoadingList DeferredResource)))

(def- creditstxt
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

;;;
;;;

(def- buttons-x 5)
(def- second-column-x (+ buttons-x 100))
(def- buttons-y 20) ; title bigfont lineheight 16+4..

;;;
;;;

; TODO sometimes does not respond to input. Focus "_" underscore was rendered...
; TODO mousereleased takes away focus of textfield even after clicking on new-char
(defn- init-textfield []
  (def- textfield (TextField. app-game-container (get-defaultfont) second-column-x buttons-y (* (+ 15 3) 6) 10))
  (def- textfield-visible (atom false))
  (.setConsumeEvents textfield false) ; so the ENTER key of keyPressed of the gamestate is registered
  (.setFocus textfield false)
  (.setMaxLength textfield 15))

(defn- start-loading-game [character-name & {new-character :new-character}]
  (.setFocus textfield false) ; focus is kept even when game-state changes
  (reset! is-loaded-character (not new-character))
  (reset! current-character-name character-name)
  (enter-state loading-gamestate))

; TODO check if name already exists? create-button can not be pressed when no name entered?
(defn- try-create-character []
  (when-let [char-name (seq (.getText textfield))]
    (start-loading-game (apply str char-name) :new-character true)))

(def- display (make-guidisplay))

(declare load-saved-game-components)

(defn- init-load-saved-game-textbuttons []
  (when (bound? #'load-saved-game-components)
    (dorun (map #(remove-guicomponent display %) load-saved-game-components)))
  (def ^:private load-saved-game-components (map-indexed
                                              (fn [idx char-name]
                                                (make-textbutton
                                                  :text char-name
                                                  :location [second-column-x (+ buttons-y (* idx 12))]
                                                  :pressed #(start-loading-game char-name :new-character false)
                                                  :visible false
                                                  :parent display))
                                              (get-session-file-character-names))))

(declare set-visiblity-state)

(initialize
  (init-textfield)
  (def ^:private create-char-button (make-textbutton :location [second-column-x (+ buttons-y 30)]
                                                     :text "create"
                                                     :pressed try-create-character
                                                     :parent display))
  (def ^:private credits-label (make-label :location [second-column-x buttons-y]
                                           :text creditstxt
                                           :parent display))
  (make-textbutton :location [buttons-x buttons-y]
                   :text "New Character"
                   :pressed (fn []
                              (.setText textfield "")
                              (.start (Thread. (fn []
                                                 (Thread/sleep 100)
                                                 (.setFocus textfield true))))
                              (set-visiblity-state :new-char))
                   :parent display)
  (make-textbutton :text "Load Character"
                   :location [buttons-x (+ buttons-y 25)]
                   :pressed #(set-visiblity-state :load-char)
                   :parent display)
  (make-textbutton :text "Credits"
                   :location [buttons-x (+ buttons-y 50)]
                   :pressed #(set-visiblity-state :credits)
                   :parent display)
  (make-textbutton :text "Exit Game"
                   :location [buttons-x (+ buttons-y 75)]
                   :pressed #(.exit app-game-container)
                   :parent display))

(let [visibility {:none      [false false false false]
                  :new-char  [false true true false]
                  :load-char [false false false true]
                  :credits   [true false false false]}]
  (defn- set-visiblity-state [state]
    (let [current (state visibility)]
      (set-visible credits-label (current 0))
      (reset! textfield-visible (current 1))
      (set-visible create-char-button (current 2))
      (dorun (map #(set-visible % (current 3)) load-saved-game-components)))))

(defn- reset-state []
  (init-load-saved-game-textbuttons)
  (set-visiblity-state :none))

(def ^:private skipped (atom false))

(defgamestate mainmenu
  (enter [container statebasedgame]
    (LoadingList/setDeferredLoading false)
    (reset-state))

  (keyPressed [int-key chr]
    (when (and @textfield-visible (= int-key Input/KEY_ENTER))
      (try-create-character))
    (when (= int-key Input/KEY_ESCAPE)
      (.exit app-game-container)))

  (leave [container statebasedgame])

  (init [container statebasedgame])

  (update [container statebasedgame delta]
    (update-mousebutton-state)
    (update-guicomponent display)
    (when (and (get-setting :skip-main-menu-at-startup)
               (not @skipped))
      (reset! skipped true)
      (start-loading-game "Testchar" :new-character true)))

  (render [container statebasedgame g]
    (render-guicomponent g display)
    (when @textfield-visible
      (.render textfield container g))
    (render-readable-text g half-screen-w 0 :centerx true :background false :bigfont true "Cyber Dungeon Quest")
    (render-readable-text g half-screen-w (- screen-height (get-line-height)) :centerx true :background false version)))

;;;
;;;

(def- next-resource (atom nil))
(def- loaded-resources (atom [])) ; todo use Total/RemainingResources ....

(defn- percent-loaded []
  (let [total (.getTotalResources (LoadingList/get))
        remaining (.getRemainingResources (LoadingList/get))
        loaded (- total remaining)
        percent (int (* 100 (/ loaded total)))]
    percent))

(defgamestate deferred-preload
  (init [container statebasedgame]
    (LoadingList/setDeferredLoading true)
    (init-all))

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
    (render-readable-text g 0 0 :shift true (apply str (interleave (map #(.getDescription %) @loaded-resources) (repeat "\n"))))
    (comment (render-readable-text g 30 half-screen-h :centery true :shift false
      (str "Loading..." (percent-loaded) "%" (when-let [resource @next-resource]
                                               (.getDescription resource)))))))
