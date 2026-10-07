(ns game.ingame-gui
  (:require
    [engine.render.color :as color]
    [engine.render.image :refer [create-image]]
    game.state.ids
    [engine.statebasedgame :refer [enter-state]]
    [game.components.ingame-loop :refer [ingame-loop-comp]]
    [game.components.render :refer [rendering]]
    [game.components.core :refer [active]]
    [engine.core :refer [get-text-height initialize]]
    [game.settings :refer [get-setting screen-height]]
    [game.gui :refer [is-visible? make-frame make-guidisplay make-imgbutton make-label mouseover? render-guicomponent set-visible switch-visible update-guicomponent]]))

(def ^:private controls-hotkey :H)
(def skillmenu-hotkey :S)
(def char-hotkey :C)
(def inventory-hotkey :I)
(def options-hotkey :ESCAPE) ; ist nicht nur options sondern auch wenn tod / throw item away hotkey

(def ingamestate-display (make-guidisplay))

(ingame-loop-comp :display
  (active [delta c _]
   (update-guicomponent ingamestate-display))
  (rendering :selfmade-gui [g c]
    (render-guicomponent g ingamestate-display)))

(def background-color color/lightGray)
(def foreground-color (.brighter background-color 0.5))

(def frame-screenborder-distance 5)

(defn find-component [display name]
  (first (filter #(= (:name @%) name) (:children @display))))

(defn- get-all-frames []
  (filter #(:is-frame @%) (:children @ingamestate-display)))

(defn some-visible-frame? []
  (some is-visible? (get-all-frames)))

(defn close-all-frames []
  (dorun (map #(set-visible % false) (get-all-frames))))

(defn mouse-inside-some-gui-component? []
  (some
    #(and (is-visible? %) (mouseover? @%))
    (:children @ingamestate-display)))

;; Help/Controls

; -> only open for first starting of the game or for every character only one time and save with save-game

(def controls
  "* Moving: Leftmouse
* Use skills: Left & right mouse
* Set Skill hotkey: Press 0-9 while hovering
over a skill at the bottom left selection lists.
* Use items in actionbar: Q,W and E.
* Minimap: TAB")

(initialize
  (def ^:private controlsframe (let [w ;(+ 10 (get-text-width controls)) TODO FIXME
                            320]
                        (make-frame :name :controls
                                    :bounds [frame-screenborder-distance
                                             212
                                             w
                                             (+ 2 (get-text-height controls))]
                                    :hotkey controls-hotkey
                                    :visible (get-setting :show-controls-frame)
                                    :parent ingamestate-display)))
  (make-label :location [2 2]
              :text controls
              :parent controlsframe))

(def buttonx-start 60) ; rechts neben skill-selection-buttons
(def x-dist 18) ; mit get-mouse-pos rausgefunden

(def buttonscale [16 16])

(defn- hotkey-str [hotkey s]
  (let [hkname (name hotkey)
        markedhkname (str "[" hkname "]")]
    (if (.contains s hkname)
      (.replace s hkname markedhkname)
      (str markedhkname s))))

; -1 location: free sp button

(initialize
  (make-imgbutton
    :image (create-image "icons/character.png" :scale buttonscale)
    :location [(+ buttonx-start (* x-dist 0)) (- screen-height 18)]
    :pressed #(switch-visible (find-component ingamestate-display :character))
    :tooltip (hotkey-str char-hotkey "Character Attributes")
    :parent ingamestate-display)

  (make-imgbutton
    :image (create-image "icons/skills.png" :scale buttonscale)
    :location [(+ buttonx-start (* x-dist 1)) (- screen-height 18)]
    :pressed #(switch-visible (find-component ingamestate-display :skillmenu))
    :tooltip (hotkey-str skillmenu-hotkey "Skills")
    :parent ingamestate-display)

  (make-imgbutton
    :image (create-image "icons/inventory.png" :scale buttonscale)
    :location [(+ buttonx-start (* x-dist 2)) (- screen-height 18)]
    :pressed #(switch-visible (find-component ingamestate-display :inventory))
    :tooltip (hotkey-str inventory-hotkey "Inventory")
    :parent ingamestate-display)

  (make-imgbutton
    :image (create-image "icons/controls.png" :scale buttonscale)
    :location [(+ buttonx-start (* x-dist 3)) (- screen-height 18)]
    :pressed #(switch-visible (find-component ingamestate-display :controls))
    :tooltip (hotkey-str controls-hotkey "Game Controls/Hotkeys")
    :parent ingamestate-display)

  (make-imgbutton
    :image (create-image "icons/options.png" :scale buttonscale)
    :location [(+ buttonx-start (* x-dist 4)) (- screen-height 18)]
    :pressed #(enter-state game.state.ids/options)
    :tooltip (hotkey-str options-hotkey "Options/Exit")
    :parent ingamestate-display))
