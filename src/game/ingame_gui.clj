(ns game.ingame-gui
  (:require [engine.render :as color]
            game.state.ids)
  (:use utils.core
        (engine input
                core
                [statebasedgame :only (enter-state)]
                render)
        [game.utils.geom :only (point-in-rect?)]
        game.settings
        (game.components [ingame-loop :only (ingame-loop-comp)]
                         [render :only (rendering)]
                         [core :only (active)])))

(def- controls-hotkey :H)
(def skillmenu-hotkey :S)
(def char-hotkey :C)
(def inventory-hotkey :I)
(def options-hotkey :ESCAPE) ; ist nicht nur options sondern auch wenn tod / throw item away hotkey

;;;;;;;;;

; Nett:
; variable characterframe textlabel (text) function or jsut when text=function (text) instead of text
; also free skill points : ...
; is-visible check of frame deutet auf guicomponent mit :parent benutzen hin!
; z.b. ausgrauen == enabled false/true !
; also skill tooltips!
; mixup of refcomponent/component ...
; leftm consume auf button A und dann move auf button B => B hat is-leftm-consumed color!

(defn mouseover? [component] (point-in-rect? (get-mouse-pos) (:bounds component)))

;;;;;;;;;;;

(def- mouseover-darkgray (rgbcolor :r 0.3 :g 0.3 :b 0.3 :a 0.5))
(def- mouseover-lightgray (rgbcolor :r 0.7 :g 0.7 :b 0.7 :a 0.5))

(defn- render-label [g {:keys [text bounds]}]
  (let [[x y w h] bounds]
    (render-readable-text g x y :background false :shift false text)))

(defn- render-pressable-rect [g {:keys [bounds] :as component}]
  (let [[x y w h] bounds]
    (when (mouseover? component)
      (fill-rect g (dec x) (dec y) (+ w 1) (+ h 1) (if (is-leftm-consumed?) mouseover-lightgray mouseover-darkgray)))
    (draw-rect g (dec x) (dec y) (+ w 1) (+ h 1) color/white)))

(defn- render-textbutton [g component]
  (render-label g component)
  (render-pressable-rect g component))

(defpreload ^:private selected-img (create-image "selected.png"))

(defn- render-checkbox [g {:keys [bounds] :as component}]
  (let [[x y w h] bounds]
    (render-textbutton g component)
    (when (:selected component)
      (draw-image selected-img (+ x (+ w 2)) y))))

(defn- render-imgbutton [g {:keys [bounds image] :as component}]
  (let [[x y w h] bounds]
    (draw-image image x y))
  (render-pressable-rect g component))

(def- framebg (rgbcolor :r 0.05 :g 0.05 :b 0.05 :a 1))
(def- frameborder (rgbcolor :r 0.2 :g 0.2 :b 0.2 :a 1))

(defn- render-frame [g {:keys [bounds] :as component}]
  (fill-rect g bounds framebg)
  (draw-rect g bounds frameborder))

;;;;;;;;;

(defn set-visible [component bool] (swap! component assoc :visible bool))

(defn switch-visible [component] (swap! component update-in [:visible] not))

(defn is-visible? [component] (:visible @component))

(defn get-absolute-posi [component]
  (let [[x y w h] (:bounds @component)]
    [x y]))

(defn get-bounds [component] (:bounds @component))

;;;;;;;

(defn- is-checkbox? [component] (contains? component :selected))

(defn- update-button [refcomponent]
  (let [{:keys [pressed visible] :as component} @refcomponent]
    (when (and visible
               (mouseover? @refcomponent)
               (try-consume-leftm-pressed))
      (if (is-checkbox? @refcomponent)
        (pressed (:selected (swap! refcomponent update-in [:selected] not)))
        (pressed)))))

(defn- update-frame [refcomponent]
  (when (is-key-pressed? (:hotkey @refcomponent))
    (switch-visible refcomponent)))

;;;;;;;;;;

(defn make-guidisplay [] (atom {:children (list)}))

(defn remove-guicomponent [display component]
  (swap! display update-in [:children] (fn [children] (remove #(= % component) children))))

(defn- translate-by-parent-bounds [[x y w h] [px py pw ph]]
  [(+ px x) (+ py y) w h])

(defn- guicomponent
  "Can also have :name"
  [{:keys [parent visible] :or {visible true} :as data}]
  {:pre [(every? #(contains? data %) [:parent :bounds])]}
  (let [component (atom (assoc data :visible visible))]
    (swap! parent update-in [:children] conj component)
    (when (:bounds @parent)
      (swap! component update-in [:bounds] translate-by-parent-bounds (:bounds @parent)))
    component))

(defn- get-textbounds [{x 0 y 1} text]
  [x y (get-text-width text (get-defaultfont)) (get-text-height text (get-defaultfont))])

(defn make-label [& {:keys [text location] :as data}]
  {:pre [(every? #(contains? data %) [:location :text])]}
  (guicomponent (merge data
                       {:render render-label
                        :bounds (get-textbounds location text)})))

(defn make-textbutton [& {:keys [text location] :as data}]
  {:pre [(every? #(contains? data %) [:location :text :pressed])]}
  (guicomponent (merge data
                       {:render render-textbutton
                        :update update-button
                        :bounds (get-textbounds location text)})))

(defn make-checkbox [& {:keys [text location] :as data}]
  {:pre [(every? #(contains? data %) [:location :text :pressed :selected])]}
  (guicomponent (merge data
                       {:render render-checkbox
                        :update update-button
                        :bounds (get-textbounds location text)})))

(defn make-imgbutton [& {:keys [image location] :as data}]
  {:pre [(every? #(contains? data %) [:location :image :pressed])]}
  (guicomponent (merge data
                       {:render render-imgbutton
                        :update update-button
                        :bounds (let [[x y] location
                                      [w h] (get-dimensions image)]
                                  [x y w h])})))

(deflazygetter ^:private get-closeimg (create-image "close.png"))
; using lazygetter instead of defpreload because other preload depends on this (make-frame)

(defn make-frame [& data]
  (let [data (apply hash-map data)]
    (assert (every? #(contains? data %) [:bounds :hotkey]))
    (let [[x y w h] (:bounds data)
          frame (guicomponent (merge data
                                     {:is-frame true
                                      :render render-frame
                                      :update update-frame}))]
      (make-imgbutton :image (get-closeimg)
                      :location [(- w 6) 2]
                      :pressed #(set-visible frame false)
                      :parent frame)
      frame)))

;;;;

(defn render-guicomponent [g display]
  (doseq [refcomponent (reverse (sort-by #(:is-frame @%) (:children @display)))
          :let [{:keys [tooltip visible render] :as component} @refcomponent]
          :when visible]
    (render g component)
    (when (and tooltip (mouseover? component))
      (let [[x y] (get-absolute-posi refcomponent)]
        (render-readable-text g x (dec y) :shift true :background true :above true tooltip)))
    (when (:children component)
      (render-guicomponent g refcomponent))))

(defn update-guicomponent [display]
  (doseq [refcomponent (:children @display)
          :let [{:keys [update children]} @refcomponent]
          :when update]
    (update refcomponent)
    (when children
      (update-guicomponent refcomponent))))

;;;;;

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
  (def- controlsframe (let [w ;(+ 10 (get-text-width controls)) TODO FIXME
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




