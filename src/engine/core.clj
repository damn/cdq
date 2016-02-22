(ns engine.core
  (:require [utils.core :refer (defnks deflazygetter)])
  (:import
    org.lwjgl.opengl.Display
    (org.newdawn.slick ScalableGame AppGameContainer BasicGame Sound Font Graphics GameContainer
                       SpriteSheet SpriteSheetFont)))

; Pre-load big images so the game does not pause/lag when they are played for the first time
; even more important when those images are played only one time for example big monster explosion
; use defpreload for it

;;

(def ^:private fns (list))

(defmacro initialize [& exprs]
  `(alter-var-root #'engine.core/fns conj (fn [] ~@exprs)))

(defn init-all []
  (doseq [f fns]
    (f)))

(defmacro defpreload [& more]
  `(initialize (def ~@more)))

;;

; method of AppGameContainer does flip = true; -> so flip the icons vertical!
; dont supply a icon24.tga else windows will use it for ALT+TAB which needs a 32x32!
; uncompressed 32BIT TGA IMAGES
; from http://lwjgl.org/javadoc/org/lwjgl/opengl/Display.html#setIcon%28java.nio.ByteBuffer[]%29
; Sets one or more icons for the Display.
;
;    On Windows you should supply at least one 16x16 icon and one 32x32.
;    Linux (and similar platforms) expect one 32x32 icon.
;    Mac OS X should be supplied one 128x128 icon
;
;The implementation will use the supplied ByteBuffers with image data in RGBA and perform any conversions nescesarry for the specific platform.
(def ^:private icon-strings (into-array ["icon16.tga" "icon32.tga"]))

(defnks create-and-start-app-game-container
  [:game :width :height :opt :show-fps :lock-framerate :full-screen :scale]
  (defn get-screen-width [] width)
  (defn get-screen-height [] height)
  (let [scale (if (= 1 scale) nil scale)
        container (AppGameContainer. (if scale (ScalableGame. game width height true) game) ; true = maintain aspect ratio
                                     (if scale (* scale width) width)
                                     (if scale (* scale height) height)
                                     (boolean full-screen))]

    (.setIcons container icon-strings)
    (.setShowFPS container (boolean show-fps))
    (when lock-framerate
      (doto container
        (.setTargetFrameRate 60)
        (.setVSync true)))
    (def app-game-container container)
    (.start (Thread.
              #(.start container)))))

(defnks start-slick-basicgame
  [:opt :full-screen
   :opt-def :title "test" :width 800 :height 600 :init (fn [container]) :update (fn [container delta]) :render (fn [container g])]
  (create-and-start-app-game-container
    :game (proxy [BasicGame] [title]
            (init [container]
              (init-all)
              (init container))
            (update [container delta]
              (update container delta))
            (render [container g]
              (render container g)))
    :width width
    :height height
    :full-screen full-screen))

;;

(defn fullscreen-supported?
  ([w h]
    (some #(and (= w (.getWidth %)) (= h (.getHeight %)))
          (Display/getAvailableDisplayModes)))
  ([]
    (let [current (Display/getDisplayMode)]
      (fullscreen-supported? (.getWidth current)
                             (.getHeight current)))))

; not (some #{displayMode} availables)
; because #<DisplayMode 1152 x 864 x 0 @0Hz> is not in AvailableDisplayModes for fullscreen... (different Hz)

;;

(defn create-sound [spath]
  (Sound. ^String (str "sounds/" spath)))

(defn play-sound [spath]
  (.play ^Sound (create-sound spath)))

(defn playonce [^Sound sound]
  (when-not (.playing sound)
    (.play sound)))

;;

(def allowed-characters (set (map char (range 32 126))))

; anuvverbubbla font has (range 32 91) & at draw-string make (.toUpperCase (str s))
; (defpreload ^:private anuvverbubbla (SpriteSheetFont. (SpriteSheet. "anuvverbubbla_8x8.png" 8 8) \space))

(deflazygetter get-defaultfont
  (SpriteSheetFont. (SpriteSheet. "simple_6x8.png" 6 8) \space))
; using lazygetter instead of defpreload because other preload depends on this (make-frame => get text size)

(defn reset-font [g] (.setFont ^Graphics g (get-defaultfont)))

(defn- get-font []
  (.getFont ^Graphics (.getGraphics ^GameContainer app-game-container)))

(defn get-line-height
  ([font] (.getLineHeight ^Font font))
  ([] (get-line-height (get-font))))

; Workaround for text-height & width because "\n" is not registered @ font getHeight/getWidth of SpriteSheetFont

(defn- number-of-lines [s]  ; do not use (count (split-lines s)) because for example with "abc\n\n" is only split into ["abc"]
  (inc (count (filter #(= \newline %) s))))

(defn get-text-height
  ([text] (get-text-height text (get-font)))
  ([text font] (* (number-of-lines text) (get-line-height font))))

(use '[clojure.string :only (split-lines)])

(defn get-text-width
  ([text font] (.getWidth ^Font font (apply max-key count (split-lines text))))
  ([text] (get-text-width text (get-font))))

;;

(defn set-mouse-cursor [data hotspotx hotspoty]
  (.setMouseCursor app-game-container data hotspotx hotspoty))

;;

(defprotocol Updateable
  (update [_ delta]))

(defrecord ImmutableCounter [cnt maxcnt stopped?]
  Updateable
  (update [this delta]
    (let [newcnt   (+ cnt delta)
          stopped? (>= newcnt maxcnt)]
      (assoc this
             :cnt (if stopped? (- newcnt maxcnt) newcnt)
             :stopped? stopped?))))

; if (> maxdelta maxcnt) it could happen that after update it is again 'stopped?' in next update
; could assert (<= maxcnt maxdelta), but dont want to depend on game.components.update here.
(defn make-counter [maxcnt]
  (ImmutableCounter. 0 maxcnt false))

(defn ratio [{:keys [cnt maxcnt]}]
  (/ cnt maxcnt))

(defn update-finally-merge [c k delta m]
  (let [counter (update (k c) delta)]
    (merge (assoc c k counter)
           (when (:stopped? counter)
             m))))

(defn reset [counter]
  (assoc counter :cnt 0))
