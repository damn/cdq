(ns engine.render
  (:require [utils.core :as utils :refer [defnks when-seq get-jar-entries]])
  (:use [game.utils.geom :as geom]
        (engine [core :as core]
                [settings :only (jar-file?)]))
  (:import java.io.File
           (org.newdawn.slick Graphics Color Image SpriteSheet)
           org.newdawn.slick.geom.Shape))

(defmacro defslick2dcolors [& slick-static-colors]
  `(do
     ~@(map #(list 'def % (symbol (str "Color/" %))) slick-static-colors)))

(defslick2dcolors
  transparent
  white
  yellow
  red
  blue
  green
  black
  gray
  cyan
  darkGray
  lightGray
  pink
  orange
  magenta)

(defnks rgbcolor [:opt-def :r 0 :g 0 :b 0 :a 1 :darker 0 :brighter 0]
  (-> (Color. (float r) (float g) (float b) (float a))
      (.darker darker)
      (.brighter brighter)))

(defmacro defcolor [namesym & args]
  `(def ~namesym (rgbcolor ~@args)))

(defn set-color [^Graphics g color]
  (.setColor g color))

(defn- center-shape [^Shape shape [x y]]
  (doto shape
    (.setCenterX x)
    (.setCenterY y)))

(defn draw-shape [^Graphics g shape]
  (.draw g shape))

(defn render-centered-shape
  ([g shape posi]
   (render-centered-shape g shape posi white))
  ([g shape posi color]
   (center-shape shape posi)
   (set-color g color)
   (draw-shape g shape)))

(defn fill-rect
  ([g [x y w h] color]
    (fill-rect g x y w h color))
  ([^Graphics g x y w h color]
    (set-color g color)
    (.fillRect g x y w h)))

(defn draw-rect
  ([g [x y w h] color]
    (draw-rect g x y w h color))
  ([^Graphics g x y w h color]
    (set-color g color)
    (.drawRect g x y w h)))

(defcolor transparent-black :a 0.8)

(defn draw-string [^Graphics g x y s]
  (.drawString g (str s) x y))

(defn- no-of-lines
  "for example lineheight 20 and textheight 1-20 -> 1 line; 21-40 -> 2 lines etc."
  [txtheight lineh]
  (int (Math/ceil (/ txtheight lineh))))

; workaround for drawstring does not detect \newline
; => seperate for the right number of newlines
; for example        ["abc\ncdef" :x "123" :a "4\n\n3"]
; needs to look like ["abc" "cdef" :x "123" :a "4" "" "3"]
; multiple \n\n in a row (at the end of the string) will not be detected by split-lines but it is okay
; because they will be counted in the height and not rendered anything in it anyway
; only problem: "Test\n\n" will be converted to "Test" with split-lines ...
; (partition-by #(= \newline %) "abc\n\n") also doesnt work...
(use '[clojure.string :only (split-lines)])

(defn- seperate-newline-strings [coll]
  (reduce #(if (coll? %2) (vec (concat %1 %2)) (conj %1 %2))
          []
          (map #(if (string? %) (split-lines %) %) coll)))

(defn- render-colored-text [^Graphics g x y & colors-and-more]
  (let [lineh (core/get-line-height)
        y (atom y)
        colors-and-more (seperate-newline-strings ; draw-string of spritesheetfont does not understand \newline so we have to seperate them manually
                          (remove nil? colors-and-more))] ; we dont want newlines for nils
    (doseq [elem colors-and-more]
      (if (instance? Color elem)
        (.setColor g elem)
        (let [originals (str elem)
              s (apply str (filter allowed-characters originals))]
          (when-seq [not-allowed (remove allowed-characters originals)]
            (println "Characters not in font file: " (pr-str not-allowed) " of string: " (str elem)))
          (draw-string g x @y s)
          (swap! y + (* lineh
                        (no-of-lines (get-text-height originals) lineh)))))))) ; height of originals because \newlines would be filtered out

(defn- get-readable-renderx
  "if string goes out of screen-width bounds it is shifted to the left."
  [string x]
  (let [xpuffer (- (get-screen-width)
                   (+ x (get-text-width string)))]
    (if (neg? xpuffer) (- x (- xpuffer)) x)))

(defn- get-readable-rendery
  "if string goes out of screen-height bounds it is shifted to the top."
  [string y]
  (let [ypuffer (- (get-screen-height)
                   (+ y (get-text-height string)))]
    (if (<= ypuffer 0) (- y (- ypuffer)) y)))

; TODO shift and background set default to false because is not expected when programming gui texts...
(defnks render-readable-text
  "textcolorseq consists of colors and the rest is str-ed. dont use keywords because of destructuring!"
  [g x y
   :opt :centerx :centery :bigfont :above
   :opt-def :shift true :background true
   & textcolorseq]
  (let [whole-text (->> textcolorseq
                     (remove #(instance? Color %))
                     (remove nil?) ; so there will be no "\n" interposed between empty lines... (render-colored-text does not increse lineh with empty lines)
                     (map str)
                     (interpose "\n")
                     (apply str))
        _ (.setFont ^Graphics g (if bigfont (get-defaultfont) (get-defaultfont)))
        w (inc (get-text-width whole-text))
        h (get-text-height whole-text)
        x (if-not centerx x (- x (/ w 2))) ; before shifting calculate centered x/y
        y (if-not centery y (- y (/ h 2)))
        y (if-not above y (- y (get-text-height whole-text)))
        x (if shift (get-readable-renderx whole-text x) x)
        y (if shift (get-readable-rendery whole-text y) y)
        x (int x)
        y (int y)]
    (when background
      (fill-rect g x y w h transparent-black))
    (set-color g white)
    (apply render-colored-text g x y textcolorseq)
    (reset-font g)))

(defn fill-centered-circle [^Graphics g radius position color]
  (set-color g color)
  (let [^Shape shape (geom/circle [-1 -1] radius)]
    (center-shape shape position)
    (.fill g shape)))

(defn get-dimensions [^Image image]
  [(.getWidth image)
   (.getHeight image)])

(defn draw-image
  ([image x y]
    (.draw ^Image image x y))
  ([image ^Float x ^Float y ^Float w ^Float h]
    (.draw ^Image image x y w h)))

(defn render-centered-image [image [x y]]
  (.drawCentered ^Image image x y))

(defn render-rotated-centered-image [^Graphics g image angle {x 0 y 1 :as position}]
  (.rotate g x y angle)
  (render-centered-image image position)
  (.rotate g x y  (- angle)))

(defn get-sub-image [^Image i x y w h]
  (.getSubImage i x y w h))

(defn get-scaled-copy
  ([^Image i value] (.getScaledCopy i value))
  ([^Image i w h]   (.getScaledCopy i w h)))

(defn create-image
  "org.newdawn.slick.Image data is cached(?) so if the same image path is loaded x2
  the second time it loads from the cached data much faster."
  [file & {:keys [transparent scale]}]
  ; Image constructor: String ref, boolean flipped, int f, Color transparent
  ; set this filter so there is no antialiasing effect when images are not at a perfect pixel position or rotated
  ; set in constructor not later in .setFilter so the image will not be loaded (deferred loading)
  (let [image (if transparent
                (Image. (str file) false Image/FILTER_NEAREST ^Color transparent)
                (Image. (str file) false Image/FILTER_NEAREST))]

    (cond
      (vector? scale) (apply get-scaled-copy image scale)
      (number? scale) (get-scaled-copy image scale)
      :else image)))

(defprotocol Animation
  (is-stopped?  [_])
  (restart      [_])
  (get-duration [_])
  (get-frame    [_]))

(defrecord ImmutableAnimation [frames frame-duration looping speed cnt maxcnt]
  Updateable
  (update [this delta]
    (let [newcnt (+ cnt (* speed delta))]
      (assoc this :cnt (cond (< newcnt maxcnt) newcnt
                             looping           (min maxcnt (- newcnt maxcnt))
                             :else             maxcnt))))
  Animation
  (is-stopped? [_]
    (and (not looping) (= cnt maxcnt)))
  (restart [this]
    (assoc this :cnt 0))
  (get-duration [_]
    maxcnt)
  (get-frame [this]
    ; int because speed can make delta a float value.
    (get frames (int (quot (dec cnt) frame-duration)))))

(def default-frame-duration 33)

(defnks create-animation [frames :opt-def :frame-duration default-frame-duration :looping false]
  (map->ImmutableAnimation
    {:frames (vec frames)
     :frame-duration frame-duration
     :looping looping
     :speed 1
     :cnt 0
     :maxcnt (* (count frames) frame-duration)}))

(defn render-centered-animation [animation position]
  (render-centered-image (get-frame animation) position))

(defn draw-grid
  "Grid lines start at top left corner."
  [^Graphics g leftx topy gridw gridh cellw cellh]
  (let [w (* gridw cellw)
        h (* gridh cellh)
        buttomy (+ topy h)
        rightx (+ leftx w)]
    (doseq [idx (range (inc gridw))
            :let [linex (+ leftx (* idx cellw))]]
      (.drawLine g linex topy linex buttomy))
    (doseq [idx (range (inc gridh))
            :let [liney (+ topy (* idx cellh))]]
      (.drawLine g leftx liney rightx liney))))

(def ^:private debug false)
(def ^:private already-printed (atom #{}))

(defn- debug-print-result [folder prefix result]
  (when debug
    (when-not (contains? @already-printed [folder prefix])
      (swap! already-printed conj [folder prefix])
      (println "\n" [folder prefix] " : " result))))

(defn- filter-prefix-and-sort [names prefix]
  (sort (if prefix
          (filter #(.startsWith ^String % prefix) names)
          names)))

; TODO  only do with jar file?
; (log "getting *.png jar entries...")
(let [all-png-entries (get-jar-entries #(.endsWith ^String % ".png"))]

  (defn get-sorted-pngs-in-jar [folder & {prefix :prefix}]
    (let [hits (filter #(.startsWith ^String % folder) all-png-entries)
          names (map #(subs % (inc (.lastIndexOf ^String % "/"))) hits)
          result (filter-prefix-and-sort names prefix)]
      (assert (apply = (map #(.lastIndexOf ^String % "/") hits)))
      (debug-print-result folder prefix result)
      result)))

"
entryname:
data/player/shooting/shoot0.png
data/player/shooting/shoot1.png
data/player/shooting/raw/weapon.png

startswith: data/player/shooting/
endswith: .png

result includes weapon.png

how to filter /raw ?
-> filter out if between starts and ends is another slash / ?
assert lastindexOf slash is the same for names in a folder?
"


(defn- get-sorted-pngs [folder & {prefix :prefix}]
    (let [file (File. ^String (str "resources/" folder))
          listed-files (.listFiles file)
          pngs (filter #(.endsWith (.getPath ^File %) ".png") listed-files)
          names (map #(.getName ^File %) pngs)
          result (filter-prefix-and-sort names prefix)]
      (debug-print-result folder prefix result)
      result))

(def get-pngs (if jar-file? get-sorted-pngs-in-jar get-sorted-pngs))

; TODO use & more for image arguments like :transparent , so for example :scale also becomes possible
(defnks folder-frames [folder :opt :transparent :prefix]
  (doall ; for pre-loading
    (map
      #(create-image (str folder %) :transparent transparent)
      (get-pngs folder :prefix prefix))))

(defnks folder-animation
  "duration is duration of all frames together, will be split evenly across frames."
  [:folder :looping :opt :duration :transparent :prefix]
  (let [frames (folder-frames folder :transparent transparent :prefix prefix)]
    (create-animation frames
                      :frame-duration (if duration
                                        (int (/ duration (count frames)))
                                        default-frame-duration)
                      :looping looping)))

(defn spritesheet
  ([file tilew tileh]
    (SpriteSheet. ^String file (int tilew) (int tileh)))
  ([file tilew tileh more]
    (SpriteSheet. ^String file (int tilew) (int tileh) ^Color more)))

(defn get-sprite [^SpriteSheet sheet [x y]]
  (.getSprite sheet x y))

(defn- get-sheet-frames [^SpriteSheet sheet]
  (for [y (range (.getVerticalCount sheet))
        x (range (.getHorizontalCount sheet))]
    (get-sprite sheet [x y])))

(defn spritesheet-frames [& more]
  (get-sheet-frames (apply spritesheet more)))

(defn draw-line
  ([g [sx sy] [ex ey]]
   (draw-line g sx sy ex ey))
  ([^Graphics g x y ex ey]
   (.drawLine g x y ex ey)))

(defn translate [^Graphics g x y]
  (.translate g x y))

(defn reset-transform [^Graphics g]
  (.resetTransform g))
