(ns engine.render.graphics
  (:require
    [clojure.string :refer [split-lines]]
    [utils.core :refer [split-kvs-and-more]]
    [utils.coll :refer [when-seq]]
    [game.utils.geom :as geom]
    [engine.core :as core :refer [allowed-characters get-defaultfont get-screen-height get-screen-width get-text-height get-text-width reset-font]]
    [engine.render.color :refer [set-color transparent-black white]])
  (:import (org.newdawn.slick Graphics Color)
           org.newdawn.slick.geom.Shape))

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
(defn render-readable-text
  "textcolorseq consists of colors and the rest is str-ed. dont use keywords because of destructuring!"
  [g x y & args]
  (let [[{:keys [centerx centery bigfont above shift background]
          :or {shift true background true}}
         textcolorseq] (split-kvs-and-more args)
        whole-text (->> textcolorseq
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

(defn draw-line
  ([g [sx sy] [ex ey]]
   (draw-line g sx sy ex ey))
  ([^Graphics g x y ex ey]
   (.drawLine g x y ex ey)))

(defn translate [^Graphics g x y]
  (.translate g x y))

(defn reset-transform [^Graphics g]
  (.resetTransform g))
