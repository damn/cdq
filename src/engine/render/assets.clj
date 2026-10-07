(ns engine.render.assets
  (:require
    [utils.core :refer [get-jar-entries]]
    [engine.settings :refer [jar-file?]]
    [engine.render.image :refer [create-image]]
    [engine.render.animation :refer [create-animation default-frame-duration]])
  (:import java.io.File
           (org.newdawn.slick Color SpriteSheet)))

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
(defn folder-frames [folder & {:keys [transparent prefix]}]
  (doall ; for pre-loading
    (map
      #(create-image (str folder %) :transparent transparent)
      (get-pngs folder :prefix prefix))))

(defn folder-animation
  "duration is duration of all frames together, will be split evenly across frames."
  [& {:keys [folder looping duration transparent prefix]}]
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
