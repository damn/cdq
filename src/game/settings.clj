(ns game.settings)

(declare ^:private config)

(defn init-config! [file]
  (alter-var-root #'config (fn [_]
                             (-> file slurp read-string))))

(defn get-setting [k]
  (let [value (get-in config
                      (if (sequential? k) k [k])
                      ::not-found)]
    (if (= value ::not-found)
      (throw (IllegalArgumentException. (str "Could not find key " k " in config")))
      value)))

; TODO this stuff should not be here ? ..
(def screen-width 384)
(def screen-height 288)
(def screen-scale 2)

(def version "alpha3")

;;

(def debug-mode (atom false))

;;

(def tile-width 16)
(def tile-height 16)

(defn in-tiles [pixel] (/ pixel tile-width))
(defn in-pixel [tiles] (* tiles tile-height))

(assert (= tile-width tile-height)) ; da oft einfach (in-tiles pxradius) & auch movement ist sonst weird -> tilew/h gleich voraussetzung!

(def half-screen-w (/ screen-width 2))
(def half-screen-h (/ screen-height 2))

(def display-width-in-tiles (/ screen-width tile-width))
(def display-height-in-tiles (/ screen-height tile-height))

(def half-display-w-in-tiles (/ display-width-in-tiles 2))
(def half-display-h-in-tiles (/ display-height-in-tiles 2))

(def left-offset-in-tiles (int half-display-w-in-tiles))
(def top-offset-in-tiles (int half-display-h-in-tiles))
