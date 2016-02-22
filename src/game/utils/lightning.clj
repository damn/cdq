(ns game.utils.lightning
  (:require [engine.render :as color]
            game.maps.data)
  (:use utils.core
        (game.utils [geom :only (get-distance tiles-inside-rect)]
                    [raycast :only (ray-blocked?)])
        (game session settings)
        (game.components core position
                         [ingame-loop :only (ingame-loop-comp)])
        (game.maps contentfields cell-grid [minimap :only (update-minimap)]))
  (:import org.newdawn.slick.tiled.LightManager
           (org.newdawn.slick Color Image)))

(def ^:private entity-ambient 0.4)
(def ^:private tile-ambient 0.4)
(def ^:private explored-tile-color (color/rgbcolor :r tile-ambient :g tile-ambient :b tile-ambient))

; Currently only light sources of player allowed because
; - lightmap is simultaneously exploring (would need a check is this player light source or something else)
; - other light sources would be visible even if not in line of sight of player
;           => only tile-corners in line of sight of player are brightened up by light sources
;           => we are already calculating these visible tile corners with the lightmap but only in radius
;                 entities are already checked for line of sight
;            => neat trick: light radius of player is screen width/height => all rendered tiles in LOS are in radius
;                  => also used for minimapupdate only in light radius but still all in LOS (problematic when light radius << screen size => minimap not really updated much)

(defn- linear-falloff [i dist r]
  (let [i (float i),dist (float dist), r (float r)]
    (* i (- 1 (/ dist r)))))

(defn- get-intensity [{radius :radius intensity :i falloff :falloff} distance]
  (cond
    (> distance radius) 0
    (not falloff)  (linear-falloff intensity distance radius)
    :else (let [max-intensity-radius (- radius falloff)]
            (cond
              (< distance max-intensity-radius) intensity
              :else
              (linear-falloff intensity (- distance max-intensity-radius) falloff)))))

(defn- calc-color
  [posi {:keys [color radius] :as light} light-posi]
  (let [distance (get-distance posi light-posi)]
    (when (< distance radius) ; TODO does not take into account width/height of an entity! maybe for square ones (+ radius half-w)
      (.scaleCopy ^Color color (get-intensity light distance)))))

(defn- get-corner-posi [[x y] corner]
  (let [x (int x),y (int y)]
    (cond
      (= corner Image/TOP_LEFT) [x y]
      (= corner Image/TOP_RIGHT) [(+ x 1) y]
      (= corner Image/BOTTOM_LEFT) [x (+ y 1)]
      (= corner Image/BOTTOM_RIGHT) [(+ x 1) (+ y 1)])))
; todo just use this and (mapv + posi (get corners-posi corner))
; condp = !
;{Image/TOP_LEFT [0 0]
; Image/TOP_RIGHT [1 0]
; Image/BOTTOM_LEFT [0 1]
; Image/BOTTOM_RIGHT [1 1]}

(def image-corners
  [Image/TOP_LEFT
   Image/TOP_RIGHT
   Image/BOTTOM_LEFT
   Image/BOTTOM_RIGHT])

(defn- create-lightmap
  "A map of int-tile-positions in radius and as value all corner-colors."
  [light-posi {:keys [radius] :as light}]
  (merge
    {:posi light-posi}
    (let [explored-tile-corners (:explored-tile-corners (game.maps.data/get-current-map-data))
          corner-colors (into {}
                          (for [tileposi (tiles-inside-rect [light-posi radius radius])
                                :when (inside-map? tileposi)
                                :let [color (when-not (ray-blocked? light-posi tileposi)
                                              (calc-color tileposi light light-posi))]]
                            (do
                              (when color
                                ; tile inside rect & inside map & not ray blocked & < distance radius
                                ; TODO? simpler => not tiles-inside-rect sondern get touched tiles of radius circle
                                ; then no need for distance check @ calc-color
                                (swap! explored-tile-corners assoc tileposi true))
                              [tileposi color])))
          tile-corner-colors (into {}
                                   (for [tileposi (keys corner-colors)]
                                     [tileposi
                                      (genmap image-corners ; TODO make vector or map color (range 4) ? faster?
                                              (fn [corner]
                                                (get corner-colors (get-corner-posi tileposi corner))))]))]
      (update-minimap (map first (filter (fn [[tile corner-colors]]
                                           (some #(not (nil? %)) (vals corner-colors)))
                                         tile-corner-colors)))
      tile-corner-colors)))

(comment
  (let [posi (get-position player-body)
        light (get-component player-body :light)]
    (compare-times 100 (create-lightmap posi light))))

(def- px-dist-dirty 5)

(def ^:private light-sources (atom #{}))
(def session (atom-session light-sources :save-session false))

; TODO map changed sets dirty
(defn- update-light-sources []
  (let [sources (filter #(get-component % :light) (get-entities-in-active-content-fields))]
    (doseq [entity sources
            :let [current-posi (get-position entity)
                  light (get-component entity :light)
                  old-position (-> light :lightmap :posi)
                  dirty (or
                          (:dirty light)
                          (and
                            (not= old-position current-posi)
                            (>
                              (in-pixel
                                (get-distance old-position current-posi))
                              px-dist-dirty)))]]
      (when dirty
        (assoc-in! entity [:light :dirty] false)
        (assoc-in! entity [:light :lightmap] (create-lightmap current-posi light))))
    (reset! light-sources (doall sources)))) ; doall here because entities might be removed later

; TODO als :posi-changed machen dann neue lightmap
; allerdings dann schwierig zu pmappen
(ingame-loop-comp :update-light-sources
  (active [delta _ _]
    (update-light-sources)))

; TODO rename add-color-to-corner
; also .addColor is unsafe image corners have to be initialized first with setImageColor(can do with 0,0,0,0)
(defn- apply-corner-color [^Image image corner color]
  (when color ; TODO not necessary already checked before but safer...
    (.addColor image corner color)))

(defn- get-corner-colors [^Image image]
  (map #(.getCornerColor image %) (range 4)))

; not yet implemented: if image color has reached max-value (>1.0f for all corners), then you can stop applying lightsources
; because image is at maximum brightness..
; -> happends a lot but filtering is more expensive!
; -> maybe do it in java?
(defn- fully-lit? [^Color c]
  (and
    (>= (.r c) 1)
    (>= (.g c) 1)
    (>= (.b c) 1)))

; previously for every image corner the color was calculated & a ray casted, but the difference for most small agents is
; very small almost not noticeable, so only the color for the center is calculated which gives a performance boost and is simpler
; and also when rotation the corner positions are not correct anymore
(defn- apply-lightsource [image position light-source]
  (let [light (get-component light-source :light)
        light-posi (get-position light-source)
        ; dont check if ray-blocked because
        ; already checked if entities are in-line-of-sight,
        ; so no need to cast another ray
        color (calc-color position light light-posi)]
    ;(when color
      ; Option 1: set-corner-colors transparent before applying lightsources
      ; then change the transparency here according to the visibility => are invisible on explored&unexplored background
      ;(set! (.a color) (.r color))

      ; Option 2: set-corner-colors transparent before applying lightsources
      ; suddenly black color like from the pitch darkness but ambient 0.4 behind them = Strange
      ;(set! (.a color) 1)

      ; Option 3 (currently): always visible in ambient color also outside light radius in unexplored terrain (when still in LOS)
      ; == player light radius is not visiblity radius
      ; => just set-corner-colors to an ambient color
     ;)
    (doseq [corner image-corners]
      (apply-corner-color image corner color))))

(defn- apply-lightsource-cached [image position light-source]
  (when-let [cached-colors (get
                             (:lightmap (get-component light-source :light))
                             (int-posi position))]
    (doseq [corner image-corners
            :let [cached-color (get cached-colors corner)]]
      ;(when (every? fully-lit? (get-corner-colors image))
      ;  (println "fully-lit and applying " [ix iy]))
      (when cached-color
        (apply-corner-color image corner cached-color)))))

(defn- set-corner-colors [image value]
  (.setImageColor ^Image image value value value))

(defn- reset-brightness [image] (set-corner-colors image 1))

(def active-lightning (atom true))

(defn set-brightness [image position]
  (if @active-lightning
    (do
      (set-corner-colors image entity-ambient)
      (runmap #(apply-lightsource image position %) @light-sources))
    (reset-brightness image)))

(defn explored? [corner-tile-position]
  (get @(:explored-tile-corners (game.maps.data/get-current-map-data)) corner-tile-position))
; is expensive always accessing current map data?
; maybe when render-grid dereference it 1 time and then just (get explored-tile-corners corner-position)
; but how to do at set-cached-brightness of tiledmaps? => just use a different explored? ...

(defn set-cached-brightness [image position] ; TODO name?
  (if @active-lightning
    (do
      (set-corner-colors image (if @debug-mode tile-ambient 0))
      (doseq [corner image-corners
              :when (explored? (get-corner-posi position corner))]
        (apply-corner-color image corner explored-tile-color))
      (runmap #(apply-lightsource-cached image position %) @light-sources))
    (reset-brightness image)))

(def lightmanager (reify LightManager
                         (setBrightness [this image x y]
                           (set-cached-brightness image [x y]))))

(defnks light-component [:intensity :radius :opt :falloff :opt-def :color color/white]
  (create-comp :light
    {:color color
     :i intensity
     :radius radius
     :falloff falloff
     :depends [:position]
     :dirty true
     :init (fn [entity]
             (add-cell-blocks-changed-listener
               #(assoc-in! entity [:light :dirty] true)))}))

(defentity map-lightsource [position]
  (position-component position)
  (light-component :intensity 1 :radius 4))
