(ns game.render-ingame
  (:require [game.debug-settings :as debug]
            game.player.status-gui) ; load it here becuz no dependencies
  (:use utils.core
        (engine core input render)
        (game settings [mouseoverbody :only (get-mouseover-body)])
        (game.components core body render ingame-loop)
        (game.maps render contentfields [camera :only (get-camera-position)])
        [game.utils.tilemap :only (get-mouse-tile-pos mouse-int-tile-pos)]
        [game.tests.lightning :only (render-mouse-tile-blocked-corners)]))

(defn- to-be-rendered-entities-from-map []
  (mapcat (fn [entity]
            (map #(vector entity %)
                 (filter :rendering (get-components entity))))
          (filter on-screen-and-in-sight?
                  (get-entities-in-active-content-fields))))

(defn- render-map-content [g]
  (let [[x y] (int-posi (translate-position (get-camera-position)))
        xtranslate (- half-screen-w x)
        ytranslate (- half-screen-h y)]
    (translate g xtranslate ytranslate)
    (doseq [[entity {:keys [renderfn] :as component}] (sort-by-order (to-be-rendered-entities-from-map)
                                                                     (comp :order second) render-on-map-order)]
      (try
       (renderfn g
                 entity
                 component
                 (translate-position (get-position entity)))
        (catch Throwable t
          (println "Render error for entity " (get-id entity) " and component type " (:type component)))))
    ;(translate g (- xtranslate) (- ytranslate))
    (reset-transform g)))

; depending how the screen shake translation should affect the screen choose resetTransform here or not
; with (.resetTransform g) everything rendered after it will not be affected by the shake (for example entity outlines, pressable body texts..)
; with (.translate g (- xtranslate) (- ytranslate)) the whole screen will be shaken

(defn- get-map-independent-render-comps []
  (filter :rendering (mapcat get-components (get-ingame-loop-entities))))

(defn- render-gui [g]
  (doseq [{render :renderfn :as component} (sort-by-order (get-map-independent-render-comps)
                                                          :order render-map-indep-order)]
    (render g component)))

(defn render-game [g]
  (rendermap g)
  (render-map-content g)
  (render-gui g))

;;

(defn- print-mouse-tile-position []
  (let [[tile-x tile-y] (get-mouse-tile-pos)]
    (str (float tile-x) " " (float tile-y))))

; y selbst aufbauen mit font height.
(defn- render-debug [g x mouseover-body]
  (let [starty 30
        lineh (get-line-height)]
    (render-readable-text g x (+ starty (* lineh 0)) (str "mouse" (get-mouse-pos)))
    (when debug/show-contentfield
      (render-readable-text g x (+ starty (* lineh 1)) (str "player content field:"  (get-player-content-field-idx))))
    (when mouseover-body
      (render-readable-text g x (+ starty (* lineh 2)) (str "maus-overbody id = "  (get-id mouseover-body))))
    (when debug/show-float-mouse-pos
      (render-readable-text g x (+ starty (* lineh 3)) (str "maus-tile x,y = "  (print-mouse-tile-position))))
    (when debug/show-tile-mouse-pos
      (render-readable-text g x (+ starty (* lineh 4)) (str "int-tile x,y = "  (mouse-int-tile-pos))))
    (when debug/show-lightning-info
      (render-mouse-tile-blocked-corners g x (+ starty (* lineh 5)) 20))
    (when debug/show-comps-count
      (render-readable-text g x (+ starty (* lineh 7)) (str "to-be-rendered-entities-from-map "  (count (to-be-rendered-entities-from-map))))
      (render-readable-text g x (+ starty (* lineh 8)) (str "map-independent-render-comps "      (count (get-map-independent-render-comps)))))))

(ingame-loop-comp :debug-infos
  (rendering :above-gui [g c]
    (when @debug-mode
      (render-debug g 25 (get-mouseover-body)))))
