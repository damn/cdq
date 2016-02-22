(ns game.maps.add
  (:require [game.maps.data :as data]
            [game.settings :refer [get-setting]]
            game.utils.lightning)
  (:use utils.core
        game.session
        (game.maps cell-grid contentfields minimap tiledmaps)
        data.grid2d))

(defnks add-maps-data
  [:map-key :cell-grid :load-content :pretty-name :rand-item-max-lvl :start-position
   :opt :tiled-map :sprite-sheet :details-sprite-sheet :spawn-monsters]
  {:pre [(xor tiled-map sprite-sheet)
         (not-any? #{map-key} (data/get-map-keys))]}
  (let [w (width cell-grid)
        h (height cell-grid)]
    (data/add-map map-key
                  (merge
                    (dissoc argsmap :map-key)
                    {:j-cell-grid (delay (create-jcell-grid cell-grid))
                     :contentfields (create-mapcontentfields w h)
                     :minimap-image (create-minimap-image w h)
                     :minimap-bodies (atom #{})
                     :explored-tile-corners (atom (create-grid w h (constantly false)))})))
  (log "Finished creating " map-key)
  ;(check-not-allowed-diagonals cell-grid)
  )

(def- map-create-fns [])

(defmacro create-map-fn [& args]
  `(alter-var-root #'game.maps.add/map-create-fns conj (fn [] ~@args)))

(def session (reify game.session/Session
               (load-session [_ _]
                 (require (get-setting :map-impl-namespace))
                 (doseq [create map-create-fns]
                   (create)))
               (save-session [_])
               (new-session-data [_])))

(defn tilemap-make-argmap [bodyseq]
  (let [temp (apply hash-map bodyseq)
        tiled-map (construct-tiledmap (str "maps/" (:file temp))
                                      game.utils.lightning/lightmanager)]
    (merge (dissoc temp :file)
           {:start-position (translate-to-tile-middle (get-player-entity-start-position tiled-map))
            :tiled-map tiled-map
            :cell-grid (create-cell-grid-from-tiled-map tiled-map)})))

(defn generatedmap-make-argmap [bodyseq]
  (let [{:keys [grid blockprops calc-sprite-posi details-calc-sprite-posi] :as temp} (first bodyseq)]
    (merge (dissoc temp :grid :blockprops :calc-sprite-posi :details-calc-sprite-posi)
           {:cell-grid (create-grid-from-gen-grid grid blockprops calc-sprite-posi details-calc-sprite-posi)})))

(defn call-add-maps-data [mapkey make-argmap-fn & bodyseq]
  (apply add-maps-data
         (apply concat  ; concat so the map is a sequence of its elements
                (assoc (make-argmap-fn bodyseq)
                       :map-key mapkey))))

(defmacro deftilemap [mapkey & body]
  `(create-map-fn
     (call-add-maps-data ~mapkey tilemap-make-argmap ~@body)))

(defmacro defgeneratedmap [mapkey body]
  `(create-map-fn
     (call-add-maps-data ~mapkey generatedmap-make-argmap ~body)))

; --> mach prozedural generierte maps mit prostprocessing (fill-singles/set-cells-behind-walls-nil/remove-nads/..?)
;& assertions 0 NADS z.b. ...?







