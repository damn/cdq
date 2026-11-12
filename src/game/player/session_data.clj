(ns game.player.session-data
  (:require
    [clojure.walk :refer [postwalk]]
    game.mouseoverbody
    [game.session :as session :refer [load-session new-session-data save-session]]
    game.components.core
    game.components.sleeping
    game.components.burrow
    game.components.update
    [game.item.cells :refer [add-item-to-cell empty-all-item-grids]]
    [game.item.grids :refer [item-grids]]
    [game.item.in-hand :refer [item-in-hand set-item-in-hand]]
    [game.item.instance :refer [create-item-instance]]
    [game.item.boni :refer [create-equip-boni-save load-equip-boni-save]]
    [data.grid2d :refer [cells]]
    [game.maps.data :as data]
    game.maps.cell-grid
    game.maps.data
    game.maps.add
    game.player.core
    game.player.skill.learnable
    game.player.skill.selection-list
    game.utils.lightning
    game.utils.msg-to-player
    [utils.core :refer [log translate-to-tile-middle]]
    [utils.coll :refer [distinct-seq?]]
    [game.monster.spawn :refer [try-spawn]]
    [game.maps.tiledmaps :refer [get-tileproperties]]
    [game.settings :refer [version]]
    [game.status-options :refer [session]]))

(def current-character-name (atom nil))

;;

(def ^:private file (str "cdq_savegame" version ".clj"))

(defn- read-session-file []
  (try (read-string (slurp file))
    (catch java.io.FileNotFoundException e {})))

(defn- pr-str-with-meta [x]
  (binding [*print-meta* true]
    (pr-str x)))

(defn- write-session-file [data]
  (->> (postwalk session/write-to-disk data)
       (assoc (read-session-file) @current-character-name)
       pr-str-with-meta
       (spit file)))

(defn get-session-file-character-names []
  (keys (read-session-file)))

(defn get-session-file-data []
  (->> (get (read-session-file) @current-character-name)
       (postwalk session/load-from-disk)))

;;

;;; maps.load (inlined) — keep save type "maps.load/session"

(defn- place-entities [tiled-map]
  ; looping through all tiles of the map 3 times. but dont do it in 1 loop because player needs to be initialized before all monsters!
  (doseq [[posi _] (get-tileproperties tiled-map "entities" "light")]
    (game.utils.lightning/map-lightsource (translate-to-tile-middle posi)))
  (doseq [[posi value] (get-tileproperties tiled-map "entities" "monster")]
    (try-spawn (translate-to-tile-middle posi) (keyword value) :debug false)))

(defn load-maps-content
  "loads the map content in the right order"
  []
  (game.player.core/init-player (:start-position (data/get-current-map-data)))
  (doseq [map-name data/added-map-order]
    (data/do-in-map map-name
                    (let [{:keys [load-content spawn-monsters tiled-map]} (data/get-map-data map-name)]
                      (when tiled-map (place-entities tiled-map))
                      (load-content)
                      (when spawn-monsters (spawn-monsters))))
    (log "Loaded " map-name " content!")))

(def maps-load-session (reify session/Session
                         (load-session [_ mapkey]
                           (data/set-map! mapkey)
                           (load-maps-content))
                         (save-session [_]
                           @data/current-map)
                         (new-session-data [_]
                           (first data/added-map-order))))

;;; item.save (inlined) — keep save type "item.save/session"

; maybe have this information in the items
(def ^:private item-save-keys [:name :sprite-idx :count :lvl :base-armor :base-dmg
                      ; need to merge color manually.
                      :color])

(defmethod session/write-to-disk :item [item]
  (merge
    (select-keys item item-save-keys)
    (when-let [boni (:equip-boni item)]
      {:equip-boni (create-equip-boni-save boni)})))

(defmethod session/load-from-disk :item [saved-item]
  (merge
    (create-item-instance (:name saved-item)
                          (select-keys saved-item item-save-keys))
    (when-let [boni (:equip-boni saved-item)]
      {:equip-boni (load-equip-boni-save boni)})
    (select-keys saved-item [:color])))

(def item-save-session (reify session/Session
                         (load-session [_ data]
                           (empty-all-item-grids)
                           (doseq [{:keys [posi grid-type item]} (:items data)]
                             (add-item-to-cell
                               item
                               (get (grid-type item-grids) posi)))
                           (when-let [item (:item-in-hand data)]
                             (set-item-in-hand item)))
                         (save-session [_]
                           (let [item-cells-with-item (filter (comp deref :item)
                                                              (mapcat cells (vals item-grids)))]
                             {:items (doall ; needed?
                                       (map
                                         (fn [cell]
                                           (merge
                                             (select-keys cell [:posi :grid-type])
                                             {:item @(:item cell)}))
                                         item-cells-with-item))
                              :item-in-hand @item-in-hand}))
                         (new-session-data [_]
                           {:items
                            [{:posi [0 0]
                              :grid-type :belt
                              :item (create-item-instance "Heal-Potion" {:count 4})}
                             {:posi [0 0]
                              :grid-type :hands
                              :item (create-item-instance "Sword")}
                             {:posi [0 0]
                              :grid-type :inventory
                              :item (create-item-instance "Restoration")}]})))

;;; session-data

(defn- make-type [v]
  (.replace (str (ns-name (:ns (meta v))) "/" (:name (meta v))) "game." ""))

; TODO dont need vars here, because this is a function? why is it a function anyway?
(defn- session-components []
  (map (fn [[v stype]] [(deref v) (or stype (make-type v))])
       [; resets all map data -> do it before creating maps => TODO put both in one maps-session-component?
        [#'game.maps.data/session]
        ; create maps before putting entities in them
        [#'game.maps.add/session]

        ; resets all entity data -> do it before addding entities
        [#'game.components.core/clear-entities-session]

        ; reset cell-change-listeners before adding entities, because entities are listeners
        [#'game.maps.cell-grid/listeners-session]

        ; adding entities
        [#'maps-load-session "maps.load/session"]
        ; comment this out for normal gameplay as of alpha3 (no quicksave)
        ;#'game.components.core/entities-session

        ; now the order of initialisation does not matter anymore
        [#'game.player.skill.selection-list/hotkeys-session]
        [#'game.player.skill.learnable/session] ; depends on player-body/learnable-skills already initialised
        [#'item-save-session "item.save/session"] ; depends on player-body (apply item bonis)
        [#'game.status-options/session]
        [#'game.mouseoverbody/cache-session]
        [#'game.mouseoverbody/saved-mouseover-body-session]
        [#'game.components.update/session]
        [#'game.components.sleeping/session]
        [#'game.components.burrow/session]
        [#'game.utils.lightning/session]
        [#'game.utils.msg-to-player/counter-session]
        [#'game.utils.msg-to-player/message-session]]))

(assert (distinct-seq? (session-components)))
; The types need to be distinct, so load-session-component can get the data from the savegame-file that corresponds to the right session-component

(defn save-game []
  (->> (for [[component stype] (session-components)]
         (when-let [savedata (save-session component)]
           [stype savedata]))
       (into {})
       write-session-file))

(defn init [is-loaded-character]
  (let [session-file-data (when is-loaded-character
                            (get-session-file-data))]
    (doseq [[component stype] (session-components)]
      (load-session component
                    (if is-loaded-character
                      (get session-file-data stype)
                      (new-session-data component))))))
