(ns game.player.session-data
  (:require [clojure.walk :refer [postwalk]]
            game.mouseoverbody
            [game.session :as session]
            (game.components core sleeping burrow update)
            game.item.save
            (game.maps cell-grid data add load)
            (game.player.skill learnable selection-list)
            (game.utils lightning msg-to-player)
            [utils.core :as utils])
  (:use utils.core
        (game settings status-options session)))

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

(defn- make-type [v] 
  (.replace (str (ns-name (:ns (meta v))) "/" (:name (meta v))) "game." ""))

; TODO dont need vars here, because this is a function? why is it a function anyway?
(defn- session-components []
  (map #(vector @% (make-type %))
       [; resets all map data -> do it before creating maps => TODO put both in one maps-session-component?
        #'game.maps.data/session
        ; create maps before putting entities in them
        #'game.maps.add/session

        ; resets all entity data -> do it before addding entities
        #'game.components.core/clear-entities-session

        ; reset cell-change-listeners before adding entities, because entities are listeners
        #'game.maps.cell-grid/listeners-session

        ; adding entities
        #'game.maps.load/session
        ; comment this out for normal gameplay as of alpha3 (no quicksave)
        ;#'game.components.core/entities-session

        ; now the order of initialisation does not matter anymore
        #'game.player.skill.selection-list/hotkeys-session
        #'game.player.skill.learnable/session ; depends on player-body/learnable-skills already initialised
        #'game.item.save/session ; depends on player-body (apply item bonis)
        #'game.status-options/session
        #'game.mouseoverbody/cache-session
        #'game.mouseoverbody/saved-mouseover-body-session
        #'game.components.update/session
        #'game.components.sleeping/session
        #'game.components.burrow/session
        #'game.utils.lightning/session
        #'game.utils.msg-to-player/counter-session
        #'game.utils.msg-to-player/message-session]))

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
