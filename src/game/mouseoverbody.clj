(ns game.mouseoverbody
  (:use
    utils.core
    (engine input render)
    (game settings session [ingame-gui :only (mouse-inside-some-gui-component?)])
    (game.components core body destructible render ingame-loop)
    game.maps.cell-grid
    (game.utils geom tilemap)))

(defn- get-current-mouseover-body []
  (when-not (mouse-inside-some-gui-component?)
    (let [tile-posi (get-mouse-tile-pos)
          hits (get-bodies-at-position tile-posi)]
      (when hits
        (let [; der als letztes gerenderte ist ganz am obersten
              ; jeder body hat :rendering also brauch net removen
              hits (filter on-screen-and-in-sight?
                           (reverse
                             (sort-by-order hits
                                            #(:order (find-first :rendering (get-components %)))
                                            render-on-map-order)))
              ;_ (println "hits: " (map #(map :type (get-components %)) hits))
              hits (if (> (count hits) 1) (remove is-player? hits) hits)]
          (first hits))))))

(def ^:private cache (atom nil)) ; cache exists so that only one time in a frame get-current-mouseover-body is calculated
(def cache-session (atom-session cache :save-session false))

; when played holds leftmouse down the mouseoverbody stays the same as long as mouse is down, even if it is not under the mouse anymore
(def saved-mouseover-body (atom nil))
(def saved-mouseover-body-session
  (atom-session saved-mouseover-body :save-session false))

(defn get-mouseover-body []
  (if-let [body @saved-mouseover-body]
    (when (exists? body) body)
    (if @debug-mode
      (get-current-mouseover-body)
      (when (and @cache (exists? @cache)) @cache))))

; -> der map-indep-comp :check-pressable-mouseoverbody benutzt get-mouseover-body und k�nnte vor
; update-saved drankommen!
; deshalb nur returnt wenn er exists.

(defn- keep-saved? [body]
  (and (is-leftbutton-down?)
       (exists? body)
       (on-screen-and-in-sight? body)))

(ingame-loop-comp :update-saved-and-cache
  (active [_ _ _]
    (reset! cache (get-current-mouseover-body))
    (when (and @saved-mouseover-body
               (not (keep-saved? @saved-mouseover-body)))
      (reset! saved-mouseover-body nil))
    (when-let [body @cache]
      (when (and (is-leftbutton-down?)
                 (not @saved-mouseover-body)
                 (attackable-by-player? body))
        (reset! saved-mouseover-body body)))))


