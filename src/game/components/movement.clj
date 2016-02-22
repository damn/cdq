(ns game.components.movement
  (:use
    utils.core
    (game [settings :only (tile-width)])
    (game.components core body position [update :only (max-delta)])
    (game.maps cell-grid)
    (game.utils geom raycast)))

(defn- create-new-position
  [delta [vx vy] [old-x old-y] speed]
  [(+ old-x (* vx speed delta))
   (+ old-y (* vy speed delta))])

(defn try-move-projectile
  "returns true if blocked, nil if moved."
  [new-posi projectile]
  (let [id (get-id projectile)
        half-w (get-half-width projectile)
        half-h (get-half-height projectile)
        {:keys [hit-effects hits-side already-hit-bodies piercing hits-wall-effect]} (get-component projectile :projectile-collision)
        touched-cells (calc-touched-cells new-posi half-w half-h)
        hit-body (find-first #(and (not (contains? already-hit-bodies %))
                                   (= hits-side (get-side %)))
                             (get-other-solid-bodies new-posi half-w half-h id touched-cells))
        movement-type (get-movement-type projectile)
        ; TODO PROJECTILE COLLISION to entityPROJECTILE and out of movement logic?
        blocked (cond hit-body
                      (do
                        (update-in! projectile [:projectile-collision :already-hit-bodies] conj hit-body)
                        (runmap #(% hit-body) hit-effects)
                        (not piercing))

                      (some #(cell-blocked? % movement-type) touched-cells) ; TODO mit x-axis und y-axis -> 2-3 abfragen anstatt 4. Lohnt?
                      (do
                        (hits-wall-effect new-posi)
                        true))]
    (when blocked (add-to-removelist projectile) true)))

(defn- update-position-projectile
  [delta v x-direction y-direction speed body]
  (let [new-position (create-new-position delta v (get-position body) speed)
        blocked (try-move-projectile new-position body)]
    (when-not blocked
      (swap-position! body new-position))
    blocked))

; TODO gleicher code wie bei components.body/blocked-location? -> f�ge zusammen
(defn- blocked-for-body? [body movement-type noclip position touched-cells]
  (or
    (some #(cell-blocked? % movement-type) touched-cells)
    (if-not noclip
      ; checks if colliding with other solid bodies -> rename?
      (seq (get-other-solid-bodies position body touched-cells)))))

(use 'clojure.set)

(comment
  ; f�r testen wie oft try-moved/update-posi-def average ist. -> so 1.4 ca.
  (def try-moved (atom 0))
  (def update-posi-def (atom 0))
  (use 'game.components.ingame-loop)
  (use 'game.components.render)
  (ingame-loop-comp :debug-infos
    (rendering :above-gui [g c]
      (render-readable-text g 50 90 (str "try-moved  " @try-moved))
      (render-readable-text g 50 110 (str "update-posi-def " @update-posi-def))
      (render-readable-text g 50 130 (str "avg " (if-not (zero? @update-posi-def) (float (/ @try-moved @update-posi-def))))))))

; TODO get-cells performance... -> mit cached-adj-cells ersetzen wenns sein soll
; ~ 1,7 1,8 % der runtime ... _> could be improved...

; auch vlt testen ob dieses remaining... zeug nen unterschied macht
; ebenfalls testen ob threshhold nen unterschied macht...
; -> best. anzahl an gegnern spawnen und auch vorgegebenem weg den spieler bewegen & messen?
; -> um vergleichen zu können ... also erst 3x schauen ob gleiche ergebnisse ...
; oder erstmal so lassen .... und auf wichtigeres konzentrieren
(defn- try-move [body v xdir ydir delta speed movement-type noclip]
  (let [new-posi (create-new-position delta v (get-position body) speed)
        to-check-tiles (get-to-check-tiles body new-posi xdir ydir) ; rename new-tiles, new-cells ?
        to-check-cells (get-cells to-check-tiles)
        blocked (blocked-for-body? body movement-type noclip new-posi to-check-cells)]
    (when-not blocked
      ; manual :posi-changed of solid body because of performance issues (calc-touched-cells)
      (let [all-tiles (apply get-touched-tiles (get-body-bounds body)) ; == all-previously-touched-tiles?
            remaining (get-cells (difference (set all-tiles) to-check-tiles))] ; == all that remain to be checked?
        ; to-check-cells not nil because not blocked
        ; remaining not nil because not moving there -> update-touched-cells
        (update-touched-cells body (concat remaining to-check-cells)))
      (swap-position! body new-posi :filter-body true) ; TODO does more than swap-position also calls posi-changed..
      (update-occupied-cell body) ; update after body has changed position! touched cells already uses cells corresponding to new position
      v)))

(defn- calc-threshold
  "Until entity is in the tile next to target and does not need to slide anymore"
  [half-size] ; assert entity half-w = half-h
  (Math/abs (.y (normalise (vector2f
                              (- 0.5 (inc half-size))
                              (- 0.5 half-size))))))

(defn- update-position-solid [delta {vx 0 vy 1 :as v} xdir ydir speed body]
  (let [threshold (calc-threshold (get-half-width body))
        movement-type (get-movement-type body)
        noclip (:noclip (get-component body :movement))
        success (or
                  (try-move body v xdir ydir delta speed movement-type noclip)
                  (when (or (is-player? body) (> (Math/abs (float vx)) threshold))
                    (try-move body [xdir 0] xdir ydir delta speed movement-type noclip))
                  (when (or (is-player? body) (> (Math/abs (float vy)) threshold))
                    (try-move body [0 ydir] ydir ydir delta speed movement-type noclip)))]
    (when success
      ; hier nicht rotiert um bewegten vektor(success) sondern um gew�nschten vektor da sonst zittern die monster herum
      (when-let [rotation (get-component body :rotation)]
        ((:moved rotation) body v))
      (when (is-player? body)
          (assoc-in! body [:movement :play-move-animation] true)))))

(defn- update-component-movement
  [delta {:keys [control-update speed] :as c} body]
  (let [v (control-update body c delta)]
    (assert (or (nil? v)
                (zero? (length v))
                (normalised? v)))
    (when (and v
               (not (zero? (length v))))
      (let [{vx 0 vy 1 :as v} (vec-posi v)
            xdir (Math/signum (float vx))
            ydir (Math/signum (float vy))
            update-position (if (get-component body :projectile-collision)
                              update-position-projectile
                              update-position-solid)]
        (update-position delta v xdir ydir speed body)))))

; max speed damit kleinere bodies beim direkten dr�berfliegen nicht �bersprungen werden (an den ecken werden sie trotzdem �bersprungen..)
; schnellere speeds m�ssten in mehreren steps sich bewegen.
; 0.0108 tile/ms
; 520 px/s
(def max-speed (* 1000 (/ min-solid-pxsize max-delta)))

(defn in-tiles-per-ms [px-per-s] (/ px-per-s 1000 tile-width))

; speed is pixels/s
(defcomponent movement [control speed :movement-type]
  {:pre [(#{:air :ground} movement-type)
         (>= speed 1) ; just to check that its now in px/s
         (<= speed max-speed)]}
  (active update-component-movement)
  control
  {
;   :init (fn [entity]
;           ;  (not asserted becuz burrowed are burrowing @init... so not solid anymore)
;           (assert (or (get-component entity :projectile-collision) ;update-position-projectile
;                       (is-solid? entity)))) ;update-position-solid
   :play-move-animation false
   :speed (in-tiles-per-ms speed)})

(defn projectile-movement-component [move-vector speed]
  (movement-component
    {:control-update (constantly move-vector)}
    speed
    :air))


