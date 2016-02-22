(ns game.components.body
  (:require [game.debug-settings :as debug]
            [game.utils.geom :as geom])
  (:use
    utils.core
    game.settings
    (game.components core render position misc)
    (game.maps cell-grid contentfields
               [camera :only (get-camera-position)])
    game.utils.raycast))

(defn is-multiple-cell? [body] (:is-multiple-cell (get-component body :body)))

(defn get-body-bounds [body]
  [(get-position body) (get-half-width body) (get-half-height body)])

(defn calc-touched-cells
  ([body]
    (apply calc-touched-cells (get-body-bounds body)))
  ([position half-w half-h]
    (get-cells
      (geom/get-touched-tiles position half-w half-h))))

(defn inside-cell? [body cell]
  (let [touched-cells (calc-touched-cells body)]
    (and (= 1 (count touched-cells))
         (= cell (first touched-cells)))))

(defn- set-occupied-cell [body]
  (if (is-multiple-cell? body)
    (let [cells (calc-touched-cells body)]
      (runmap #(swap! % update-in [:occupied] conj body) cells)
      (assoc-in! body [:body :occupied-cells] cells))
    (let [cell (get-cell (get-position body))]
      (assert cell)
      (swap! cell update-in [:occupied] conj body)
      (assoc-in! body [:body :occupied-cell] cell))))

(defn- remove-from-occupied-cell [body]
  (if (is-multiple-cell? body)
    (runmap #(swap! % update-in [:occupied] disj body) (:occupied-cells (get-component body :body)))
    (swap! (get-occupied-cell body) update-in [:occupied] disj body)))

(defn update-occupied-cell [body]
  (remove-from-occupied-cell body)
  (set-occupied-cell body))

(defn- set-touched-cells
  ([body]
    (set-touched-cells body (calc-touched-cells body)))
  ([body new-cells]
    {:pre [(not-any? nil? new-cells)]}
    (assoc-in! body [:body :cached-touched-cells] new-cells)
    (runmap #(add-body % body) new-cells)))

(defn- remove-from-touched-cells
  ([body]
    (remove-from-touched-cells body (get-cached-touched-cells body)))
  ([body cached-cells]
    (runmap #(remove-body % body) cached-cells)))

(defn update-touched-cells
  ([body]
    (update-touched-cells body (calc-touched-cells body)))
  ([body new-cells]
    (let [cached-cells (get-cached-touched-cells body)]
      (when-not (= new-cells cached-cells)
        (remove-from-touched-cells body cached-cells)
        (set-touched-cells body new-cells)))))

; f�r mouseoverbodies damit man gut klicken kann
; solid da f�r projektile soll es net gelten
; entweder dies oder max-speed muss festgelegt werden.
(def min-solid-pxsize 8)
(def half-min-solid-pxsize (/ min-solid-pxsize 2))

(declare body-info-renderfn)

(defcomponent body [:solid :side :half-pxw :half-pxh :mouseover-outline]
  {:pre [(= (class solid) java.lang.Boolean)
         (#{:monster :player :no-side} side)
         (>= half-pxw (if solid half-min-solid-pxsize 0))
         (>= half-pxh (if solid half-min-solid-pxsize 0))
         (< (/ half-pxw tile-width) 2)
         (< (/ half-pxh tile-height) 2)]} ; wg. rendering on screen (max ungetestet - ist etwas kleiner als 3 die tats�chliche maximale size)
  (render-on-map :air body-info-renderfn)
  (let [half-w (/ half-pxw tile-width)
        half-h (/ half-pxh tile-height)]
    {:half-width half-w
     :half-height half-h
     :is-multiple-cell (geom/does-not-fit-in-a-tile? half-w half-h)
     :depends [:position]
     :init (if-not solid
             set-touched-cells
             (fn [entity]
               (set-touched-cells entity)
               (set-occupied-cell entity)))
     :destruct (if-not solid
                 remove-from-touched-cells
                 (fn [entity]
                   (remove-from-touched-cells entity)
                   (remove-from-occupied-cell entity)))
     :posi-changed (if-not solid
                     update-touched-cells
                     (fn [entity] ; posi changed of solid entities @ update-movement manually called
                       (update-touched-cells entity)
                       (update-occupied-cell entity)))}))

(defnks create-body
  [:solid :opt :pxw :pxh :dimensions :mouseover-outline :opt-def :side :no-side ]
  (let [pxw (or pxw (dimensions 0))
        pxh (or pxh (dimensions 1))]
    (assert (and pxw pxh))
    (body-component solid side (/ pxw 2) (/ pxh 2) mouseover-outline)))

;; Body Geometry

(defn- body-rect [body] ; cache in body and update @ :posi-changed?
  (let [[[x y] half-w half-h] (get-body-bounds body)]
    [(- x half-w) ; 22% faster with 4 float casts
     (- y half-h)
     (* 2 half-w)
     (* 2 half-h)]))

; cache in body and update @ :posi-changed?
(def rect-shape (comp geom/rectangle body-rect))

(defn circle-collides? [p radius body]
  (geom/collides? (geom/circle p radius)
                  (rect-shape body)))

(defn- point-in-body? [p body]
  (geom/point-in-rect? p (body-rect body)))

(defn get-bodies-at-position [position]
  (when-let [cell (get-cell position)]
    (filter #(point-in-body? position %)
            (map get-entity (get-body-ids cell)))))

(defn bodies-in-range? [b1 b2 range-sqr]
  (geom/in-range? (get-position b1) (get-position b2) range-sqr))

; circle-collides creates a new Circle shape for every check -> could be optimized
(defn get-touched-bodies
  [posi radius]
  (filter #(circle-collides? posi radius %)
          (get-bodies-from-cells
            (calc-touched-cells posi radius radius))))

(defn get-other-solid-bodies
  ([position half-w half-h body-id touched-cells]
    (filter
      #(and
         (not= (get-id %) body-id)
         (is-solid? %)
         (geom/rect-collision? [position half-w half-h] (get-body-bounds %)))
      (get-bodies-from-cells touched-cells)))
  ([position half-w half-h touched-cells]
    (get-other-solid-bodies position half-w half-h -1 touched-cells))
  ([position body touched-cells]
    (get-other-solid-bodies position (get-half-width body) (get-half-height body) (get-id body) touched-cells)))

(defn colliding-with-other-solid-bodies? [body]
  (boolean
    (not-empty
      (get-other-solid-bodies (get-position body) body (get-cached-touched-cells body)))))

(defn blocked-location?
  ([posi half-w half-h movement-type]
    (let [touched-cells (calc-touched-cells posi half-w half-h)]
      (or
        (some #(cell-blocked? % movement-type) touched-cells)
        (seq (get-other-solid-bodies posi half-w half-h touched-cells)))))
  ([posi body movement-type]
    (blocked-location? posi (get-half-width body) (get-half-height body) movement-type))
  ([posi body]
    (blocked-location? posi body (get-movement-type body))))

; TODO adjacent-cells could be nil -> :middle @cell -> NPE
(defn find-nearby-valid-location [body posi]
  (when-let [cell (find-first #(not (blocked-location? (:middle @%) body))
                              (cached-get-adjacent-cells (get-cell posi)))]
    (:middle @cell)))

(defn teleport
  "searches nearby positions for a free one if target is blocked
   if none exists -> just teleports to the blocked position."
  [body posi]
  (swap-position! body
                  (if-not (blocked-location? posi body)
                    posi
                    (if-let [valid (find-nearby-valid-location body posi)]
                      valid
                      posi))))
;;

(defn- entity-on-screen? [entity] ; TODO posi-in-rect hier? / does not take into account body size
  (let [[x y] (get-position entity)
        x (float x)
        y (float y)
        [cx cy] (get-camera-position)
        px (float cx)
        py (float cy)
        xdist (Math/abs (- x px))
        ydist (Math/abs (- y py))]
    (and
      (<= xdist (inc half-display-w-in-tiles))
      (<= ydist (inc half-display-h-in-tiles)))))

(defn is-burrowed? [entity]
  (if-let [c (get-component entity :burrow)]
    (:burrowed c)))

(defn is-affectable? [entity]
  (not (is-burrowed? entity)))

(defn- is-body-visible? [entity]
  (not (is-burrowed? entity)))

; in sight of player-body -> ray from player-body! ; not in sight of some light-source ..
(defn- in-sight? [entity]
  (or
    (when @debug-mode debug/entities-always-in-los)
    (get-component entity :always-in-sight)
    (and (is-body-visible? entity)
         (not (ray-blocked? (get-position player-body) (get-position entity))))))

; hier entity mit position component ben�tigt - kein body
; Has to work with entities who have :position aber kein :body!
(defn on-screen-and-in-sight? [entity]
  (and (entity-on-screen? entity)
       (in-sight? entity)))

; Assert: not jumping over tiles, so speed is <1 tile at max-delta

(defn- get-direction-fn [d] (if (pos? d) + -))

(defn- get-to-check-tiles-big [body [x y] xdir ydir]
  (let [half-w (get-half-width body)
        half-h (get-half-height body)
        x (float x),y (float y),half-w (float half-w),half-h (float half-h)]
    (set
      (concat
        (when-not (zero? xdir)
          (let [x (int ((get-direction-fn xdir) x half-w))
                t (int (- y half-h))
                b (int (+ y half-h))]
            (for [y (range t (inc b))]
              [x y])))
        (when-not (zero? ydir)
          (let [y (int ((get-direction-fn ydir) y half-h))
                l (int (- x half-w))
                r (int (+ x half-w))]
            (for [x (range l (inc r))]
              [x y])))))))

(defn- get-to-check-tiles-small [body [x y] xdir ydir]
  (let [half-w (get-half-width body)
        half-h (get-half-height body)
        x (float x),y (float y),half-w (float half-w),half-h (float half-h)]
    (set
      (concat
        (when-not (zero? xdir)
          (let [x (int ((get-direction-fn xdir) x half-w))
                t (int (- y half-h))
                b (int (+ y half-h))]
            [[x t] [x b]]))
        (when-not (zero? ydir)
          (let [y (int ((get-direction-fn ydir) y half-h))
                l (int (- x half-w))
                r (int (+ x half-w))]
            [[l y] [r y]]))))))

(defn get-to-check-tiles [body new-posi xdir ydir]
  (if (is-multiple-cell? body)
    (get-to-check-tiles-big body new-posi xdir ydir)
    (get-to-check-tiles-small body new-posi xdir ydir)))

;;

(defn get-other-bodies-in-adjacent-cells [entity]
  (->> entity  ; TODO could be faster using occupied-cell instead of get-position/get-cell...
    get-position
    get-cell
    cached-get-adjacent-cells
    get-bodies-from-cells
    (remove #(= (get-id %) (get-id entity)))))

;;

(defn get-dist-to-player
  "only works for single cell bodies, may return nil when outside of potential-field"
  [entity]
  (:dist-to-player @(get-occupied-cell entity)))
