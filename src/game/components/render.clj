(ns game.components.render
  (:require
    [engine.core :refer [make-counter ratio update]]
    [engine.render :as color :refer [draw-line fill-centered-circle get-dimensions get-frame is-stopped? render-centered-image render-centered-shape render-readable-text render-rotated-centered-image restart rgbcolor set-color]]
    [game.utils.geom :as geom]
    [utils.core :refer [define-order int-posi make-fn order-contains?]]
    [utils.numbers :refer [readable-number]]
    [game.settings :refer [in-pixel tile-height tile-width]]
    [game.components.core :refer [active add-to-removelist create-entity create-entity-no-init defcomponent defentity get-half-pxh get-half-pxw get-position update-counter!]]
    [game.components.misc :refer [delete-after-duration-component]]
    [game.components.position :refer [position-component]]
    [game.components.ingame-loop :refer [ingame-loop-comp]]
    [game.utils.tilemap :refer [screenpos-of-tilepos]]
    [game.utils.lightning :refer [set-brightness set-cached-brightness]])
  (:import org.newdawn.slick.Color))

(defn- make-rendering-props [order fnargs]
  `{:rendering true
    :order ~order
    :renderfn ~(make-fn fnargs)})

; can not check if order is keyword because order might also be a symbol and not a keyword literal
(defn- destructure-render-args
  "four cases:
  order renderfn
  renderfn
  order [graphics component] &body
  [graphics component] &body"
  [default & more]
  (cond
    (= 1 (count more)) [default more]
    (vector? (first more)) [default more]
    :else [(first more) (rest more)]))

(def render-on-map-order (define-order [:is-ground :on-ground :default :air :top-level]))

(defmacro render-on-map [& more]
  (let [[order fnargs] (apply destructure-render-args :default more)]
    `(do
       (assert (order-contains? render-on-map-order ~order))
       ~(make-rendering-props order fnargs))))

; :hpmana below gui because tooltips should be over them ... then we have tooltips which is another type of tooltips...
(def render-map-indep-order (define-order [:below-gui :hpmanabar :selfmade-gui :gui :tooltips :above-gui]))

; maybe rename to match render-on-map -> render-on-gui?
(defmacro rendering [& more]
  (let [[order fnargs] (apply destructure-render-args :gui more)]
    `(do
       (assert (order-contains? render-map-indep-order ~order))
       ~(make-rendering-props order fnargs))))

;;

(defn translate-position [[x y]]
  [(* x tile-width),(* y tile-height)])

(defcomponent rect-render [pxw pxh]
  {:shape (geom/rectangle -1 -1 pxw pxh)}
  (render-on-map [g _ c render-posi]
    (render-centered-shape g (:shape c) render-posi)))

(defn- render-rotated-lighted-image [g entity render-posi image apply-light]
  (when apply-light
    (if (:always-in-sight @entity)
      (set-cached-brightness image (int-posi (get-position entity))) ; brightness like map tiles explored/unexplored
      (set-brightness image (get-position entity))))
  (if-let [angle (:angle (:rotation @entity))]
    (render-rotated-centered-image g image angle render-posi)
    (render-centered-image image render-posi)))

(defcomponent image-render [image & {:keys [order apply-light]
                                     :or {order :default apply-light true}}]
  (render-on-map order [g entity c render-posi]
    (render-rotated-lighted-image g entity render-posi (:image c) apply-light))
  {:depends [:position]
   :image image})

(defn- update-animation
  "control is a function with (fn [entity]) which must return the new animation key and can also update some stuff"
  [delta {:keys [akey control] :as component} entity]
  (let [last-akey akey
        new-akey (control entity)
        _ (assert (keyword? new-akey))
        new-animation (cond-> (new-akey component)
                        (not= last-akey new-akey) restart
                        :then                     (update delta))]
    (swap! entity update-in [(:type component)]
                assoc
                :akey    new-akey
                new-akey new-animation)))

(defn current-animation [entity]
  (let [{:keys [akey] :as c} (:animation @entity)]
    (akey c)))

(defn- render-animation [g entity {:keys [apply-light] :as component} render-posi]
  ; can be nil when mapchange -> render happends before update for a component (else update happends before render)
  (when-not (:akey component)
    (update-animation 1 component entity))
  (render-rotated-lighted-image g entity render-posi (get-frame (current-animation entity)) apply-light))

; types-animations is a map of keyword to animation and control returns a keyword which animation to play next.
(defcomponent animation [:control types-animations & {:keys [order apply-light]
                                                      :or {order :default apply-light true}}]
  (render-on-map order render-animation)
  (active update-animation)
  {:depends [:position]
   :akey nil
   :apply-light apply-light}
  types-animations)

(defn single-animation-component [animation & more]
  (apply animation-component (constantly :default) {:default animation} more))

(defentity animation-entity [:position :animation :opt :opt-def :order :air]
  (position-component position)
  {:type :always-in-sight}
  (single-animation-component animation :order order :apply-light false)
  (merge {:type :delete-after-stopped}
         (active [_ _ entity]
           (when (-> (:animation @entity) :default is-stopped?)
             (add-to-removelist entity)))))

; FIXME animation that is passed to reload session args == initial one with cnt = 0 ....
; animation component 'current' ?

;;;

(defentity create-circle-render-effect
  {:save-session true}
  [position radius color duration]
  (position-component position)
  (delete-after-duration-component duration)
  (let [shape (geom/circle [-1 -1] (in-pixel radius))] ; TODO shape rendering component?
    (merge {:type :visual}
           (render-on-map [g _ c render-posi]
                          (render-centered-shape g shape render-posi color)))))

(defn- drawfatline [g [x y] [ex ey]]
  (doseq [x  [(dec x)  x  (inc x)]
          y  [(dec y)  y  (inc y)]
          ex [(dec ex) ex (inc ex)]
          ey [(dec ey) ey (inc ey)]]
    (draw-line g x y ex ey)))

(defentity create-lines-render-effect [healer healed-bodies duration]
  (position-component (get-position healer))
  (delete-after-duration-component duration)
  {:type :always-in-sight}
  (merge {:type :visual}
         (render-on-map :air [g _ component start]
                        (set-color g color/green)
                        (doseq [end (map #(translate-position (get-position %)) healed-bodies)]
                          (drawfatline g start end)))))

(defentity ^:private create-line-render-effect* [start end duration color thin]
  (position-component start)
  (delete-after-duration-component duration)
  {:type :always-in-sight}
  (merge {:type :visual}
         (render-on-map :air [g _ component start]
                        (set-color g color)
                        (if thin
                          (draw-line   g start (translate-position end))
                          (drawfatline g start (translate-position end))))))

(defn create-line-render-effect [start end duration & {color :color thin :thin}]
  (create-line-render-effect* start end duration color thin))

(defn render-above-body [g body [x y] image & {ypuffer :ypuffer :or {ypuffer 0}}]
  (let [posi [x
              (- y (get-half-pxh body) (/ (second (get-dimensions image)) 2) ypuffer)]]
    (render-centered-image image posi)))

(defn show-string-effect [body duration color string]
  (let [tilepos (get-position body)]
    (ingame-loop-comp :string-effect
      {:counter (make-counter duration)
       :color (Color. ^Color color)}  ; copy before changing it @ set!
      (active [delta {:keys [color counter] :as c} entity]
        (set! (.a ^Color color) (- 1 (ratio counter)))
        (when (update-counter! entity delta c)
          (add-to-removelist entity)))
      (rendering :below-gui [g {:keys [color counter] :as c}]
        (let [[rx ry] (screenpos-of-tilepos tilepos)]
          (render-readable-text g rx (- ry (get-half-pxh body) (* 50 (ratio counter)))
            :shift false
            :above true
            :centerx true
            :background false
            color string))))))

(defn show-gains-hp-effect [body gained]
  (show-string-effect body 2000 (rgbcolor :g 1 :r 0.2 :b 0.2) (str "+" (readable-number gained))))

(defn show-gains-mana-effect [body gained]
  (show-string-effect body 2000 (rgbcolor :g 0.2 :r 0.2 :b 1) (str "+" (readable-number gained))))

(defn circle-around-body-render-comp
  "target entity is not the entity of this component (used at sub-entities that only share position)"
  [target color order]
  (merge {:type :visuals}
         (render-on-map order [g _ c render-posi]
           (let [radius (+ 2 (get-half-pxw target))]
             (fill-centered-circle g radius render-posi color)))))
