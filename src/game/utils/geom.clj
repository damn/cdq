(ns game.utils.geom
  (:use
    [utils.core :only (approx-numbers)]
    [engine.input :only (get-mouse-pos)]
    [game.settings :only (half-screen-w half-screen-h)]
    game.components.core)
  (:import
    (org.newdawn.slick.geom Shape Vector2f Rectangle Circle Transform)))

(defn vec-posi
  "Takes Vector2f or [x y] and returns [x y]."
  [v]
  (if (instance? Vector2f v)
    [(.x ^Vector2f v)
     (.y ^Vector2f v)]
    v))

(defn rectangle
  ([[x y w h]]
   (rectangle x y w h))
  ([x y w h]
   (Rectangle. x y w h)))

(defn circle [[x y] radius]
  (Circle. x y radius))

(defn set-radius [circle pixels]
  (.setRadius ^Circle circle pixels))

(defn rotate [shape angle x y]
  (.transform
   shape
   (Transform/createRotateTransform (Math/toRadians angle) x y)))

(defn vector2f
  ([x y]
   (Vector2f. x y))
  ([p]
   (if (instance? Vector2f p)
     p
     (let [[x y] p]
       (Vector2f. x y)))))

(defn scale     [v n]   (.scale    ^Vector2f v n))
(defn normalise [v]     (.normalise ^Vector2f v))
(defn add       [v1 v2] (.add ^Vector2f v1 ^Vector2f v2))
(defn length    [v]     (.length ^Vector2f v))

(defn normalised? [v]
  (approx-numbers 1 (.length ^Vector2f v) 0.001))

(defn get-distance [p1 p2]
  (.distance ^Vector2f (vector2f p1) (vector2f p2)))

(defn get-normal-vectors [^Vector2f v]
  (let [x (.x v),y (.y v)]
    [(Vector2f. (- y) x)
     (Vector2f. y (- x))]))

(defn direction-vector [source target]
  {:post [(or (normalised? %) (zero? (.length ^Vector2f %)))]}
  (let [[sx sy] source
        [tx ty] target]
    (.normalise (Vector2f. (- tx sx) (- ty sy)))))

(defn entity-direction-vector [source target]
  (direction-vector (get-position source) (get-position target)))

(defn get-vector-to-player [body]
  (entity-direction-vector body player-body))

(defn get-vector-away-from-player [body]
  (entity-direction-vector player-body body))

(defn get-angle-from-vector
  "converts theta of slick Vector2f to angle from top (top is 0 degree, moving right is 90 degree etc.)"
  [v]
  (let [angle (+ 90 (.getTheta ^Vector2f (vector2f v)))]
    (if (> angle 360)
      (- angle 360)
      angle)))

(defn vector-from-angle [angle]
  {:post [(normalised? %)]}
  (.add (Vector2f. 0 -1) (double angle)))

(defn collides? [^Shape a ^Shape b]
  (or (.intersects a b) (.contains a b) (.contains b a)))

(defn point-in-rect?
  ([posi [x y w h]]
    (point-in-rect? posi x y w h))
  ([[x y] rectx recty rectw recth]
    (point-in-rect? x y rectx recty rectw recth))
  ([x y rectx recty rectw recth]
    (and
      (>= x rectx)
      (>= y recty)
      (<= x (+ rectx rectw))
      (<= y (+ recty recth)))))

(defn rect-collision?
  "x,y centerposition."
  [[[x1 y1] half-w1 half-h1]
   [[x2 y2] half-w2 half-h2]]
  (and
    (let [x1 (float x1),x2 (float x2),half-w1 (float half-w1),half-w2 (float half-w2)]
      (< (Math/abs (- x1 x2)) (+ half-w1 half-w2)))
    (let [y1 (float y1),y2 (float y2),half-h1 (float half-h1),half-h2 (float half-h2)]
      (< (Math/abs (- y1 y2)) (+ half-h1 half-h2)))))

(defn tiles-inside-rect
  "x,y centerposition. tiles are integer positions. First tile is top left tile!"
  ; top left first ... because used in such a way @ prebuilt placement
  [[[x y] half-w half-h]]
  (let [topy (- y half-h)
        bottomy (+ y half-h)
        leftx (- x half-w)
        rightx (+ x half-w)
        top-tile (Math/floor topy)
        bottom-tile (Math/floor bottomy)
        left-tile (Math/floor leftx)
        right-tile (Math/floor rightx)]
    (for [x (range left-tile (inc right-tile)) ;include end-tile
          y (range top-tile (inc bottom-tile))]
      [(int x) (int y)]))) ; da sonst [3.0 4.0] z.B.

; noch schneller:
; test of xdist > range
; test ob ydist > range
; -> gar kein sqrd erstmal?
; TODO : ber�cksichtige gr�sse der bodies mit ein
(defn in-range?
  "collision of two circles."
  [[x1 y1] [x2 y2] range-square]
  (let [x1 (float x1)
        x2 (float x2)
        xdist (Math/abs (- x1 x2))
        xdist-sqr (* xdist xdist)]
    (if (> xdist-sqr range-square)
      false
      (let [y1 (float y1)
            y2 (float y2)
            ydist (Math/abs (- y1 y2))
            ydist-sqr (* ydist ydist)]
        (if (> ydist-sqr range-square)
          false
          (<= (+ xdist-sqr ydist-sqr) range-square))))))

(defn- get-touched-tiles-small
  "for <1 cell bodies, touching maximum 4 tiles"
  [[x y] half-w half-h]
  (let [x (float x)
        y (float y)
        half-w (float half-w)
        half-h (float half-h)
        r (int (+ x half-w))
        l (int (- x half-w))
        t (int (- y half-h))
        b (int (+ y half-h))]
    (distinct [[l t] [l b] [r t] [r b]])))

(defn- get-touched-tiles-big
  "iterates from top-left to bottom-right tile. works for >1 cell bodies"
  [[x y] half-w half-h]
  (let [x (float x)
        y (float y)
        half-w (float half-w)
        half-h (float half-h)
        r (int (+ x half-w))
        l (int (- x half-w))
        t (int (- y half-h))
        b (int (+ y half-h))]
    (distinct (for [x (range l (inc r))
                    y (range t (inc b))]
                [x y]))))

(defn does-not-fit-in-a-tile? [half-w half-h]
  (or
    (>= half-w 0.5)
    (>= half-h 0.5)))

(defn get-touched-tiles [posi half-w half-h]
  (if (does-not-fit-in-a-tile? half-w half-h)
    (get-touched-tiles-big posi half-w half-h)
    (get-touched-tiles-small posi half-w half-h)))

(defn add-or-subtract-angle
  "man will current in richtung new rotieren. muss man dazu add or subtract machen f�r den k�rzesten weg?
  returns add or subtract-function"
  [current-angle new-angle]
	(if (<= current-angle 180)
   (if (or (<= new-angle current-angle) (>= new-angle (+ 180 current-angle)))
     -
     +)
   (if (and (>= new-angle (- current-angle 180)) (<= new-angle current-angle))
     -
     +)))

(defn get-angle-to-position
  "Treat Position a the middle of the coordinate sytem and the angle is from the top
  so if target is right of origin -> 90� below origin -> 180�"
  [{originx 0 :as origin} {targetx 0 :as target}]
  (let [v (Vector2f. 0 -1)
        originv (vector2f origin)
        targetv (vector2f target)
        differencev (.normalise (.sub ^Vector2f targetv ^Vector2f originv))
        angle (int (Math/toDegrees (Math/acos (.dot v differencev))))]
     ; 0-180� ok -> target rechts von origin ansonsten adjust
    (if (< targetx originx)
      (- 360 angle)
      angle)))

(defn degrees? [n] (and (>= n 0) (<= n 360)))

(defn smallest-distance [a b]
	{:pre [(degrees? a) (degrees? b)]
	 :post [(degrees? %)]}
  (let [dist (Math/abs (float (- a b)))]
    (if (> dist 180) (- 360 dist) dist)))

(defn rotate-angle-to-angle [current-angle new-angle rotationspeed delta]
  (let [adjustment (* delta rotationspeed)]
    (if (>=
          adjustment
          (smallest-distance current-angle new-angle))
      new-angle
      (let [change-fn (add-or-subtract-angle current-angle new-angle)
            adjusted-angle (change-fn current-angle adjustment)]
        (cond
          (< adjusted-angle 0) (+ 360 adjusted-angle)
          (> adjusted-angle 360) (- adjusted-angle 360)
          :else adjusted-angle)))))

(defn degree-add [degree by] (rem (+ degree by) 360))

(defn degree-minus [degree by] (rem (+ 360 (- degree by)) 360))

(defn get-vector-to-mouse-coords
  ([]
    (get-vector-to-mouse-coords (get-mouse-pos)))
  ([posi]
    (direction-vector [half-screen-w half-screen-h] posi)))

