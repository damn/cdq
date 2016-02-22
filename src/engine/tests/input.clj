(ns engine.tests.input
  (:use (engine core input render)))

(def lastpressed (atom {}))

(defmacro test-them [& forms]
  (let [tests (for [form forms]
                `(str "\n" ~(str form) " " ~form))]
    `(str ~@tests)))

(defn render [container g]
  (render-readable-text g 10 10
    "Input-Test\n\n"
    (test-them
      (get-mouse-pos)
      (is-leftbutton-down?)
      (is-rightbutton-down?)
      (is-key-down? :A)
      (is-key-down? :C)
      @lastpressed)
    "\n\n\n\nHold 'C' while left/rightmouse-pressing to consume mouse-presses until you let go."
    (test-them
      (is-leftm-consumed?)
      (is-rightm-consumed?))))

(defn update-test []
  (when (is-key-pressed? :A)
    (swap! lastpressed assoc :A (System/currentTimeMillis)))
  (when (is-leftm-pressed?) (swap! lastpressed assoc :leftmouse (System/currentTimeMillis)))
  (when (is-rightm-pressed?) (swap! lastpressed assoc :rightmouse (System/currentTimeMillis)))
  (when (is-key-down? :C)
    (when (try-consume-leftm-pressed)
      (println "try-consume-leftm-pressed returns true!, should return nil when called another time this frame: " (try-consume-leftm-pressed)))
    (try-consume-rightm-pressed)))

(defn start-test []
  (start-slick-basicgame
    :title "Input Test"
    :update (fn [container delta]
              (update-mousebutton-state)
              (update-test))
    :render (fn [container g]
              (render container g))))

;(start-test)
