(ns engine.input
  "In order to use include engine.input/update-mousebutton-state and engine.core/init-all in your game."
  (:use [engine.core :only (app-game-container defpreload)])
  (:import org.newdawn.slick.Input))

(defpreload ^:private input
  (.getInput app-game-container))

(defn clear-key-pressed-record   [] (.clearKeyPressedRecord   ^Input input))
(defn clear-mouse-pressed-record [] (.clearMousePressedRecord ^Input input))

(defn get-mouse-pos []
  [(.getMouseX ^Input input)
   (.getMouseY ^Input input)])

(defn- to-mouse-key [k]
  (case k
    :left  Input/MOUSE_LEFT_BUTTON
    :right Input/MOUSE_RIGHT_BUTTON))

(defn- is-mouse-button-down? [k] (.isMouseButtonDown ^Input input (to-mouse-key k)))
(defn- is-mouse-pressed?     [k] (.isMousePressed    ^Input input (to-mouse-key k)))

(def is-leftbutton-down?  (partial is-mouse-button-down? :left))
(def is-rightbutton-down? (partial is-mouse-button-down? :right))

(def ^:private to-keyboard-key
  (memoize (fn [k]
             (eval (symbol (str "org.newdawn.slick.Input/KEY_" (name k)))))))

(defn is-key-pressed?
  "Since last call to this. So do not call this twice in one frame else it will return false."
  [k]
  (.isKeyPressed ^Input input (to-keyboard-key k)))

(defn is-key-down? [k]
  (.isKeyDown ^Input input (to-keyboard-key k)))

; when using is-...-pressed? it is probably useful also to check if is-...-consumed?
; for example a bug occured:
; waypoints menu opens with try-consume-..-pressed while is-...-pressed? closed it again in the same frame
; TODO maybe is-...-pressed? always checks if not consumed yet (so it is really 'consumed')

(def mousebutton {:pressed  false
                  :consumed false})

(def ^:private state (atom {:left  mousebutton
                            :right mousebutton}))

(defn- is-pressed? [button] (-> @state button :pressed))

(defn is-leftm-pressed?  [] (is-pressed? :left))
(defn is-rightm-pressed? [] (is-pressed? :right))

(defn- is-consumed? [button] (-> @state button :consumed))

(defn is-leftm-consumed?  [] (is-consumed? :left))
(defn is-rightm-consumed? [] (is-consumed? :right))

(defn- check-if-pressed [state button]
  (assoc-in state [button :pressed] (is-mouse-pressed? button)))

(defn- resolve-consumed [state button]
  (if (and (-> state button :consumed)
           (not (is-mouse-button-down? button)))
    (assoc-in state [button :consumed] false)
    state))

(defn update-mousebutton-state []
  (swap! state #(-> %
                  (check-if-pressed :left)
                  (resolve-consumed :left)
                  (check-if-pressed :right)
                  (resolve-consumed :right))))

(defn- try-consume-pressed [button]
  (when (and (is-pressed? button)
             (not (is-consumed? button)))
    (swap! state assoc-in [button :consumed] true)))

(defn try-consume-leftm-pressed
  "If leftmouse was pressed this frame and not yet consumed, consumes it and returns true else returns nil.
   It is consumed as long as the leftmouse-button is down."
  []
  (try-consume-pressed :left))

(defn try-consume-rightm-pressed []
  (try-consume-pressed :right))
