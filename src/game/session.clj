(ns game.session)

(defprotocol Session ; State
  (load-session     [_ data]) ; set-state
  (save-session     [_])      ; current-value
  (new-session-data [_]))     ; initial-value

(deftype AtomSessionState [a initial]
  Session
  (load-session [_ data]
    (reset! a data))
  (save-session [_]
    @a)
  (new-session-data [_]
    initial))

(deftype NoSaveAtomSessionState [a initial]
  Session
  (load-session [_ _]
    (reset! a initial))
  (save-session [_])
  (new-session-data [_]))

(defn atom-session [a & {save :save-session :or {save true}}]
  {:pre [(instance? clojure.lang.Atom a)]}
  (if save
    (AtomSessionState.       a @a)
    (NoSaveAtomSessionState. a @a)))

;; Serialisation

(defn- pr-type [x]
  (or (get (meta x) :pr) (class x)))

(defmulti load-from-disk pr-type)
(defmulti write-to-disk  pr-type)

(defmethod load-from-disk :default [x] x)
(defmethod write-to-disk  :default [x] x)

(comment
  ; An alternative to load-from-disk and write-to-disk would be print-dup,
  ; it retains metadata and type info but is less readable than walk-term
  ; But anyhow without pretty print and with more and more entities, it isnt readable anyway.
  ; Another disadvantage that it only dispatches on class, so it doesnt work for item.
  (binding [*print-dup* true]
    (pr-str (Color. 1 1 1)))
  (defmethod print-dup Color [c out]
    (.write out
            (str "#="
                 `(Color. ~(.r c) ~(.g c) ~(.b c) ~(.a c))))))
