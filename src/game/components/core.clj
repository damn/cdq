(ns game.components.core
  (:require
    [utils.core :refer [get-unique-number make-fn when-apply]]
    [utils.coll :refer [distinct-seq? filter-map]]
    [engine.core :refer [update]]
    [game.session :as session]
    [game.components.active :refer [add-blocks remove-blocks]]))

(def id-entity-map (atom {}))

;; Component
; Component = map with :type. Special keys:
; :depends [:a :b :c] -> entity checks at creation if those components exist.
; :init, :destruct -> (fn [entity]) at creation / removal.

;; Entity

(defn- dependency-ok? [component types]
  (if-let [dependencies (:depends component)]
    (every? #(some #{%} types) dependencies)
    true))

(defn- dependencies-ok? [components]
  (let [types (map :type components)]
    (every? #(dependency-ok? % types) components)))

(defn add-component [entity component]
  {:pre [component (:type component)]}
  (let [ctype (:type component)
        types (map :type (vals @entity))]
    (assert (not-any? #{ctype} types))
    (assert (dependency-ok? component types))
    (swap! entity assoc-in [ctype] component)
    (when-apply (:init component) entity)
    entity))

(defn- init! [entity]
  (swap! id-entity-map assoc (:id (meta entity)) entity)
  (doseq [c (vals @entity)]
    (when-let [init (:init c)]
      (init entity)))
  entity)

(defn create-entity-no-init [& components]
  (let [components (remove nil? components)]
    (assert (seq components))
    (assert (every? :type components))
    (assert (distinct-seq? (map :type components)))
    (assert (dependencies-ok? components))
    (atom (zipmap (map :type components) components)
          :meta {:id (get-unique-number)})))

(defn create-entity [& components]
  (init!
    (apply create-entity-no-init components)))

;; Removelist

(def ^:private removelist (atom nil))

(defn- munge-id [entity]
  (if (number? entity) entity (:id (meta entity))))

(defn add-to-removelist [entity]  ; arglist entity-or-id
  (swap! removelist conj (munge-id entity)))

(defn destruct-entity
  "do not call this while update-components is active - use add-to-removelist instead.
  because calling this while update-components is running could lead to NullPointerE"
  [entity]
  (when (and entity (get @id-entity-map (:id (meta entity))))
    (swap! id-entity-map dissoc (:id (meta entity)))
    (dorun (map #(when-apply (:destruct %) entity) (vals @entity)))))

(defn update-removelist []
  (dorun (map #(destruct-entity (get @id-entity-map %)) @removelist))
  (reset! removelist #{}))

;; Get-position here becaused is used a lot.

(defn get-position [entity]
  (:value (:position @entity)))

(declare player-body)

(defn is-player? [entity]
  (= (:id (meta entity)) (:id (meta player-body))))

(defmacro active
  "Use: (active updatefn) or (active [delta component] fbody)
  Should be independent of update-order and changes will probably take effect in next frame
  because of snapshot order."
  [& args]
  `(let [f# ~(make-fn args)]
     (assert (fn? f#))
     {:updatefn f#}))

(defn- get-active-component-types [mapentity]
  (map :type (filter :updatefn (vals mapentity))))

(defn block-active-components [entity]
  (add-blocks entity (get-active-component-types entity)))

(defn unblock-active-components [entity]
  (remove-blocks entity (get-active-component-types entity)))

(defn reset-component-state-after-blocked [entity]
  (doseq [{:keys [block-effect-reset-state] :as c} (vals @entity)
          :when block-effect-reset-state]
    (block-effect-reset-state entity c)))

;;

(comment
  (let [a (create-entity {:type :a :updatefn 1}
                         {:type :b :updatefn 1})]
    (println @a)
    (unblock-active-components
      (block-active-components @a))))

;;

(defn update-counter! [entity delta c & [counterkey & _]]
  (let [k (or counterkey :counter)
        counter (update (k c) delta)]
    (swap! entity assoc-in [(:type c) k] counter)
    (:stopped? counter)))

;;

(defn save-single-entity-session [entity]
  (when-let [{:keys [constructor args]} (:session @entity)]
    {:constructor constructor
     :args args
     :components (for [{:keys [type serialize] :as component} (vals @entity)
                       :when serialize]
                   [type (select-keys component serialize)])}))

(def entities-session
  (reify session/Session
    (save-session [_]
      (->> id-entity-map
           deref
           vals
           (keep save-single-entity-session)))

    (load-session [_ entity-saves]
      (doseq [{:keys [constructor args components]} entity-saves
              :let [f (resolve (symbol constructor))
                    entity (apply f args)]]
        (doseq [[ctype m] components]
          (swap! entity update-in [ctype] merge m))
        (init! entity)))

    (new-session-data [_])))

;; -> all entities are loaded in the first-map currently and this will not work with rand generated maps
;; because the random generation would also be done with the same random seed so the same map is generated

;; order of initialisation ok? -> listeners befire entitiy add?
;;
;; also be careful with randomisation -> create-item-body with argument string creates a random image for the cyber implants for example!
;; properties INITIAL & COMPONENT SESSION SHOULD 100% re-create same values.
;; -> for entities that you want to save write down documentation for example input completely defines the state no randomisation (same item instance same img etc)

(def clear-entities-session (reify
                              session/Session
                              (load-session [_ _]
                                (reset! removelist #{})
                                (swap! id-entity-map filter-map
                                       #(:ingame-loop-entity @%)))
                              (save-session [_])
                              (new-session-data [_])))
