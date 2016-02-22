(ns game.components.core
  (:require [utils.core :as utils]
            [engine.core :refer [update]]
            [game.session :as session]
            [clojure.tools.macro :refer (name-with-attributes)])
  (:use utils.core
        game.components.active))

(def ^:private id-entity-map (atom {}))

(defn get-components [entity] (vals @entity))

(definline get-component [entity ctype]
  (list ctype (list 'deref entity)))

(defn get-id [entity] (:id (meta entity)))

(defn get-entity [id]
  (get @id-entity-map id))

(defn exists? [entity] (get-entity (get-id entity)))

;; Component

; TODO :type; :destruct, :init and :depends are METADATA and belong there -> information hiding.
; (create-comp :type :image :depends :position :init fn :destruct fn {:a 123} {:b 345})
(defn create-comp
  "All components should be created with this function.
  Special keys:
  :depends [:a :b :c] -> entity checks at creation if :a :b and :c components exist.
  :init, :destruct -> function with (fn [entity]) called at creation, removal of entity.
  Uses safe-merge - asserts that no keys in props is overridden."
  [ctype & maps]
  {:pre [(keyword? ctype)
         (every? map? maps)]}
  (apply safe-merge {:type ctype} maps))

(defmacro defcomponent
  "the first element of body may be {:pre :post}; the rest should be maps merged into the component-map.
  creates a factory function with name: ctype-component.
  Any number of arguments in arg-vector can be keywords, and for every keyword a map is conjoined to the body with those keywords as keys."
  [ctype arg-vector & body]
  (let [keywords-map (keywords-to-hash-map (filter keyword? arg-vector))
        ; transform keywords to symbols for a normal argument vector
        ; note some forms in the arg vector may be neither symbols nor keywords for example map destructuring so
        ; we have to check if keyword?
        arg-vector (mapv #(if (keyword? %) (symbol (name %)) %) arg-vector)
        first-element (first body)
        body (if (is-condition-map? first-element)
               `(~first-element (create-comp ~(keyword ctype) ~@(conj (rest body) keywords-map)))
               `((create-comp ~(keyword ctype) ~@(conj body keywords-map))))]
    `(defn ~(symbol (str (name ctype) "-component")) ~arg-vector
       ~@body)))

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
        types (map :type (get-components entity))]
    (assert (not-any? #{ctype} types))
    (assert (dependency-ok? component types))
    (assoc-in! entity [ctype] component)
    (when-apply (:init component) entity)
    entity))

(defn- init! [entity]
  (swap! id-entity-map assoc (get-id entity) entity)
  (doseq [c (get-components entity)]
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

(defn- dont-insert [form]
  (remove nil? [form]))

(defmacro defentity
  "Namesym can be followed by docstring and metadata map.
  Metadata can contain :save-session, if the entity should have a :session component."
  [namesym & more]
  (let [[namesym [argvec & more]] (name-with-attributes namesym more)
        [condition-map components] (condition-map-and-rest more)
        defsym (if (some keyword? argvec) `defnks 'defn)
        defform (fn [nm create-fn]
                  `(~defsym ~nm ~argvec
                     ~@(dont-insert condition-map)
                     (~create-fn
                       ~@(dont-insert
                           (when (-> namesym meta :save-session)
                             `(create-comp :session
                                           {:constructor ~(str *ns* "/" namesym "*")
                                            :args ~(if (= defsym 'defn)
                                                     argvec
                                                     `(apply concat ~'argsmap))})))
                       ~@components)))]
    (assert (every? (if (= defsym 'defn) symbol? keyword?) argvec))
    (list 'do
          (defform namesym                   'create-entity)
          (defform (symbol (str namesym \*)) 'create-entity-no-init))))

;; Removelist

(def ^:private removelist (atom nil))

(defn- munge-id [entity]
  (if (number? entity) entity (get-id entity)))

(defn add-to-removelist [entity]  ; arglist entity-or-id
  (swap! removelist conj (munge-id entity)))

(defn destruct-entity
  "do not call this while update-components is active - use add-to-removelist instead.
  because calling this while update-components is running could lead to NullPointerE"
  [entity]
  (when (and entity (exists? entity))
    (swap! id-entity-map dissoc (get-id entity))
    (runmap #(when-apply (:destruct %) entity) (get-components entity))))

(defn update-removelist []
  (runmap #(destruct-entity (get-entity %)) @removelist)
  (reset! removelist #{}))

;; Get-position here becaused is used a lot.

(defn get-position [entity]
  (:value (get-component entity :position)))

(defmacro defbody-key-getter [& ks]
  (let [defs (for [k# ks]
               `(defn ~(symbol (str "get-" (name k#))) [body#]
                  (~k# (get-component body# :body))))]
    `(do ~@defs)))

; circular dependencies body<->render
(defbody-key-getter
  :half-pxw
  :half-pxh
  :half-width
  :half-height
  :side
  :cached-touched-cells
  :occupied-cell)

; circular dependency body<->cell-grid
(defn is-solid? [body] (:solid (get-component body :body)))

; circular dependencies movement<->body
(defn get-movement-type [entity] (:movement-type (get-component entity :movement)))

(declare player-body)

(defn is-player? [entity]
  (= (get-id entity) (get-id player-body)))

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
  (doseq [{:keys [block-effect-reset-state] :as c} (get-components entity)
          :when block-effect-reset-state]
    (block-effect-reset-state entity c)))

;;

(comment
  (let [a (create-entity (create-comp :a {:updatefn 1})
                         (create-comp :b {:updatefn 1}))]
    (println @a)
    (unblock-active-components
      (block-active-components @a))))

;;

(defn update-counter! [entity delta c & [counterkey & _]]
  (let [k (or counterkey :counter)
        counter (update (k c) delta)]
    (assoc-in! entity [(:type c) k] counter)
    (:stopped? counter)))

;;

(defn save-single-entity-session [entity]
  (when-let [{:keys [constructor args]} (get-component entity :session)]
    {:constructor constructor
     :args args
     :components (for [{:keys [type serialize] :as component} (get-components entity)
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
          (update-in! entity [ctype] merge m))
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
                                       #(get-component % :ingame-loop-entity)))
                              (save-session [_])
                              (new-session-data [_])))
