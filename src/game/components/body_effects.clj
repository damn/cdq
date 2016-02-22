(ns game.components.body-effects
  (:use utils.core
        (game.components core
                         position
                         [sleeping :only (wake-up)]
                         [misc :only (delete-after-duration-component)])))

(defn get-sub-entities [entity]
  (when-let [subids (:ids (get-component entity :sub-entities))]
    (map get-entity subids)))

(defcomponent sub-entities []
  {:depends [:position]
   :ids #{}
   :destruct (fn [entity]
              (runmap destruct-entity (get-sub-entities entity)))
   :posi-changed (fn [entity]
                   (let [p (get-position entity)]
                     (runmap #(swap-position! % p) (get-sub-entities entity))))})

(defnks body-effect-entity
  "Effect-entities are deleted when target entity is deleted.
  They have a position component that is getting updated when the target position changes.
  They also call 'wake-up' on the target."
  [:target
   :opt :duration
   :opt-def :type :no-type :undofn (fn []) :dofn (fn [])
   & additional-components]
  (wake-up target)
  (apply create-entity
         (position-component (get-position target))
         (create-comp :body-effect
           {:target target
            :effect-type type
            :init (fn [this]
                    (dofn)
                    (when-not (get-component target :sub-entities)
                      (add-component target (sub-entities-component)))
                    (update-in! target [:sub-entities :ids] conj (get-id this)))
            ; first disj id before undofn is called!
            ; undofn may access get-effect-entities and this one is already not existing!
            :destruct (fn [this]
                        (update-in! target [:sub-entities :ids] disj (get-id this))
                        (undofn))})
         (when duration
           (delete-after-duration-component duration))
         additional-components))
; -> could move delete-after-duration/wake-up(as obligatory-key)/undo/do to implementation

; fix psi-charger set-durations @ undo -> remove this 'first disj' comment
; -> set-durations of psi-charger?? index 0,1,2 even if only 1 less?
; better: durations not set when one is undo but at creation ? 20/40/60?

(defmacro defeffectentity [namesym argvec & more]
  `(defn ~namesym ~argvec
     (body-effect-entity :type ~(keyword namesym)
                         ~@more)))

(defn get-certain-effect-entities [entity effect-type]
  (filter #(= (:effect-type (get-component % :body-effect)) effect-type)
          (get-sub-entities entity)))

(comment
  (map #(:effect-type (get-component % :body-effect))
       (get-sub-entities (get-entity 31))))

(comment
  (with-redefs [position-component (fn [p] {:type :position :p p})
                game.components.body/get-other-bodies-in-adjacent-cells (fn [entity] ())]

    (let [testcoll (atom [])
          entity (create-entity (create-comp :position)
                                (create-comp :body)
                                (game.components.sleeping/sleeping-component))]
      (println "sleep? " (#'game.components.sleeping/is-sleeping? entity))
      (body-effect-entity :target entity
                          :dofn #(swap! testcoll conj 1)
                          :undofn #(swap! testcoll conj 2))
      (destruct-entity entity)
      (println "sleep? " (#'game.components.sleeping/is-sleeping? entity))
      @testcoll)))
