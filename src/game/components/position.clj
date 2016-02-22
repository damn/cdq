(ns game.components.position
  (:use utils.core
        game.components.core
        game.maps.contentfields))

(defcomponent :position [p]
  {:value p
   :serialize [:value]
   :init put-entity-in-correct-content-field
   :destruct remove-entity-from-content-field
   :posi-changed put-entity-in-correct-content-field})

(def get-tile (comp int-posi get-position))

(defn swap-position! [entity posi & {filter-body :filter-body}]
  (assoc-in! entity [:position :value] posi)
  (doseq [c (get-components entity)
          :when (not
                  (and
                    filter-body
                    (= :body (:type c))))]
    (when-apply (:posi-changed c) entity)))

