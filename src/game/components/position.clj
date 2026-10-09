(ns game.components.position
  (:require
    [utils.core :refer [int-posi when-apply]]
    [game.maps.contentfields :refer [put-entity-in-correct-content-field remove-entity-from-content-field]]))

(defn position-component [p]
  {:type :position
   :value p
   :serialize [:value]
   :init put-entity-in-correct-content-field
   :destruct remove-entity-from-content-field
   :posi-changed put-entity-in-correct-content-field})

(defn get-tile [entity]
  (int-posi (:value (:position @entity))))

(defn swap-position! [entity posi & {filter-body :filter-body}]
  (swap! entity assoc-in [:position :value] posi)
  (doseq [c (vals @entity)
          :when (not
                  (and
                    filter-body
                    (= :body (:type c))))]
    (when-apply (:posi-changed c) entity)))

