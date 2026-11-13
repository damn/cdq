(ns game.cell-grid
  (:require
    [game.test.utils :refer [with-private-fns]]
    [cdq.core :refer [cell-add-body create-cell cell-remove-body]]
    [cdq.core :refer [create-entity]]
    [clojure.test :refer [deftest is]]))


(let [mycell (create-cell [3 4] #{})
      myentity (create-entity
                 {:type :a})] ; TODO because entity without comp no id

  (with-private-fns [cdq.core [cell-contains-body?]]
    (deftest test-cell-body-ids

      (is
        (= nil (cell-contains-body? mycell myentity)))

      (cell-add-body mycell myentity)
      (is
        (= (:id (meta myentity)) (cell-contains-body? mycell myentity)))
      (is
        (not= :bla (cell-contains-body? mycell myentity)))

      (cell-remove-body mycell myentity)
      (is (thrown? AssertionError
            (cell-remove-body mycell myentity)))

      (cell-add-body mycell myentity)
      (is (thrown? AssertionError
            (cell-add-body mycell myentity)))

      (is
        (= (:id (meta myentity)) (cell-contains-body? mycell myentity))))))
