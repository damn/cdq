(ns game.cell-grid
  (:require
    [game.test.utils :refer [with-private-fns]]
    [game.maps.cell-grid :refer [add-body create-cell remove-body]]
    [game.components.core :refer [create-entity]]
    [clojure.test :refer [deftest is]]))


(let [mycell (create-cell [3 4] #{})
      myentity (create-entity
                 {:type :a})] ; TODO because entity without comp no id

  (with-private-fns [game.maps.cell-grid [in-cell?]]
    (deftest test-cell-body-ids

      (is
        (= nil (in-cell? mycell myentity)))

      (add-body mycell myentity)
      (is
        (= (:id (meta myentity)) (in-cell? mycell myentity)))
      (is
        (not= :bla (in-cell? mycell myentity)))

      (remove-body mycell myentity)
      (is (thrown? AssertionError
            (remove-body mycell myentity)))

      (add-body mycell myentity)
      (is (thrown? AssertionError
            (add-body mycell myentity)))

      (is
        (= (:id (meta myentity)) (in-cell? mycell myentity))))))
