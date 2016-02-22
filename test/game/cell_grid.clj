(ns game.cell-grid
  (:use
    (clojure test)
    [game.test.utils :only (with-private-fns)]
    (game.maps [cell-grid :only (create-cell add-body remove-body)])
    (game.components [core :only (create-comp create-entity get-id)])))


(let [mycell (create-cell [3 4] #{})
      myentity (create-entity
                 (create-comp :a))] ; TODO because entity without comp no id

  (with-private-fns [game.maps.cell-grid [in-cell?]]
    (deftest test-cell-body-ids

      (is
        (= nil (in-cell? mycell myentity)))

      (add-body mycell myentity)
      (is
        (= (get-id myentity) (in-cell? mycell myentity)))
      (is
        (not= :bla (in-cell? mycell myentity)))

      (remove-body mycell myentity)
      (is (thrown? AssertionError
            (remove-body mycell myentity)))

      (add-body mycell myentity)
      (is (thrown? AssertionError
            (add-body mycell myentity)))

      (is
        (= (get-id myentity) (in-cell? mycell myentity))))))
