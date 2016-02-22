(ns game.test.components.render
  (:use clojure.test
        game.test.utils
        game.components.core
        game.components.position
        game.components.render
        engine.render))

(with-private-fns [game.components.render [update-animation]]
  (deftest test-animation-component-update
    (let [entity (create-entity
                   (position-component [0 0])
                   (animation-component (constantly :a)
                                        {:a (assoc (create-animation [:a1 :a2 :a3]) :type :a)
                                         :b (assoc (create-animation [:b1 :b2 :b3]) :type :b)}))
          update-a #(update-animation %
                                      (get-component entity :animation)
                                      entity)
          update-b #(update-animation %
                                      (assoc (get-component entity :animation) :control (constantly :b))
                                      entity)
          cnt #(:cnt (current-animation entity))]
      (update-a 20)
      (is (= 20 (cnt)))
      (is (= :a (:type (current-animation entity))))
      (update-a 18)
      (is (= 38 (cnt)))
      (is (= :a (:type (current-animation entity))))

      (update-b 15)
      (is (= 15 (cnt)))
      (is (= :b (:type (current-animation entity))))
      (update-b 8)
      (is (= 23 (cnt)))
      (is (= :b (:type (current-animation entity))))

      (update-a 3.5)
      (is (= 3.5 (cnt)))
      (is (= :a (:type (current-animation entity)))))))
