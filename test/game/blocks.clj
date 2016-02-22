(ns game.blocks
  (:use
    [game.test.utils :only (with-private-fns)]
    clojure.test
    game.components.active))

(deftest test-not-blocked
  (is (= (not-blocked? {:abc 4}) true))
  (is (= (not-blocked? {:blocks 0}) true))
  (is (= (not-blocked? {:blocks 3}) false))
  (is (= (not-blocked? {:blocks 100}) false))
  (is (thrown? AssertionError (not-blocked? {:blocks -1})))
  (is (thrown? ClassCastException (not-blocked? {:blocks "hi"}))))

(with-private-fns [game.components.active [switch-blocking]]
  (deftest add-and-remove
    (is (= (add-block {:test {}} :test)
           {:test {:blocks 1}}))
    (is (= (add-block {:test {:blocks 0}} :test)
           {:test {:blocks 1}}))
    (is (= (add-block {:test {:blocks 0}} nil)
           {:test {:blocks 0}}))
    (is (= (remove-block {:test {:blocks 1}} :test)
           {:test {:blocks 0}}))
    (is (= (remove-block {:test {:blocks 1}} nil)
           {:test {:blocks 1}}))
    (is
      (=
        (let [component {:type :test,:blocked-comptype :foo,:state-blocks {}}]
          (switch-blocking
            {:test component,:foo {:type :foo :blocks 1}}
            component
            :gee))
        {:test {:type :test
                :blocked-comptype nil
                :state-blocks {}}
         :foo {:type :foo
               :blocks 0}}))
    (is
      (=
        (let [component {:type :test,:blocked-comptype nil, :state-blocks {:gee :foo}}]
          (switch-blocking
            {:test component,:foo {:type :foo,:blocks 1}}
            component
            :gee))
        {:test {:type :test
                :blocked-comptype :foo
                :state-blocks {:gee :foo}}
         :foo {:type :foo
               :blocks 2}}))))

(deftest test-switch-state
  (is
    (=
      (switch-state
        {:a {:type :a
             :state :sit
             :blocked-comptype nil
             :state-blocks {}}}
        :a
        :walk)
      {:a {:type :a
           :state :walk
           :blocked-comptype nil
           :state-blocks {}}})))


