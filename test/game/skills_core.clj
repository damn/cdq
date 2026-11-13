(ns game.skills-core
  (:require
    [game.test.utils :refer [with-private-fns]]
    [cdq.core :refer [mapvals]]
    [cdq.core :refer [make-counter]]
    [clojure.test :refer [deftest is]]))

(with-private-fns [cdq.core [skill-update-cooldown
                                                skill-update-cooldowns]]
  (deftest test-update-cooldown
    (is
      (->
        {:cooldown-counter (make-counter 0)
         :state :attacking}
        (skill-update-cooldown 3)
        :state
        (= :attacking)))
    (is
      (->
        {:cooldown-counter (make-counter 0)
         :state :cooldown}
        (skill-update-cooldown 3)
        :state
        (= :ready)))
    (is
      (->
        {:cooldown-counter (make-counter 4)
         :state :cooldown}
        (skill-update-cooldown 3)
        :cooldown-counter
        :cnt
        (= 3)))
    (is
      (=
        {0 :cooldown, 1 :ready, 2 :ready}
        (mapvals
          (:skills
            (skill-update-cooldowns
              {:skills
               {0 {:cooldown-counter (make-counter 2)
                   :state :cooldown}
                1 {:cooldown-counter (make-counter 1)
                   :state :cooldown}
                2 {:cooldown-counter (make-counter 0)
                   :state :cooldown}}}
              1))
          :state)))))

(with-private-fns [cdq.core [skill-try-set-active-cooldown]]
  (deftest test-set-active-skill-cooldown
    (is
      (let [skillmanager {:active-type :melee :skills {}}]
        (=
          (skill-try-set-active-cooldown skillmanager)
          skillmanager)))
    (is
      (let [skillmanager {:active-type :melee, :skills {:melee {:state :ready}}}]
        (=
          (skill-try-set-active-cooldown skillmanager)
          {:active-type :melee, :skills {:melee {:state :cooldown}}})))))
