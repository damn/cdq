(ns game.skills-core
  (:use
    [game.test.utils :only (with-private-fns)]
    (clojure test)
    [utils.core :only (mapvals)]
    (game.components.skills core)
    [engine.core :only (make-counter)]))

(with-private-fns [game.components.skills.core [update-cooldown
                                                update-cooldowns]]
  (deftest test-update-cooldown
    (is
      (->
        {:cooldown-counter (make-counter 0)
         :state :attacking}
        (update-cooldown 3)
        :state
        (= :attacking)))
    (is
      (->
        {:cooldown-counter (make-counter 0)
         :state :cooldown}
        (update-cooldown 3)
        :state
        (= :ready)))
    (is
      (->
        {:cooldown-counter (make-counter 4)
         :state :cooldown}
        (update-cooldown 3)
        :cooldown-counter
        :cnt
        (= 3)))
    (is
      (=
        {0 :cooldown, 1 :ready, 2 :ready}
        (mapvals
          (:skills
            (update-cooldowns
              {:skills
               {0 {:cooldown-counter (make-counter 2)
                   :state :cooldown}
                1 {:cooldown-counter (make-counter 1)
                   :state :cooldown}
                2 {:cooldown-counter (make-counter 0)
                   :state :cooldown}}}
              1))
          :state)))))

(with-private-fns [game.components.skills.core [try-set-active-skill-cooldown]]
  (deftest test-set-active-skill-cooldown
    (is
      (let [skillmanager {:active-type :melee :skills {}}]
        (=
          (try-set-active-skill-cooldown skillmanager)
          skillmanager)))
    (is
      (let [skillmanager {:active-type :melee, :skills {:melee {:state :ready}}}]
        (=
          (try-set-active-skill-cooldown skillmanager)
          {:active-type :melee, :skills {:melee {:state :cooldown}}})))))
