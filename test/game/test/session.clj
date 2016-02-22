(ns game.test.session
  (:require [clojure.test :refer :all]
            [game.test.helper :refer :all]
            [game.components.render :refer :all]
            [engine.render :as color]
            [game.maps.contentfields :refer [get-all-entities-of-current-map]]
            [game.components.core :refer [get-component get-position]]
            [game.components.position :refer [swap-position!]]
            game.player.session-data))

(use-fixtures :once start-game-once-fixture)

(defn get-constructor
  "A way to find entities of a certain 'type'."
  [entity]
  (:constructor (get-component entity :session)))

(defn get-all-entities-with-constructor [constructor]
  (filter #(= constructor (get-constructor %))
          (get-all-entities-of-current-map)))

(deftest-ingame test-position-and-delete-after-duration-with-circle-render-effect-entity
  (let [old-posi [30 15]
        new-posi [35 13]

        entity (create-circle-render-effect old-posi 1 color/red 3500)
        get-entity #(-> entity get-constructor get-all-entities-with-constructor first)

        get-posi #(get-position (get-entity))

        get-count #(-> (get-entity) (get-component :delete-after-duration) :counter :cnt)
        new-count 33]
    (is (= old-posi (get-posi)))
    (swap-position! entity new-posi)
    (is (= new-posi (get-posi)))

    (is (zero? (get-count)))
    (game.components.update/update-component new-count (get-component entity :delete-after-duration) entity)
    (is (= new-count (get-count)))

    (game.player.session-data/save-game)
    (game.player.session-data/init true)

    (is (= new-count (get-count)))
    (is (= new-posi (get-posi)))))
