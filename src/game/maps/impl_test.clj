(ns game.maps.impl-test
  (:require game.maps.add))

(game.maps.add/deftilemap :empty-test-map
  :pretty-name "Empty Test Map"
  :file "empty_test_map.tmx"
  :spawn-monsters (fn [])
  :load-content (fn [])
  :rand-item-max-lvl 1)
