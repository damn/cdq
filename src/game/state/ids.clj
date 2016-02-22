(ns game.state.ids
  (:use [utils.core :only (get-unique-number)]))

; primary keys because of cyclic dependencies between options menu and ingame-state
(def ingame  (get-unique-number))
(def options (get-unique-number))
(def minimap (get-unique-number))
