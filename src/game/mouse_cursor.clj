(ns game.mouse-cursor
  (:require
    [engine.core :refer [initialize set-mouse-cursor]]
    [utils.core :refer [deflazygetter]])
  (:import org.newdawn.slick.opengl.CursorLoader))

(let [hotspotx 0
      hotspoty 0]

  (deflazygetter get-default-mouse-cursor
    (-> (CursorLoader/get) (.getCursor "cursor.png" hotspotx hotspoty)))

  (defn reset-default-mouse-cursor []
    (set-mouse-cursor (get-default-mouse-cursor) hotspotx hotspoty)))

(initialize
  (reset-default-mouse-cursor))

; (.setDefaultMouseCursor app-game-container) -> der normale

