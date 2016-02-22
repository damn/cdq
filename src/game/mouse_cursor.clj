(ns game.mouse-cursor
  (:use
    [engine.core :only (set-mouse-cursor initialize)]
    [utils.core :only (deflazygetter)])
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

