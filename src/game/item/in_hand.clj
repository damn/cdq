(ns game.item.in-hand
  (:require
    [engine.core :refer [set-mouse-cursor]]
    [engine.render.image :refer [get-scaled-copy]]
    [cdq.core :refer [screen-scale]]
    [utils.core :refer [deflazygetter]])
  (:import org.newdawn.slick.opengl.CursorLoader))

;;; mouse-cursor

(let [hotspotx 0
      hotspoty 0]

  (deflazygetter get-default-mouse-cursor
    (-> (CursorLoader/get) (.getCursor "cursor.png" hotspotx hotspoty)))

  (defn reset-default-mouse-cursor []
    (set-mouse-cursor (get-default-mouse-cursor) hotspotx hotspoty)))

(alter-var-root #'engine.core/fns conj
  (fn []
    (reset-default-mouse-cursor)))

; (.setDefaultMouseCursor app-game-container) -> der normale

;;; in-hand

(def item-in-hand (atom nil))

(defn is-item-in-hand? [] @item-in-hand)

(defn set-item-in-hand [item]
  (reset! item-in-hand item)
  (set-mouse-cursor (get-scaled-copy (:image item) screen-scale) (* 8 screen-scale) (* 8 screen-scale)))

(defn empty-item-in-hand []
  (reset! item-in-hand nil)
  (reset-default-mouse-cursor))
