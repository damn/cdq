(ns game.tools.tiles-walkable-property-xml-printer
  (:use utils.prxml))

(defn tiletoxml [id walkable]
  {:pre [(#{:air :ground :unwalkable} walkable)]}
  (when-not (= :unwalkable walkable)
    (xml-str [:tile {:id id}
              [:properties [:property {:name "walkable" :value (name walkable)}]]])))

(println (apply str (interpose "\n" (map #(tiletoxml % :ground) (range 37)))))

; stelle image dar
; mit einem bestimmten grid drübergezeichnet
; selektiere 1 oder mehrere tiles und ermögliche deselektieren
; wähle ground/air/nix als icons und visualisiere auch über jedem
; speichern möglich als *.tsx
