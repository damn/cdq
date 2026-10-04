(ns game.tools.tiles-walkable-property-xml-printer
  (:require [utils.prxml :refer [xml-str]]))

(defn tiletoxml [id walkable]
  {:pre [(#{:air :ground :unwalkable} walkable)]}
  (when-not (= :unwalkable walkable)
    (xml-str [:tile {:id id}
              [:properties [:property {:name "walkable" :value (name walkable)}]]])))

(println (apply str (interpose "\n" (map #(tiletoxml % :ground) (range 37)))))

; stelle image dar
; mit einem bestimmten grid dr�bergezeichnet
; selektiere 1 oder mehrere tiles und erm�gliche deselektieren
; w�hle ground/air/nix als icons und visualisiere auch �ber jedem
; speichern m�glich als *.tsx
