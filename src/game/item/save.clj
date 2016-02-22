(ns game.item.save
  (:require [game.session :as session])
  (:use utils.core
        [engine.render :only (rgbcolor)]
        (game.item cells instance boni)
        data.grid2d))

; maybe have this information in the items
(def- item-save-keys [:name :sprite-idx :count :lvl :base-armor :base-dmg
                      ; need to merge color manually.
                      :color])

(defmethod session/write-to-disk :item [item]
  (merge
    (select-keys item item-save-keys)
    (when-let [boni (:equip-boni item)]
      {:equip-boni (create-equip-boni-save boni)})))

(defmethod session/load-from-disk :item [saved-item]
  (merge
    (create-item-instance (:name saved-item)
                          (select-keys saved-item item-save-keys))
    (when-let [boni (:equip-boni saved-item)]
      {:equip-boni (load-equip-boni-save boni)})
    (select-keys saved-item [:color])))

(def session (reify session/Session
               (load-session [_ data]
                 (empty-all-item-grids)
                 (doseq [{:keys [posi grid-type item]} (:items data)]
                   (add-item-to-cell
                     item
                     (get (grid-type item-grids) posi)))
                 (when-let [item (:item-in-hand data)]
                   (set-item-in-hand item)))
               (save-session [_]
                 (let [item-cells-with-item (filter get-item
                                                    (mapcat cells (vals item-grids)))]
                   {:items (doall ; needed?
                             (map
                               (fn [cell]
                                 (merge
                                   (select-keys cell [:posi :grid-type])
                                   {:item (get-item cell)}))
                               item-cells-with-item))
                    :item-in-hand @item-in-hand}))
               (new-session-data [_]
                 {:items
                  [{:posi [0 0]
                    :grid-type :belt
                    :item (create-item-instance "Heal-Potion" {:count 4})}
                   {:posi [0 0]
                    :grid-type :hands
                    :item (create-item-instance "Sword")}
                   {:posi [0 0]
                    :grid-type :inventory
                    :item (create-item-instance "Restoration")}]})))
