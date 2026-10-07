(ns game.item.cells
  (:require
    [utils.core :refer [thread-through]]
    [data.grid2d :refer [cells]]
    [game.components.core :refer [player-body]]
    [game.components.skills.core :refer [get-active-skill is-attacking?]]
    [game.item.grids :refer [item-grids]]))

(defn get-equiped-hands-item []
  @(:item (get (:hands item-grids) [0 0]))) ; (-> item-grids :hands [0 0])

(defn cell-allows-item? [{allows :allows-type :as cell} item]
  (or
    (= :all allows)
    (= (:type item) allows)))

(defn cell-empty-and-allows-item? [cell item]
  (and
    (nil? @(:item cell))
    (cell-allows-item? cell item)))

(defn cell-filled-and-allows-item? [cell item]
  (and
    @(:item cell)
    (cell-allows-item? cell item)))

(defn inc-count-of-item? [item1 item2]
  (and
    item1
    (:count item1)
    (:count item2)
    (= (:name item1) (:name item2))))

(defn- check-apply-boni [item cell k]
  (let [components player-body]
    (when (:is-equipment cell)
      (swap! components thread-through (map k (:equip-boni item)))
      (when-let [equip (k item)]
        (swap! components equip)))))

(defn add-item-to-cell [item cell]
  {:pre [(cell-empty-and-allows-item? cell item)]}
  (reset! (:item cell) item)
  (check-apply-boni item cell :equip))

(defn remove-item-from-cell [cell]
  (let [item @(:item cell)] ; get the item bevor setting it nil so that :unequip works!
    (reset! (:item cell) nil)
    (check-apply-boni item cell :unequip)))

(defn empty-all-item-grids []
  (doseq [grid (vals item-grids)
          cell (cells grid)]
      (remove-item-from-cell cell)))

(defn remove-one-item [cell]
  (let [item @(:item cell)]
    (if-let [cnt (:count item)]
      (if (> cnt 1)
        (swap! (:item cell) update-in [:count] dec)
        (remove-item-from-cell cell))
      (remove-item-from-cell cell))))

(defn remove-one-item-from [grid-type item-name]
  {:pre [(some #{item-name}
           (map #(:name @(:item %))
             (cells (:belt item-grids))))]}
  (remove-one-item
    (first (filter
      #(= item-name (:name @(:item %)))
      (cells (:belt item-grids))))))

(defn item-not-in-use? [cell]
  (let [item @(:item cell)
        skillmanager (:skillmanager @player-body)
        active-skill (get-active-skill skillmanager)
        in-use (and
                 (:is-equipment cell) item (is-attacking? skillmanager)
                 (or
                   (and (:skill item) (= active-skill (:skill item)))
                   (and (:melee-weapon item) (= (get-equiped-hands-item) item) (:is-melee active-skill))))]
    (not in-use)))

(defn item-in-use? [cell]
  (not (item-not-in-use? cell)))

(defn get-inventory-cells-with-item-name [item-name grid-type]
  (filter
    #(and
       @(:item %)
       (= (:name @(:item %)) item-name))
    (cells (grid-type item-grids))))

(defn- try-put-item-in
  "returns true when the item was picked up"
  [picked-item grid-type]
  (let [item-cells (cells (grid-type item-grids))
        cell-with-same-item (first
                              (get-inventory-cells-with-item-name (:name picked-item) grid-type))
        picked-up (if
                    (and
                      cell-with-same-item
                      (:count @(:item cell-with-same-item)))
                    (swap! (:item cell-with-same-item) update-in [:count] + (:count picked-item))
                    (if-let [free-cell (first (filter #(nil? @(:item %)) item-cells))]
                      (do
                        (add-item-to-cell picked-item free-cell)
                        true)
                      false))]
    picked-up))

(defn try-pickup-item [item]
  (or
    (when-let [grid-type (:grid-type
                           (first (filter #(= (:allows-type %) (:type item))
                             (vals item-grids))))]
      (try-put-item-in item grid-type)) ; 1. try
    (try-put-item-in item :inventory))) ; 2. try
