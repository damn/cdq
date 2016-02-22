(ns game.item.update
  (:use
    utils.core
    (engine core input)
    [game.ingame-gui :only (mouse-inside-some-gui-component?)]
    data.grid2d
    (game.components core)
    (game.item cells instance)))

(defn- update-drag-and-drop []
  (let [mouseover-cell (get-mouseover-item-cell)]
    (cond

      ; TRY PICK FROM CELL
      (and
        (mouse-over-an-item-cell?)
        (not (is-item-in-hand?))
        (get-item mouseover-cell)
        (item-not-in-use? mouseover-cell))
      (do
        (play-sound "bfxr_takeit.wav")
        (set-item-in-hand (get-item mouseover-cell))
        (remove-item-from-cell mouseover-cell))

      ; TRY PUT/INCREMENT/SWAP IN CELL
      (and
        (mouse-over-an-item-cell?)
        (is-item-in-hand?))
      (cond
        (cell-empty-and-allows-item? mouseover-cell @item-in-hand) ; PUT ITEM IN
        (do
          (play-sound "bfxr_itemput.wav")
          (add-item-to-cell @item-in-hand mouseover-cell)
          (empty-item-in-hand))

        (inc-count-of-item? (get-item mouseover-cell) @item-in-hand) ; INCREMENT ITEM
        (do
          (play-sound "bfxr_itemput.wav")
          (swap! (:item mouseover-cell) update-in [:count] + (:count @item-in-hand))
          (empty-item-in-hand))

        (cell-filled-and-allows-item? mouseover-cell @item-in-hand) ; SWAP ITEMS
        (let [cell-item (get-item mouseover-cell)]
          (remove-item-from-cell mouseover-cell)
          (add-item-to-cell @item-in-hand mouseover-cell)
          (set-item-in-hand cell-item)
          (play-sound "bfxr_itemput.wav")))

      ; PUT ON GROUND
      (and
        (not (mouse-over-an-item-cell?))
        (not (mouse-inside-some-gui-component?))
        (is-item-in-hand?))
      (do
        (play-sound "bfxr_itemputground.wav")
        (put-item-on-ground)))))

(defn- try-usable-item-effect [{:keys [effect use-sound]:as item} cell]
  (if (effect)
    (do
      (remove-one-item cell)
      (play-sound use-sound))
    (play-sound "bfxr_denied.wav")))

(defn- update-belt-hotkeys []
  (let [belt-grid (:belt item-grids)]
    (doseq [[hotkey cellposition] hks-cells]
      (when (is-key-pressed? hotkey)
        (let [cell (get belt-grid cellposition)
              item (get-item cell)]
          (when item
            (try-usable-item-effect item cell)))))))

(defn update-items []
  (update-belt-hotkeys)
  (when
    (and
      (or (mouse-over-an-item-cell?) (is-item-in-hand?))
      (try-consume-leftm-pressed))
    (update-drag-and-drop))
  (let [mouseover-cell (get-mouseover-item-cell)]
    (when
      (and mouseover-cell
           (and (not (is-rightm-consumed?)) (try-consume-rightm-pressed))
           (get-item mouseover-cell)
           (= (:type (get-item mouseover-cell)) :usable)
           (item-not-in-use? mouseover-cell))
      (try-usable-item-effect (get-item mouseover-cell) mouseover-cell))))



