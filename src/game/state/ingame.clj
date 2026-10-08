(ns game.state.ingame
  (:import
    (org.newdawn.slick Image SpriteSheet)
    org.newdawn.slick.tiled.TiledMap)
  (:require
    [engine.input :as input :refer [get-mouse-pos is-key-pressed? is-rightm-consumed? try-consume-leftm-pressed try-consume-rightm-pressed]]
    [engine.render.color :refer [black red set-color white yellow]]
    [engine.render.image :refer [create-image draw-image get-dimensions get-sub-image]]
    [engine.render.assets :refer [get-sprite]]
    [engine.render.graphics :refer [draw-grid fill-rect render-readable-text reset-transform translate]]
    [engine.statebasedgame :as state :refer [defgamestate enter-state]]
    [game.debug-settings :as debug]
    [game.state.ids :as ids]
    [game.state.mainmenu :refer [mainmenu-gamestate]]
    [game.components.update :as cupd]
    [game.maps.data :refer [get-current-map-data iterating-map-dependent-comps]]
    [game.maps.mapchange :refer [check-change-map]]
    [game.player.session-data :refer [current-character-name]]
    [game.utils.lightning :refer [image-corners set-cached-brightness]]
    [game.utils.tilemap :refer [get-mouse-tile-pos mouse-int-tile-pos]]
    [utils.core :refer [int-posi sort-by-order]]
    [utils.numbers :refer [get-ratio readable-number variance-val-str]]
    [engine.core :refer [get-line-height initialize play-sound]]
    [game.settings :refer [debug-mode display-height-in-tiles display-width-in-tiles get-setting half-display-h-in-tiles half-display-w-in-tiles half-screen-h half-screen-w left-offset-in-tiles screen-height screen-width tile-height tile-width top-offset-in-tiles]]
    [game.screenshake :refer [translate-shake-after-render translate-shake-before-render update-shake]]
    [game.mouseoverbody :refer [get-mouseover-body]]
    [game.gui :refer [is-visible? make-frame]]
    [game.ingame-gui :refer [char-hotkey close-all-frames frame-screenborder-distance ingamestate-display mouse-inside-some-gui-component? options-hotkey some-visible-frame?]]
    [game.maps.contentfields :refer [get-entities-in-active-content-fields get-player-content-field-idx]]
    [game.maps.cell-grid :refer [cell-blocked? get-body-ids get-cell get-cell-grid get-map-h get-map-w]]
    [game.maps.camera :refer [get-camera-position]]
    [game.maps.tiledmaps :refer [get-layer-index]]
    [game.components.core :refer [get-position player-body update-removelist]]
    [game.components.body :refer [on-screen-and-in-sight?]]
    [game.components.render :refer [render-map-indep-order render-on-map-order rendering translate-position]]
    [game.components.destructible :refer [get-armor get-armor-reduce-info get-hp is-dead?]]
    [game.components.ingame-loop :refer [get-ingame-loop-entities ingame-loop-comp]]
    [game.components.movement.ai.potential-field :refer [calculate-mouseover-body-colors render-potential-field-following-mouseover-info render-potential-field-info]]
    [game.components.skills.core :refer [get-mana]]
    [game.components.skills.melee :refer [get-current-player-melee-dmg]]
    [game.item.cells :refer [add-item-to-cell cell-empty-and-allows-item? cell-filled-and-allows-item? inc-count-of-item? item-not-in-use? remove-item-from-cell remove-one-item]]
    [game.item.grids :refer [hks-cells inventory-height inventoryry item-grids]]
    [game.item.cells-ui :refer [get-mouseover-item-cell mouse-over-an-item-cell?]]
    [game.item.in-hand :refer [empty-item-in-hand is-item-in-hand? item-in-hand set-item-in-hand]]
    [game.item.instance :refer [put-item-on-ground]]
    [game.player.skill.selection-list :refer [close-skill-selection-lists some-skill-selection-list-visible?]]
    [game.player.skill.skillmanager :refer [get-selected-skill]]
    [game.player.core :refer [player-death try-revive-player]]
    [game.utils.raycast :refer [ray-blocked?]]))

;;; item update (was game.item.update)

(defn- update-drag-and-drop []
  (let [mouseover-cell (get-mouseover-item-cell)]
    (cond
      (and
        (mouse-over-an-item-cell?)
        (not (is-item-in-hand?))
        @(:item mouseover-cell)
        (item-not-in-use? mouseover-cell))
      (do
        (play-sound "bfxr_takeit.wav")
        (set-item-in-hand @(:item mouseover-cell))
        (remove-item-from-cell mouseover-cell))

      (and
        (mouse-over-an-item-cell?)
        (is-item-in-hand?))
      (cond
        (cell-empty-and-allows-item? mouseover-cell @item-in-hand)
        (do
          (play-sound "bfxr_itemput.wav")
          (add-item-to-cell @item-in-hand mouseover-cell)
          (empty-item-in-hand))

        (inc-count-of-item? @(:item mouseover-cell) @item-in-hand)
        (do
          (play-sound "bfxr_itemput.wav")
          (swap! (:item mouseover-cell) update-in [:count] + (:count @item-in-hand))
          (empty-item-in-hand))

        (cell-filled-and-allows-item? mouseover-cell @item-in-hand)
        (let [cell-item @(:item mouseover-cell)]
          (remove-item-from-cell mouseover-cell)
          (add-item-to-cell @item-in-hand mouseover-cell)
          (set-item-in-hand cell-item)
          (play-sound "bfxr_itemput.wav")))

      (and
        (not (mouse-over-an-item-cell?))
        (not (mouse-inside-some-gui-component?))
        (is-item-in-hand?))
      (do
        (play-sound "bfxr_itemputground.wav")
        (put-item-on-ground)))))

(defn- try-usable-item-effect [{:keys [effect use-sound] :as item} cell]
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
              item @(:item cell)]
          (when item
            (try-usable-item-effect item cell)))))))

(defn- update-items []
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
           @(:item mouseover-cell)
           (= (:type @(:item mouseover-cell)) :usable)
           (item-not-in-use? mouseover-cell))
      (try-usable-item-effect @(:item mouseover-cell) mouseover-cell))))

;;; update-ingame (was game.update-ingame)

(defn- update-game [delta]
  (when (input/is-key-pressed? options-hotkey)
    (cond
      (is-item-in-hand?) (put-item-on-ground)
      (some-visible-frame?) (close-all-frames)
      (some-skill-selection-list-visible?) (close-skill-selection-lists)
      (is-dead? player-body) (when-not (try-revive-player)
                               (state/enter-state mainmenu-gamestate))
      :else (state/enter-state ids/options)))
  (when (input/is-key-pressed? :TAB)
    (state/enter-state ids/minimap))
  (when (and (get-setting :debug-mode)
             (input/is-key-pressed? :D))
    (swap! debug-mode not))
  (when (and (get-setting :is-pausable)
             (input/is-key-pressed? :P))
    (swap! cupd/running not))
  (when @cupd/running
    (input/update-mousebutton-state)
    (update-removelist)
    (update-items)
    (try
      (cupd/update-active-components delta (get-ingame-loop-entities))
      (catch Throwable t
        (println "Catched throwable: " t)
        (reset! cupd/running false)))

    (reset! iterating-map-dependent-comps true)
    (try
      (cupd/update-active-components delta (get-entities-in-active-content-fields))
      (catch Throwable t
        (println "Catched throwable: " t)
        (reset! cupd/running false)))
    (reset! iterating-map-dependent-comps false)

    (when (is-dead? player-body)
      (player-death))
    (check-change-map)))

;;; lightning debug (was game.tests.lightning)

(defn- get-corner-name [n]
  (case (int n)
    0 "TOP_LEFT"
    1 "TOP_RIGHT"
    2 "BOTTOM_RIGHT"
    3 "BOTTOM_LEFT"))

(defn- get-corner-position [x y half-w half-h corner]
  (let [x (float x) y (float y) half-w (float half-w) half-h (float half-h)]
    (cond
      (= corner Image/TOP_LEFT)     [(- x half-w) (- y half-h)]
      (= corner Image/TOP_RIGHT)    [(+ x half-w) (- y half-h)]
      (= corner Image/BOTTOM_LEFT)  [(- x half-w) (+ y half-h)]
      (= corner Image/BOTTOM_RIGHT) [(+ x half-w) (+ y half-h)])))

(defn- render-mouse-tile-blocked-corners [g xstart ystart ypuffer]
  (let [[x y] (mouse-int-tile-pos)
        ix (+ x 0.5)
        iy (+ y 0.5)
        half-w 0.5
        half-h 0.5
        light-posi (get-position player-body)]
    (dorun
      (map-indexed
        (fn [idx corner]
          (let [posi-vec (get-corner-position ix iy half-w half-h corner)]
            (render-readable-text g xstart (+ ystart (* idx ypuffer))
              (str (get-corner-name corner) " " posi-vec
                " LIGHT? "
                (not (ray-blocked? light-posi posi-vec))))))
        image-corners))))

;;; player status gui (was game.player.status-gui)

(defn- render-infostr-on-bar [g infostr y h]
  (render-readable-text g half-screen-w (+ y 3) :background false :centerx true :centery false
                        white
                        infostr))

(initialize
  (def ^:private rahmen (create-image "gui/rahmen.png"))
  (def ^:private rahmenw (first (get-dimensions rahmen)))
  (def ^:private rahmenh (second (get-dimensions rahmen)))
  (def ^:private hpcontent (create-image "gui/hp.png"))
  (def ^:private manacontent (create-image "gui/mana.png")))

(defn- render-hpmana-bar [g x y contentimg minmaxval name]
  (draw-image rahmen x y)
  (draw-image (get-sub-image contentimg 0 0 (* rahmenw (get-ratio minmaxval)) rahmenh) x y)
  (render-infostr-on-bar g (str (readable-number (:current minmaxval)) "/" (int (:max minmaxval)) " " name) y rahmenh))

(defn- render-player-stats [g]
  (let [x (- half-screen-w (/ rahmenw 2))
        y-hp (- screen-height rahmenh)
        y-mana (- y-hp rahmenh)]
    (render-hpmana-bar g x y-hp hpcontent (get-hp player-body) "HP")
    (render-hpmana-bar g x y-mana manacontent (get-mana player-body) "MP")))

(ingame-loop-comp :player-hp-mana
  (rendering :hpmanabar [g c]
    (render-player-stats g)))

(ingame-loop-comp :render-armor-status
  (rendering [g c]
    (when-let [mouseover-body (get-mouseover-body)]
      (when-let [armor (get-armor mouseover-body)]
        (render-readable-text g half-screen-w 0 :centerx true
                              (str (readable-number armor) " Armor\n" (get-armor-reduce-info armor)))))))

(initialize
  (def ^:private character-frame (make-frame :name :character
                                    :bounds [(- screen-width frame-screenborder-distance 160) (+ inventoryry inventory-height 5) 160 30]
                                    :hotkey char-hotkey
                                    :visible false
                                    :parent ingamestate-display)))

(defn- status-dmg-info [{:keys [is-melee dmg-info] :as skill}]
  (cond
    is-melee (variance-val-str (get-current-player-melee-dmg))
    dmg-info (dmg-info skill)))

(ingame-loop-comp :player-character-status
  (rendering [g c]
    (when (is-visible? character-frame)
      (let [[leftx topy] (:bounds @character-frame)]
        (render-readable-text g (+ leftx 2) (+ topy 2) :background false
          (str "Name: " @current-character-name)
          (str "Leftmouse Damage: " (status-dmg-info (get-selected-skill :left)))
          (str "Rightmouse Damage: " (status-dmg-info (get-selected-skill :right))))))))

;;; maps.render (was game.maps.render)

(defn- render-generated-grid [x y cells ^SpriteSheet sprite-sheet get-sprite-posi]
  (.startUse sprite-sheet)
  (doseq [[tx ty cell tileposi] cells]
    (when-let [sheet-posi (get-sprite-posi @cell)]
      (let [image (get-sprite sprite-sheet sheet-posi)
            render-x (+ x (* tile-width tx))
            render-y (+ y (* tile-height ty))]
        (set-cached-brightness image tileposi)
        (.drawEmbedded ^Image image render-x render-y tile-width tile-height))))
  (.endUse sprite-sheet))

(defn- render-gen-grids [x y sx sy width height]
  (let [grid (get-cell-grid)
        cells (for [tx (range width)
                    ty (range height)
                    :let [tileposi [(+ sx tx) (+ sy ty)]
                          cell (get grid tileposi)]
                    :when cell]
                [tx ty cell tileposi])
        {:keys [sprite-sheet details-sprite-sheet]} (get-current-map-data)]
    (render-generated-grid x y cells sprite-sheet :sprite-posi)
    (when details-sprite-sheet
      (render-generated-grid x y cells details-sprite-sheet :details-sprite-posi))))

(def ^:private marker-size 12)
(def ^:private pathfnd-marker-size 8)

(defn- render-debug-map-info
  [g render-tile-start-x render-tile-start-y render-start-x render-start-y]
  (let [xrange (range -1 (+ display-width-in-tiles 3))
        yrange (range -1 (+ display-height-in-tiles 3))
        half-tile-width (/ tile-width 2)
        half-tile-height (/ tile-height 2)
        half-marker-size (/ marker-size 2)
        mouseoverbody (get-mouseover-body)]
    (when debug/potential-field-following-mouseover-info
      (calculate-mouseover-body-colors mouseoverbody))
    (doseq [x xrange
            y yrange
            :let [tilex (+ x render-tile-start-x)
                  tiley (+ y render-tile-start-y)
                  cell (get-cell [tilex tiley])
                  corner-x (+ render-start-x (* x tile-width))
                  corner-y (+ render-start-y (* y tile-height))
                  xrect (- (+ corner-x half-tile-width) half-marker-size)
                  yrect (- (+ corner-y half-tile-height) half-marker-size)]
            :when cell]
      (when
        (and
          debug/show-besetzte-cells
          (seq (get-body-ids cell)))
        (fill-rect g xrect yrect marker-size marker-size yellow))
      (when debug/show-blocked-cells
        (when (cell-blocked? cell :ground)
          (fill-rect g xrect yrect marker-size marker-size red)))
      (when
        (and
          debug/show-occupied-cells
          (seq (:occupied (deref cell))))
        (fill-rect g xrect yrect marker-size marker-size yellow))
      (when debug/potential-field-following-mouseover-info
        (render-potential-field-following-mouseover-info g corner-x corner-y xrect yrect cell mouseoverbody))
      (when debug/show-potential-field
        (render-potential-field-info g corner-x corner-y xrect yrect cell)))
    (when debug/show-map-grid
      (set-color g black)
      (draw-grid g
                 (- render-start-x (* render-tile-start-x tile-width))
                 (- render-start-y (* render-tile-start-y tile-height))
                 (get-map-w)
                 (get-map-h)
                 tile-width
                 tile-height))))

(def left-offset-in-tiles-buffer (- left-offset-in-tiles half-display-w-in-tiles))
(def top-offset-in-tiles-buffer (- top-offset-in-tiles half-display-h-in-tiles))

(defn- rendermap [g]
  (let [[center-x center-y] (get-camera-position)
        center-tile-x (int center-x)
        center-tile-y (int center-y)
        center-tile-offset-x (int (* tile-width (- center-tile-x center-x)))
        center-tile-offset-y (int (* tile-height (- center-tile-y center-y)))
        sx (- center-tile-offset-x (int (* left-offset-in-tiles-buffer tile-width)) tile-width)
        sy (- center-tile-offset-y (int (* top-offset-in-tiles-buffer tile-height)) tile-height)
        tsx (dec (- center-tile-x left-offset-in-tiles))
        tsy (dec (- center-tile-y top-offset-in-tiles))
        render-width-tiles (+ display-width-in-tiles 3)
        render-height-tiles (+ display-height-in-tiles 3)]
    (if-let [^TiledMap tiled-map (:tiled-map (get-current-map-data))]
      (do
        (.render tiled-map sx sy tsx tsy render-width-tiles render-height-tiles (get-layer-index tiled-map "ground") false)
        (when-let [idx (get-layer-index tiled-map "details")]
          (.render tiled-map sx sy tsx tsy render-width-tiles render-height-tiles idx false)))
      (render-gen-grids sx sy tsx tsy render-width-tiles render-height-tiles))
    (when @debug-mode
      (render-debug-map-info g tsx tsy sx sy))))

;;; render-ingame (was game.render-ingame)

(defn- to-be-rendered-entities-from-map []
  (mapcat (fn [entity]
            (map #(vector entity %)
                 (filter :rendering (vals @entity))))
          (filter on-screen-and-in-sight?
                  (get-entities-in-active-content-fields))))

(defn- render-map-content [g]
  (let [[x y] (int-posi (translate-position (get-camera-position)))
        xtranslate (- half-screen-w x)
        ytranslate (- half-screen-h y)]
    (translate g xtranslate ytranslate)
    (doseq [[entity {:keys [renderfn] :as component}] (sort-by-order (to-be-rendered-entities-from-map)
                                                                     (comp :order second) render-on-map-order)]
      (try
       (renderfn g
                 entity
                 component
                 (translate-position (get-position entity)))
        (catch Throwable t
          (println "Render error for entity " (:id (meta entity)) " and component type " (:type component)))))
    (reset-transform g)))

(defn- get-map-independent-render-comps []
  (filter :rendering (mapcat #(vals @%) (get-ingame-loop-entities))))

(defn- render-gui [g]
  (doseq [{render :renderfn :as component} (sort-by-order (get-map-independent-render-comps)
                                                          :order render-map-indep-order)]
    (render g component)))

(defn- print-mouse-tile-position []
  (let [[tile-x tile-y] (get-mouse-tile-pos)]
    (str (float tile-x) " " (float tile-y))))

(defn- render-debug [g x mouseover-body]
  (let [starty 30
        lineh (get-line-height)]
    (render-readable-text g x (+ starty (* lineh 0)) (str "mouse" (get-mouse-pos)))
    (when debug/show-contentfield
      (render-readable-text g x (+ starty (* lineh 1)) (str "player content field:"  (get-player-content-field-idx))))
    (when mouseover-body
      (render-readable-text g x (+ starty (* lineh 2)) (str "maus-overbody id = "  (:id (meta mouseover-body)))))
    (when debug/show-float-mouse-pos
      (render-readable-text g x (+ starty (* lineh 3)) (str "maus-tile x,y = "  (print-mouse-tile-position))))
    (when debug/show-tile-mouse-pos
      (render-readable-text g x (+ starty (* lineh 4)) (str "int-tile x,y = "  (mouse-int-tile-pos))))
    (when debug/show-lightning-info
      (render-mouse-tile-blocked-corners g x (+ starty (* lineh 5)) 20))
    (when debug/show-comps-count
      (render-readable-text g x (+ starty (* lineh 7)) (str "to-be-rendered-entities-from-map "  (count (to-be-rendered-entities-from-map))))
      (render-readable-text g x (+ starty (* lineh 8)) (str "map-independent-render-comps "      (count (get-map-independent-render-comps)))))))

(ingame-loop-comp :debug-infos
  (rendering :above-gui [g c]
    (when @debug-mode
      (render-debug g 25 (get-mouseover-body)))))

(defgamestate ingame ids/ingame
  (enter [container statebasedgame]
    (input/clear-key-pressed-record)
    (input/clear-mouse-pressed-record))

  (init [container statebasedgame])

  (update [container statebasedgame delta]
    (let [delta (min delta game.components.update/max-delta)]
      (update-shake delta)
      (update-game delta)))

  (render [container statebasedgame g]
    (translate-shake-before-render g)
    (rendermap g)
    (render-map-content g)
    (render-gui g)
    (translate-shake-after-render g))

  (keyPressed [int-key chr]))
