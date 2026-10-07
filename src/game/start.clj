(ns game.start
  (:import (java.awt GridLayout Dimension)
           (javax.swing JPanel JFrame JButton ButtonGroup JRadioButton)
           (java.awt.event KeyEvent ActionListener)
           (org.newdawn.slick Image Color SpriteSheet Input Music)
           (org.newdawn.slick.gui TextField)
           (org.newdawn.slick.loading LoadingList DeferredResource)
           org.newdawn.slick.tiled.TiledMap)
  (:require
    [engine.input :as input :refer [get-mouse-pos is-key-pressed? is-rightm-consumed? try-consume-leftm-pressed try-consume-rightm-pressed update-mousebutton-state]]
    [engine.render :as g :refer [black create-animation create-image draw-grid draw-image fill-rect folder-animation folder-frames get-dimensions get-scaled-copy get-sprite get-sub-image green orange red render-readable-text reset-transform set-color spritesheet-frames translate white yellow]]
    [engine.statebasedgame :as state :refer [defgamestate enter-state init-state-based-game state-based-game]]
    [game.debug-settings :as debug]
    [game.session :as sess]
    [game.state.ids :as ids]
    [game.components.update :as cupd]
    game.components.body-render
    game.components.burrow
    game.maps.add
    [engine.settings :refer [jar-file?]]
    [game.status-options :refer [get-state get-text set-state status-check-boxes]]
    [game.mouse-cursor :refer [reset-default-mouse-cursor]]
    [game.maps.minimap :refer [render-minimap show-on-minimap]]
    [game.maps.data :refer [get-current-map-data iterating-map-dependent-comps]]
    [game.maps.mapchange :refer [check-change-map]]
    [game.components.sleeping :refer [wake-up]]
    [game.components.shield :refer [shield-component]]
    [game.monster.spawn :refer [try-spawn]]
    [game.player.session-data :refer [current-character-name get-session-file-character-names]]
    [game.utils.lightning :refer [image-corners light-component set-cached-brightness]]
    [game.utils.tilemap :refer [get-mouse-tile-pos mouse-int-tile-pos]]
    [utils.core :as utils :refer [assoc-in! get-ratio int-posi lower-than-max? readable-number runmap sort-by-order translate-to-tile-middle update-in! variance-val-str]]
    [engine.core :refer [app-game-container create-and-start-app-game-container create-sound defpreload fullscreen-supported? get-defaultfont get-line-height get-screen-height get-screen-width init-all initialize make-counter play-sound update]]
    [data.grid2d :refer [cells height posis width]]
    [game.settings :refer [debug-mode display-height-in-tiles display-width-in-tiles get-setting half-display-h-in-tiles half-display-w-in-tiles half-screen-h half-screen-w in-tiles init-config! left-offset-in-tiles screen-height screen-scale screen-width tile-height tile-width top-offset-in-tiles version]]
    [game.screenshake :refer [translate-shake-after-render translate-shake-before-render update-shake]]
    game.media
    [game.mouseoverbody :refer [get-mouseover-body]]
    [game.ingame-gui :refer [char-hotkey close-all-frames frame-screenborder-distance ingamestate-display is-visible? make-checkbox make-frame make-guidisplay make-label make-textbutton mouse-inside-some-gui-component? options-hotkey remove-guicomponent render-guicomponent set-visible some-visible-frame? update-guicomponent]]
    [game.maps.contentfields :refer [get-entities-in-active-content-fields get-player-content-field-idx]]
    [game.maps.cell-grid :refer [cell-blocked? get-body-ids get-cell get-cell-grid get-map-h get-map-w]]
    [game.maps.camera :refer [get-camera-position]]
    [game.maps.tiledmaps :refer [get-layer-index]]
    [game.components.core :refer [active add-to-removelist create-comp defcomponent exists? get-component get-components get-id get-position is-player? player-body update-counter! update-removelist]]
    game.components.position
    [game.components.body :refer [blocked-location? bodies-in-range? circle-collides? get-bodies-at-position get-dist-to-player on-screen-and-in-sight? teleport]]
    [game.components.misc :refer [hp-regen-component rotate-to-player rotation-component]]
    [game.components.render :refer [animation-component animation-entity create-line-render-effect create-lines-render-effect image-render-component render-map-indep-order render-on-map-order rendering single-animation-component translate-position]]
    [game.components.destructible :refer [calc-effective-spell-dmg deal-dmg explosion-frames get-armor get-armor-reduce-info get-destructible-bodies get-hp is-dead? set-hp-to-max]]
    game.components.body-effects
    [game.components.body-effects-impl :refer [add-psi-charge aoe-armor-reducer consume-psi-charges create-bullettime-effects current-psi-charges dmg-effect max-psi-charges slowdown-effect stun stun-collision-effect]]
    [game.components.movement :refer [movement-component projectile-movement-component]]
    [game.components.ingame-loop :refer [get-ingame-loop-entities ingame-loop-comp]]
    [game.components.active :refer [blocks-component]]
    [game.components.movement.ai.potential-field :refer [calculate-mouseover-body-colors path-to-player-movement potential-field-player-following render-potential-field-following-mouseover-info render-potential-field-info]]
    [game.entity.nova :refer [nova-effect]]
    [game.entity.projectile :refer [fire-projectile]]
    game.entity.teleporters
    [game.components.skills.core :refer [enough-mana? get-mana get-skill get-skill-use-mouse-tile-pos is-attacking? is-ready? is-usable? skillmanager-component skillmanager-skill standalone-skill]]
    [game.components.skills.melee :refer [get-current-player-melee-dmg melee-weapon monster-melee-component monster-melee-props player-melee-props]]
    [game.components.skills.utils :refer [check-line-of-sight get-player-ranged-vector]]
    [game.item.cells :refer [add-item-to-cell cell-empty-and-allows-item? cell-filled-and-allows-item? empty-item-in-hand get-item get-mouseover-item-cell hks-cells inc-count-of-item? inventory-height inventoryry is-item-in-hand? item-grids item-in-hand item-not-in-use? mouse-over-an-item-cell? remove-item-from-cell remove-one-item set-item-in-hand]]
    [game.item.instance :refer [create-item-body put-item-on-ground]]
    [game.item.instance-impl :refer [create-rand-item]]
    game.monsters.little-bot
    game.monsters.littlespider
    game.monsters.xploding-drone
    game.monsters.research-station
    game.monsters.mine
    game.monsters.skull-chainsaw
    game.monsters.storagebox
    game.monsters.armored-skull
    game.monsters.armored-skull2
    game.monsters.big-skull-chainsaw
    game.monsters.big-teleporting-melee
    game.monsters.burrower
    game.monsters.ranged
    game.monsters.shield-turret
    game.monsters.mage-skull
    game.monsters.fly
    game.monsters.healer
    game.monsters.instant-healer
    game.monsters.nova-melee
    game.monsters.slowdown-caster
    game.monsters.ray-shooter
    game.monsters.first-boss
    game.monsters.test-hunter
    game.player.skills.player-ranged
    game.player.skills.psi-charger
    game.player.skills.nova
    game.player.skills.bullettime-field
    game.player.skills.aoe-armor-reduce
    game.player.skills.stomp
    game.player.skills.psi-bolt
    [game.player.skill.selection-list :refer [close-skill-selection-lists some-skill-selection-list-visible?]]
    [game.player.skill.skillmanager :refer [get-selected-skill]]
    [game.player.core :refer [player-death try-revive-player]]
    [game.utils.raycast :refer [is-path-blocked? ray-blocked?]]
    [game.utils.random :refer [get-rand-weighted-item if-chance percent-chance rand-int-between]]
    [game.utils.geom :refer [get-angle-to-position get-touched-tiles get-vector-away-from-player get-vector-to-player in-range? normalise rotate-angle-to-angle scale vector-from-angle vector2f]])
  (:gen-class))

;;; serialization (was game.serialization)

(defn- relevant-data [^Image image]
  {:transparent-color (.transparent image)
   :iref              (.getResourceReference image)
   :width             (.getWidth          image)
   :height            (.getHeight         image)
   :texture-width     (.getTextureWidth   image)
   :texture-height    (.getTextureHeight  image)
   :texture-offset-x  (.getTextureOffsetX image)
   :texture-offset-y  (.getTextureOffsetY image)
   :texture (let [^org.newdawn.slick.opengl.Texture texture (.getTexture image)]
              {:texture-width  (.getTextureWidth  texture)
               :texture-height (.getTextureHeight texture)})})

(defn- save-image-data [image]
  (let [{:keys [iref
                transparent-color
                width
                height
                texture-width
                texture-height
                texture-offset-x
                texture-offset-y
                texture]}
        (relevant-data image)

        in-px-w (fn [texels] (int (* texels (:texture-width  texture))))
        in-px-h (fn [texels] (int (* texels (:texture-height texture))))
        sw (in-px-w texture-width)
        sh (in-px-h texture-height)
        sx (in-px-w texture-offset-x)
        sy (in-px-h texture-offset-y)]
    {:iref              iref
     :transparent-color transparent-color
     :sub-bounds        [sx sy sw sh]
     :bounds            [width height]}))

(defn- recreate-image [{iref              :iref
                        transparent-color :transparent-color
                        [sx sy sw sh]     :sub-bounds
                        [width height]    :bounds}]
  (-> (g/create-image iref :transparent transparent-color)
      (g/get-sub-image sx sy sw sh)
      (g/get-scaled-copy width height)))

(defmethod sess/write-to-disk Image [^Image i]
  (with-meta (save-image-data i)
             {:pr :image}))

(defmethod sess/load-from-disk :image [data]
  (recreate-image data))

(defmethod sess/write-to-disk Color [^Color c]
  (with-meta [(.r c) (.g c) (.b c) (.a c)]
             {:pr :color}))

(defmethod sess/load-from-disk :color [[r g b a]]
  (g/rgbcolor :r r :g g :b b :a a))

;;; resolution setup (was game.resolutionsetup)

(defn- makeframe []
  (doto (JFrame. "Resolution Setup")
    (.setVisible true)
    (.setResizable false)
    (.setLocationRelativeTo nil) ; center
    (.setPreferredSize (Dimension. 300 150))
    (.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)))

(defn- fits-in-desktop? [w h]
  (let [displaymode (org.lwjgl.opengl.Display/getDesktopDisplayMode)]
    (and (<= w (.getWidth displaymode))
         (<= h (.getHeight displaymode)))))

(defn- make-button [[scale width height] & {fs :fs}]
  {:scale scale
   :width width
   :height height
   :button (JRadioButton. (str width "*" height (when fs " fullscreen")))
   :fullscreen fs})

(defn- make-startbutton [start-the-game buttons frame]
  (let [startbutton (JButton. "Start")]
    (doto startbutton
      (.setMnemonic  KeyEvent/VK_S)
      (.addActionListener
        (reify ActionListener
          (actionPerformed [this e]
            (when-let [{:keys [scale fullscreen]} (first (filter #(.isSelected (:button %)) buttons))]
              (intern 'game.settings 'screen-scale scale)
              (.setEnabled startbutton false)
              (.dispose frame)
              (start-the-game fullscreen))))))))

(defn- disable-too-big-resolutions [buttons]
  (dorun
      (map #(when-not (fits-in-desktop? (:width %) (:height %))
              (.setEnabled (:button %) false))
           buttons)))

(defn- resolution-setup-frame [start-the-game]
  (let [scales-res (map (fn [s] [s (* screen-width s) (* screen-height s)]) [1 2 3 4])
        buttons (sort-by :scale
                         (concat (map make-button scales-res)
                                 (map #(make-button % :fs true)
                                      (filter #(fullscreen-supported? (% 1) (% 2)) scales-res))))
        panel (JPanel. (GridLayout. 0 1))
        buttongroup (ButtonGroup.)
        frame (makeframe)
        startbutton (make-startbutton start-the-game buttons frame)]
    (disable-too-big-resolutions buttons)
    (let [recommended (:button (first (filter #(= (:scale %) 3) buttons)))]
      (.setSelected (if (.isEnabled recommended)
                      recommended
                      (:button (first (filter #(.isEnabled (:button %)) (reverse buttons)))))
        true))
    (dorun (map #(.add buttongroup (:button %)) buttons))
    (dorun (map #(.add panel (:button %)) buttons))
    (.add panel startbutton)
    (.add frame panel)
    (.pack frame)))

;;; item update (was game.item.update)

(defn- update-drag-and-drop []
  (let [mouseover-cell (get-mouseover-item-cell)]
    (cond
      (and
        (mouse-over-an-item-cell?)
        (not (is-item-in-hand?))
        (get-item mouseover-cell)
        (item-not-in-use? mouseover-cell))
      (do
        (play-sound "bfxr_takeit.wav")
        (set-item-in-hand (get-item mouseover-cell))
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

        (inc-count-of-item? (get-item mouseover-cell) @item-in-hand)
        (do
          (play-sound "bfxr_itemput.wav")
          (swap! (:item mouseover-cell) update-in [:count] + (:count @item-in-hand))
          (empty-item-in-hand))

        (cell-filled-and-allows-item? mouseover-cell @item-in-hand)
        (let [cell-item (get-item mouseover-cell)]
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
              item (get-item cell)]
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
           (get-item mouseover-cell)
           (= (:type (get-item mouseover-cell)) :usable)
           (item-not-in-use? mouseover-cell))
      (try-usable-item-effect (get-item mouseover-cell) mouseover-cell))))

;;; update-ingame (was game.update-ingame)

(declare mainmenu-gamestate)

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
                 (filter :rendering (get-components entity))))
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
          (println "Render error for entity " (get-id entity) " and component type " (:type component)))))
    (reset-transform g)))

(defn- get-map-independent-render-comps []
  (filter :rendering (mapcat get-components (get-ingame-loop-entities))))

(defn- render-gui [g]
  (doseq [{render :renderfn :as component} (sort-by-order (get-map-independent-render-comps)
                                                          :order render-map-indep-order)]
    (render g component)))

(defn- render-game [g]
  (rendermap g)
  (render-map-content g)
  (render-gui g))

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
      (render-readable-text g x (+ starty (* lineh 2)) (str "maus-overbody id = "  (get-id mouseover-body))))
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

;;; load-session + mainmenu (were game.state.load-session / main)

(def is-loaded-character (atom false))

(def ^:private loading-render-once (atom false))

(defgamestate loading
  (enter [container statebasedgame]
    (reset! loading-render-once false))

  (init [container statebasedgame])

  (update [container statebasedgame delta]
    (when @loading-render-once
      (utils/log "Loading new session")
      (game.player.session-data/init @is-loaded-character)
      (utils/log "Finished loading new session")
      (state/enter-state ids/ingame)))

  (render [container statebasedgame g]
    (reset! loading-render-once true)
    (g/render-readable-text g (/ (get-screen-width) 2) (/ (get-screen-height) 2) :centerx true "Loading...")))

(def ^:private creditstxt
  "Created by Michael Sappler
  Devlog: http://resatori.com

  Graphics:
  - Icons by Lorc
  - lostgarden.com by Daniel Cook
  - AI Wars Graphics pack (arcen games):
  Chris Park, Daniel Cook, Philippe Chabot
  and Hans Martin Portmann.
  - AngbandTk Icons by David Gervais

  Music:
  - dungeon1.xm
  from http://modarchive.org/
  by Gammis of Lemonride ")

(def ^:private menu-buttons-x 5)
(def ^:private menu-second-column-x (+ menu-buttons-x 100))
(def ^:private menu-buttons-y 20)

(defn- init-textfield []
  (def ^:private textfield (TextField. app-game-container (get-defaultfont) menu-second-column-x menu-buttons-y (* (+ 15 3) 6) 10))
  (def ^:private textfield-visible (atom false))
  (.setConsumeEvents textfield false)
  (.setFocus textfield false)
  (.setMaxLength textfield 15))

(defn- start-loading-game [character-name & {new-character :new-character}]
  (.setFocus textfield false)
  (reset! is-loaded-character (not new-character))
  (reset! current-character-name character-name)
  (enter-state loading-gamestate))

(defn- try-create-character []
  (when-let [char-name (seq (.getText textfield))]
    (start-loading-game (apply str char-name) :new-character true)))

(def ^:private menu-display (make-guidisplay))

(declare load-saved-game-components)

(defn- init-load-saved-game-textbuttons []
  (when (bound? #'load-saved-game-components)
    (dorun (map #(remove-guicomponent menu-display %) load-saved-game-components)))
  (def ^:private load-saved-game-components (map-indexed
                                              (fn [idx char-name]
                                                (make-textbutton
                                                  :text char-name
                                                  :location [menu-second-column-x (+ menu-buttons-y (* idx 12))]
                                                  :pressed #(start-loading-game char-name :new-character false)
                                                  :visible false
                                                  :parent menu-display))
                                              (get-session-file-character-names))))

(declare set-visiblity-state)

(initialize
  (init-textfield)
  (def ^:private create-char-button (make-textbutton :location [menu-second-column-x (+ menu-buttons-y 30)]
                                                     :text "create"
                                                     :pressed try-create-character
                                                     :parent menu-display))
  (def ^:private credits-label (make-label :location [menu-second-column-x menu-buttons-y]
                                           :text creditstxt
                                           :parent menu-display))
  (make-textbutton :location [menu-buttons-x menu-buttons-y]
                   :text "New Character"
                   :pressed (fn []
                              (.setText textfield "")
                              (.start (Thread. (fn []
                                                 (Thread/sleep 100)
                                                 (.setFocus textfield true))))
                              (set-visiblity-state :new-char))
                   :parent menu-display)
  (make-textbutton :text "Load Character"
                   :location [menu-buttons-x (+ menu-buttons-y 25)]
                   :pressed #(set-visiblity-state :load-char)
                   :parent menu-display)
  (make-textbutton :text "Credits"
                   :location [menu-buttons-x (+ menu-buttons-y 50)]
                   :pressed #(set-visiblity-state :credits)
                   :parent menu-display)
  (make-textbutton :text "Exit Game"
                   :location [menu-buttons-x (+ menu-buttons-y 75)]
                   :pressed #(.exit app-game-container)
                   :parent menu-display))

(let [visibility {:none      [false false false false]
                  :new-char  [false true true false]
                  :load-char [false false false true]
                  :credits   [true false false false]}]
  (defn- set-visiblity-state [vis-state]
    (let [current (vis-state visibility)]
      (set-visible credits-label (current 0))
      (reset! textfield-visible (current 1))
      (set-visible create-char-button (current 2))
      (dorun (map #(set-visible % (current 3)) load-saved-game-components)))))

(defn- reset-menu-state []
  (init-load-saved-game-textbuttons)
  (set-visiblity-state :none))

(def ^:private menu-skipped (atom false))

(def ^:private music nil)

(initialize
  (alter-var-root #'music (constantly
                            (doto (Music. "sounds/dungeon1.xm" true)
                              (.setVolume (float 1))))))

(defgamestate mainmenu
  (enter [container statebasedgame]
    (LoadingList/setDeferredLoading false)
    (when music
      (.loop ^Music music))
    (reset-menu-state))

  (keyPressed [int-key chr]
    (when (and @textfield-visible (= int-key Input/KEY_ENTER))
      (try-create-character))
    (when (= int-key Input/KEY_ESCAPE)
      (.exit app-game-container)))

  (leave [container statebasedgame])

  (init [container statebasedgame])

  (update [container statebasedgame delta]
    (update-mousebutton-state)
    (update-guicomponent menu-display)
    (when (and (get-setting :skip-main-menu-at-startup)
               (not @menu-skipped))
      (reset! menu-skipped true)
      (start-loading-game "Testchar" :new-character true)))

  (render [container statebasedgame g]
    (render-guicomponent g menu-display)
    (when @textfield-visible
      (.render textfield container g))
    (render-readable-text g half-screen-w 0 :centerx true :background false :bigfont true "Cyber Dungeon Quest")
    (render-readable-text g half-screen-w (- screen-height (get-line-height)) :centerx true :background false version)))

(def ^:private next-resource (atom nil))
(def ^:private loaded-resources (atom []))

(defgamestate deferred-preload
  (init [container statebasedgame]
    (LoadingList/setDeferredLoading true)
    (init-all))

  (update [container statebasedgame delta]
    (when-let [resource @next-resource]
      (.load resource)
      (swap! loaded-resources conj resource)
      (reset! next-resource nil))
    (when (> (.getRemainingResources (LoadingList/get)) 0)
      (reset! next-resource (.getNext (LoadingList/get))))
    (when (zero? (.getRemainingResources (LoadingList/get)))
      (enter-state mainmenu-gamestate)))

  (render [container statebasedgame g]
    (render-readable-text g 0 0 :shift true (apply str (interleave (map #(.getDescription %) @loaded-resources) (repeat "\n"))))))

;;; game states (were game.state.ingame / minimap / options)

(defn- limit-delta [delta]
  (min delta game.components.update/max-delta))

(defgamestate ingame ids/ingame
  (enter [container statebasedgame]
    (input/clear-key-pressed-record)
    (input/clear-mouse-pressed-record))

  (init [container statebasedgame])

  (update [container statebasedgame delta]
    (let [delta (limit-delta delta)]
      (update-shake delta)
      (update-game delta)))

  (render [container statebasedgame g]
    (translate-shake-before-render g)
    (render-game g)
    (translate-shake-after-render g))

  (keyPressed [int-key chr]))

(defgamestate minimap ids/minimap
  (enter [container statebasedgame])

  (init [container statebasedgame])

  (update [container statebasedgame delta]
    (when (or (is-key-pressed? :TAB)
              (is-key-pressed? :ESCAPE))
      (enter-state ids/ingame)))

  (render [container statebasedgame g]
    (render-minimap g)))

(def ^:private options-bx 5)
(def ^:private options-by 20)
(def ^:private options-ypuffer 12)

(initialize
  (def ^:private options-display (make-guidisplay))
  (make-textbutton
    :text "Exit"
    :location [options-bx options-by]
    :pressed #(enter-state mainmenu-gamestate)
    :parent options-display)
  (make-textbutton
    :text "Resume"
    :location [options-bx (+ options-by (* options-ypuffer 2))]
    :pressed #(enter-state ids/ingame)
    :parent options-display)
  (dorun
    (map-indexed
      (fn [idx item]
        (make-checkbox :text (get-text item)
                       :location [100 (+ options-by (* options-ypuffer idx))]
                       :pressed #(set-state item %)
                       :selected (boolean (get-state item))
                       :parent options-display))
      @status-check-boxes))
  (when-not (fullscreen-supported?)
    (make-label :location [options-bx 150]
                :text "This resolution is not supported in fullscreen mode."
                :visible true
                :parent options-display)))

(defgamestate options ids/options
  (enter [container statebasedgame])

  (init [container statebasedgame])

  (update [container statebasedgame delta]
    (update-mousebutton-state)
    (update-guicomponent options-display)
    (when (is-key-pressed? :ESCAPE)
      (enter-state ids/ingame)))

  (render [container statebasedgame g]
    (render-guicomponent g options-display)))

;;; boot

(defn- start-the-game [fullscreen]
  (init-state-based-game "Cyber-Dungeon-Quest"
    [deferred-preload-gamestate
     mainmenu-gamestate
     loading-gamestate
     ingame-gamestate
     options-gamestate
     minimap-gamestate])
  (create-and-start-app-game-container
    :game state-based-game
    :width screen-width
    :height screen-height
    :scale screen-scale
    :full-screen fullscreen
    :show-fps (get-setting :show-fps)
    :lock-framerate false))

(defn start [& {:keys [config]}]
  (let [development-mode (if-let [dev (System/getenv "development")]
                           (Boolean/valueOf dev)
                           (not jar-file?))
        file (cond config config
                   development-mode "config/development.clj"
                   :else "config/production.clj")]
    (utils/log "Using config file: " file)
    (init-config! file))
  (if (get-setting :show-resolution-setup)
    (resolution-setup-frame start-the-game)
    (start-the-game false)))

(defn -main []
  (start))
