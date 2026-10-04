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
    [utils.core :as utils :refer [assoc-in! defnks get-ratio int-posi lower-than-max? readable-number runmap sort-by-order translate-to-tile-middle update-in! variance-val-str]]
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
    [game.monster.defmonster :refer [defmonster get-monster-properties monsterimage monsterresrc]]
    [game.player.skill.learnable :refer [deflearnable-skill learnable-skills]]
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

;;; learnable skills (was game.player.skill.learnable-impl)

(defn- dmg-info-player-spell [skill]
  (variance-val-str
    (calc-effective-spell-dmg
      (:dmg skill)
      (:percent-modify-spell (get-component player-body :item-boni)))))

(defpreload ^:private projectile-frames (folder-frames "effects/energyball/"))

(deflearnable-skill player-ranged
  :manacost 2
  :menu-posi [0 0]
  :mousebutton :both
  :icon "icons/ranged.png"
  :info "Fires a projectile"
  {:dmg [5 7]
   :dmg-info dmg-info-player-spell
   :show-info-for [:cost :dmg]
   :animation :casting
   :do-skill (fn [entity component]
               (fire-projectile
                 :startbody entity
                 :px-size 8
                 :animation (create-animation projectile-frames :looping true)
                 :side :player
                 :hits-side :monster
                 :movement (projectile-movement-component (get-player-ranged-vector) 160)
                 :hit-effects [(dmg-effect (:dmg component) :is-player-spell true)
                               (stun-collision-effect 100 200)]
                 :piercing false
                 :maxrange 8))})

(defn- psi-charge-hit-effect [_]
  (let [{:keys [attack-bonus movement-bonus]} (:psi-charger learnable-skills)]
    (add-psi-charge player-body attack-bonus movement-bonus)))

(deflearnable-skill psi-charger
  :manacost 12
  :menu-posi [1 0]
  :mousebutton :both
  :icon "icons/psi_charger.png"
  :info
  "Melee Attack where every successful hit
gives you a PSI-Charge. PSI-Charges give
you a passive bonus and can be used with
finisher-skills.

1 Charge  - +20% Movement-Speed
            +13% Attack-Speed
2 Charges - +40% Movement-Speed
            +27% Attack-Speed
3 Charges - +60% Movement-Speed
            +40% Attack-Speed"
  {:attack-bonus (/ 0.4 max-psi-charges)
   :movement-bonus (/ 0.6 max-psi-charges)
   :show-info-for [:cost]}
  (player-melee-props [psi-charge-hit-effect]))

(defpreload ^:private nova-frames (folder-frames "effects/nova/"))

(deflearnable-skill nova
  :manacost 10
  :menu-posi [2 0]
  :mousebutton :right
  :icon "icons/nova.png"
  :info "Fires a nova."
  {:dmg [6 9]
   :dmg-info dmg-info-player-spell
   :radius 3
   :show-info-for [:cost :dmg]
   :animation :casting
   :do-skill (fn [entity {:keys [radius dmg] :as component}]
               (nova-effect
                 :position (get-position entity)
                 :duration 200
                 :maxradius radius
                 :affects-side :monster
                 :dmg dmg
                 :is-player-spell true
                 :animation (create-animation nova-frames)))})

(def ^:private curse-infostr "Curse\nOnly one curse is active at a time\n")

(defpreload ^:private curse-frames (map #(get-scaled-copy % 0.3) (folder-frames "effects/curse/")))

(defn- create-curse-effect [[x y]]
  (play-sound "bfxr_curse.wav")
  (animation-entity
    :position [x (- y (in-tiles (/ 80 3)))]
    :animation (create-animation curse-frames)))

(deflearnable-skill bullettime-field
  :manacost 50
  :menu-posi [0 1]
  :mousebutton :both
  :icon "icons/bullettime.png"
  :info (str curse-infostr "Slows down monsters by 60%")
  {:radius 1.5
   :seconds 20
   :show-info-for [:cost :seconds]
   :animation :casting
   :check-usable check-line-of-sight
   :do-skill (fn [_ {:keys [radius seconds]}]
               (let [posi (get-skill-use-mouse-tile-pos)]
                 (create-curse-effect posi)
                 (create-bullettime-effects posi radius seconds)))})

(deflearnable-skill aoe-armor-reduce
  :manacost 50
  :menu-posi [0 2]
  :mousebutton :both
  :icon "icons/armorreduce.png"
  :info (str curse-infostr "Reduces armor by 50%")
  {:radius 1.5
   :seconds 20
   :show-info-for [:cost :seconds]
   :animation :casting
   :check-usable check-line-of-sight
   :do-skill (fn [_ {:keys [radius seconds]}]
               (let [posi (get-skill-use-mouse-tile-pos)]
                 (create-curse-effect posi)
                 (aoe-armor-reducer posi radius 50 seconds)))})

(deflearnable-skill stomp
  :manacost 10
  :menu-posi [1 1]
  :mousebutton :right
  :icon "icons/stomp.png"
  :info "Finisher-Skill
Stuns and deals damage

0 Charges - 1s stun duration
1 Charge  - 1.3s stun duration
            20% melee weapon damage
2 Charges - 1.6s stun duration
            40% melee weapon damage
3 Charges - 2s stun duration
            60% melee weapon damage"
  {:dmg-info (fn [skill]
               (let [cnt (current-psi-charges player-body)
                     [mn mx] (get-current-player-melee-dmg)
                     modifier (* cnt (:dmg-modifier skill))]
                 (variance-val-str
                   [(* modifier mn) (* modifier mx)])))
   :radius 2
   :dmg-modifier 0.2
   :animation :casting
   :show-info-for [:cost]
   :do-skill (fn [entity {:keys [radius dmg-modifier] :as component}]
               (let [posi (get-position entity)
                     cnt (consume-psi-charges entity)
                     duration (+ 1000 (* cnt 333))
                     dmg (* cnt dmg-modifier (rand-int-between (get-current-player-melee-dmg)))
                     hits (get-destructible-bodies posi radius :monster)]
                 (play-sound "explode-erco.wav")
                 (animation-entity
                   :position posi
                   :animation (folder-animation :folder "effects/stomp/" :looping false))
                 (runmap #(stun % duration) hits)
                 (runmap #(deal-dmg dmg %) hits)))})

(def ^:private psi-bolt-pxradius 17)
(defpreload ^:private psi-bolt-frames (folder-frames "effects/psibolt/"))

(defn- do-skill-psi-bolt [entity component]
  (let [cnt (consume-psi-charges entity)
        radius (in-tiles psi-bolt-pxradius)
        dmg (*
              (+ 0.5 (* cnt 0.5))
              (rand-int-between (get-current-player-melee-dmg)))
        posi (get-skill-use-mouse-tile-pos)
        hits (get-destructible-bodies posi radius :monster)]
    (play-sound "bfxr_psibolt.wav")
    (animation-entity
      :animation (create-animation psi-bolt-frames)
      :position posi)
    (runmap #(deal-dmg dmg %) hits)))

(deflearnable-skill psi-bolt
  :manacost 15
  :menu-posi [1 2]
  :mousebutton :right
  :icon "icons/psibolt.png"
  :info "Finisher-Skill
PSI-Explosion

0 Charges - 50% melee weapon damage
1 Charge  - 100% melee weapon damage
2 Charges - 150% melee weapon damage
3 Charges - 200% melee weapon damage"
  {:animation :casting
   :show-info-for [:cost]
   :check-usable check-line-of-sight
   :do-skill do-skill-psi-bolt
   :dmg-info (fn [entity skill]
               (let [cnt (current-psi-charges entity)
                     [mn mx] (get-current-player-melee-dmg)
                     modifier (+ 0.5 (* cnt 0.5))]
                 (variance-val-str
                   [(* modifier mn) (* modifier mx)])))})

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

;;; movement.ai.homing (was game.components.movement.ai.homing)

(defn- move-and-rotate-to-target-control
  [projectile {:keys [target-body current-angle rotationspeed]} delta]
  (vector-from-angle
    (if-not (exists? target-body)
      current-angle
      (let [angle-to-target (get-angle-to-position (get-position projectile) (get-position target-body))
            adjusted-angle (rotate-angle-to-angle current-angle angle-to-target rotationspeed delta)]
        (assoc-in! projectile [:movement :current-angle] adjusted-angle)
        adjusted-angle))))

(defn create-homing-movement [speed target-body start-angle rotationspeed move-type]
  (movement-component
    {:control-update move-and-rotate-to-target-control
     :target-body target-body
     :current-angle start-angle
     :rotationspeed rotationspeed}
    speed
    move-type))


;;; movement.ai.ranged-monster (was game.components.movement.ai.ranged-monster)

(defn- runaway [body component]
  (cond
    (not (is-usable? body (get-skill body :ranged)))
    (potential-field-player-following body)

    (bodies-in-range? body player-body (:runaway-dist-sqrd component))
    (get-vector-away-from-player body)

    :else
    nil))

(defn- randomise [body _]
  (cond
    (not (is-usable? body (get-skill body :ranged)))
    (potential-field-player-following body)

    :else
    (normalise
      (vector2f
        (if-chance 50 (rand) (- (rand)))
        (if-chance 50 (rand) (- (rand)))))))

(defn- duration-counter-control
  [body {:keys [current-movement-vector create-vectorfn counter] :as component} delta]
  (if (or (nil? current-movement-vector)
          (update-counter! body delta component))
    (let [v (create-vectorfn body component)]
      (assoc-in! body [:movement :current-movement-vector] v)
      v)
    current-movement-vector))

(defn ranged-runaway-movement-comp
  ([speed runaway-dist-in-tiles move-type]
    (movement-component
      {:control-update duration-counter-control
       :create-vectorfn runaway
       :counter (make-counter 300)
       :current-movement-vector nil
       :runaway-dist-sqrd (Math/pow runaway-dist-in-tiles 2)}
      speed
      move-type)))

(defn ranged-randomly-moving-comp
  [speed duration move-type]
  (movement-component
    {:control-update duration-counter-control
     :create-vectorfn randomise
     :counter (make-counter duration)
     :current-movement-vector nil}
    speed
    move-type))

; hat nix mit ranged_monster zu tun -> tu woanders hin
; wenn hp wieder max ist (also geheilt wurden -> finished!)
(defn- update-lowhp-runaway [body {:keys [running-away counter] :as c} delta]
  (let [finished (and running-away (update-counter! body delta c))]
    (when finished
      (assoc-in! body [:movement :running-away] false))
    (if (and running-away (not finished))
      (get-vector-away-from-player body)
      (potential-field-player-following body))))

(defn lowhp-runaway-movement [speed]
  (movement-component {:control-update update-lowhp-runaway
                       :running-away false
                       :counter (make-counter 0)}
                      speed
                      :ground))

(defn rand-when-low-hp [body]
  (->> body get-hp get-ratio (- 1) (* 100) percent-chance))

(defn lowhp-dealt-dmg-trigger [body lethal]
  (let [move-comp (get-component body :movement)]
    (when (and (not lethal)
               (not (:running-away move-comp))
               (rand-when-low-hp body))
      (update-in! body [:movement]
                  #(-> %
                       (assoc-in [:counter :maxcnt] (* (rand-int-between 3 10) 1000))
                       (assoc-in [:running-away] true))))))

;;; monsters (was game.monster.monsters)

(defn- path-to-player-blocked?
  [[sx sy] projectile-pxsize]
  (let [[tx ty] (get-position player-body)
        path-w (in-tiles projectile-pxsize)]
    (is-path-blocked? sx sy tx ty path-w)))

(defpreload ^:private redball-frames (folder-frames "effects/red_ball/"))

(let [maxrange 10
      maxrange-squared (Math/pow maxrange 2)
      pxsize 7]
  (defn- redball-projectile-skill-props []
    {:show-cast-bar false
     :check-usable (fn [entity component]
                     (and (not (path-to-player-blocked? (get-position entity) pxsize))
                          (bodies-in-range? entity player-body maxrange-squared)))
     :do-skill (fn [entity component]
                 (fire-projectile
                   :startbody entity
                   :px-size pxsize
                   :animation (create-animation redball-frames :looping true)
                   :side :monster
                   :hits-side :player
                   :movement (projectile-movement-component (get-vector-to-player entity) 80)
                   :hit-effects [(dmg-effect [3 5])
                                 (stun-collision-effect 10 150)]
                   :maxrange maxrange))}))

(defnks ranged-component
  [:cooldown :opt :attacktime :opt-def :state-blocks {:attacking :movement}]
  (standalone-skill
    :stype :ranged
    :cooldown cooldown
    :attacktime attacktime
    :state-blocks state-blocks
    :props (redball-projectile-skill-props)))

;;

(defpreload ^:private monsterdie-frames (folder-frames "effects/monsterexplosion/"))

(defn- monster-die-effect [body]
  (animation-entity
    :animation (create-animation monsterdie-frames)
    :position (get-position body)))

(def ^:private monster-drop-table
  {{"Grenade" 1
    "Battle-Drugs" 1} 2
   {
    ;"Mana-Potion" 2
    "Heal-Potion" 4
    "Big-Mana-Potion" 1
    "Big-Heal-Potion" 4} 9})

(defn- default-monster-death [body & {sound :sound :or {sound true}}]
  (monster-die-effect body) ; "effect" was ist das? sound/animation oder was
  (when sound
    (play-sound "bfxr_defaultmonsterdeath.wav"))
  (let [position (get-position body)]
    (if-chance 20 ; TODO when-chance
      (let [item-name (get-rand-weighted-item
                        (get-rand-weighted-item monster-drop-table))]
        (create-item-body position item-name)))
    (if-chance 6
      (create-rand-item position :max-lvl (:rand-item-max-lvl (game.maps.data/get-current-map-data))))))

(defn death-trigger [f] (create-comp :death-trigger {:destruct f}))

(defn- default-death-trigger [] (death-trigger default-monster-death))

(defpreload ^:private bigger-explosion-frames (spritesheet-frames "effects/explosn.png" 20 20))
(defpreload ^:private big-explosion-frames (spritesheet-frames "effects/expbig.png" 40 40))

(defn- rand-posis-hit-effect
  "pixel distance from center of boss for explosions"
  [body hit-posis & {big-explosion :big-explosion}]
  (doseq [[x y] (take
                  (int (/ (count hit-posis) 2))
                  (shuffle hit-posis))
          :let [vx (/ x tile-width)
                vy (/ y tile-height)
                [x y] (get-position body)
                explosion-posi [(+ x vx) (+ y vy)]]]
    (animation-entity
      :position explosion-posi
      :animation (create-animation (if big-explosion
                                     big-explosion-frames
                                     (if-chance 50 explosion-frames bigger-explosion-frames))))))

(defn- big-body-hit-effect
  [hit-posis]
  (create-comp :hit-effect
    {:trigger (fn [body] (rand-posis-hit-effect body hit-posis))}))

;;

(defmonster little-bot {:hp 0.5 :armor 0 :pxsize 9}
  (death-trigger (fn [body]
                   (play-sound "bfxr_defaultmonsterdeath.wav")
                   (monster-die-effect body)))
  (path-to-player-movement 15)
  (rotation-component)
  (image-render-component (monsterimage "littlebot.png"))
  (monster-melee-component
    :cooldown 1000
    :attacktime 100
    :hit-sound (create-sound "slash.wav")
    :target-id (get-id player-body)))

(defn- get-free-posis [body position half-w half-h]
  (remove #(blocked-location? % body)
          (map translate-to-tile-middle
               (get-touched-tiles position half-w half-h))))

(defn monsterteleport-animation [position]
  (animation-entity
    :animation (create-animation (spritesheet-frames "effects/red_teleport.png" 17 17) :frame-duration 100)
    :order :on-ground
    :position position))

; TODO mach irgendwo in SICHTBAREN cells von player
; nur dann m�glich wenn er eine findet
; set-hp-to-max macht noch ne animation h�tt ich net erwartet
; wenn lethal dmg dann hier healing aber trotzdem death -> aber sonst fast unkillbar wohl...
(defn- teleport-and-heal-when-low-hp [body lethal]
  (when (and (not lethal)
             (rand-when-low-hp body)
             (zero? (rand-int 5))) ; not too strong ... only 1 in 5
    (play-sound "bfxr_monstercast.wav")
    (let [free-posis (get-free-posis body (get-position body) 6 3)]
      (when (seq free-posis)
        (let [posi (rand-nth free-posis)]
          (teleport body posi)
          (monsterteleport-animation posi)))
      (set-hp-to-max body)))) ; danach hp-to-max damit an neuer posi +hp string steht

(defn- normal-monster-melee []
  (monster-melee-component :cooldown 500
                           :attacktime 250
                           :hit-sound (create-sound "slash.wav")
                           :target-id (get-id player-body)))

(defmonster littlespider {:hp 1 :armor 7 :pxsize 13}
  (default-death-trigger)
  (path-to-player-movement 47)
  (rotation-component)
  (single-animation-component
    (folder-animation :folder (monsterresrc "littlespider/") :duration 300 :looping true))
  (normal-monster-melee))

(defmonster xploding-drone {:hp 1.5 :armor 5 :pxsize 15}
  (death-trigger (fn [this-body]
                   (default-monster-death this-body :sound false) ; TODO this strange sound false and play-sound ... => default monster dead more than 1 thing...
                   (play-sound "bfxr_dronedeath.wav")
                   (nova-effect ; TODO two novas because different dmg to player/monster ... => im dealt dmg trigger berücksichtigen?
                     :position (get-position this-body)
                     :duration 150
                     :maxradius 2
                     :affects-side [:player]
                     :dmg [20 20]
                     :animation (folder-animation :folder "effects/xpldrone/" :duration 150 :looping false))
                   (nova-effect
                     :position (get-position this-body)
                     :duration 150
                     :maxradius 2
                     :affects-side [:monster]
                     :dmg [4 8]
                     :animation (folder-animation :folder "effects/xpldrone/" :duration 150 :looping false))))
  (path-to-player-movement 13)
  (rotation-component)
  (single-animation-component
    (folder-animation :folder (monsterresrc "xplodingdrone/") :duration 500 :looping true))
  (monster-melee-component
    :cooldown 500
    :attacktime 500
    :hit-sound (create-sound "slash.wav")
    :target-id (get-id player-body)))

(let [posis [[9 -11] [-1 -11] [-13 -7] [-9 2] [3 3] [3 15] [-5 14] [7 17] [-11 5] [-12 14]]]
  (defmonster research-station {:hp 10 :armor 25 :pxsize 43}
    (big-body-hit-effect posis)
    (death-trigger (fn [body]
                     (play-sound "bfxr_stationdeath.wav")
                     (rand-posis-hit-effect body posis :big-explosion true)))
    (show-on-minimap orange)
    (single-animation-component
      (folder-animation :folder (monsterresrc "station/") :duration 500 :looping true))))

(defpreload ^:private mine-explosion-frames (folder-frames "effects/mine/"))

(defmonster mine {:hp 1 :armor 0 :pxsize 15}
  (death-trigger (fn [this-body]
                   (play-sound "bfxr_minedeath.wav")
                   (nova-effect
                     :position (get-position this-body)
                     :duration 300
                     :maxradius 4
                     :affects-side [:player]
                     :dmg [30 40]
                     :animation (create-animation mine-explosion-frames))
                   (nova-effect
                     :position (get-position this-body)
                     :duration 300
                     :maxradius 4
                     :affects-side [:monster]
                     :dmg [6 8]
                     :animation (create-animation mine-explosion-frames))))
  (image-render-component (monsterimage "mine.png"))
  (single-animation-component
    (create-animation (spritesheet-frames "effects/red_glow.png" 32 32)
                      :frame-duration 150
                      :looping true)
    :order :on-ground))

(defmonster skull-chainsaw {:hp 1.3 :armor 4 :pxsize 15}
  (default-death-trigger)
  (lowhp-runaway-movement 50)
  (create-comp :dealt-dmg-trigger {:do lowhp-dealt-dmg-trigger})
  (rotation-component)
  (single-animation-component
    (folder-animation :folder (monsterresrc "teleportraider/") :duration 500 :looping true))
  (normal-monster-melee))

(defmonster storagebox {:hp 1 :armor 25 :pxsize 41}
  ;(default-death-trigger)
  (image-render-component (monsterimage "storage1.png")))

(defmonster armored-skull {:hp 2 :armor 65 :pxsize 15}
  (default-death-trigger)
  (path-to-player-movement 13)
  (rotation-component)
  (hp-regen-component 2)
  (image-render-component (monsterimage "coredemonhand.png"))
  (normal-monster-melee))

(defmonster armored-skull2 {:hp 1 :armor 23 :pxsize 15}
  (default-death-trigger)
  (path-to-player-movement 13)
  (rotation-component)
  (hp-regen-component 5)
  (image-render-component (monsterimage "vorticularcutlass.png"))
  (normal-monster-melee))

(defmonster big-skull-chainsaw {:hp 3 :armor 25 :pxsize 15}
  (default-death-trigger)
  (path-to-player-movement 25)
  (rotation-component)
  (hp-regen-component 1)
  (image-render-component (monsterimage "vorticularcutlassiv.png"))
  (normal-monster-melee))

(defmonster big-teleporting-melee {:hp 5 :armor 15 :pxsize 15}
  (default-death-trigger)
  (path-to-player-movement 8)
  (rotation-component)
  (single-animation-component
    (folder-animation :folder (monsterresrc "corebomber/") :duration 300 :looping true))
  (normal-monster-melee)
  (standalone-skill
    :stype :teleporting
    :cooldown 1000
    :attacktime 600
    :props {:show-cast-bar true
            :shoot-sound "bfxr_bigmeleeteleport.wav"
            :check-usable (fn [entity _]
                            (let [dist (get-dist-to-player entity)]
                              (or (not dist) (>= dist 80))))
            :do-skill (fn [entity component]
                        (let [old-posi (get-position entity)
                              posis (get-free-posis entity (get-position player-body) 2 2)]
                          (when (not-empty posis)
                            (let [posi (rand-nth posis)]
                              (teleport entity posi)
                              (monsterteleport-animation posi)
                              (create-line-render-effect posi old-posi 140 :color white)))))}))
; TODO shoot-sound?


(defmonster burrower {:hp 2.5 :armor 20 :pxsize 15}
  (default-death-trigger)
  (path-to-player-movement 30)
  (rotation-component)
  (image-render-component (monsterimage "burrower.png"))
  (normal-monster-melee)
  (game.components.burrow/burrow-component))

(defmonster ranged {:hp 0.6 :armor 5 :pxsize 15}
  (default-death-trigger)
  (ranged-runaway-movement-comp 24 (rand-int-between 2 6) :ground)
  (rotation-component)
  (image-render-component (monsterimage "core_raider.png"))
  (ranged-component :cooldown (rand-int-between 2000 2500) :attacktime 500))

(defmonster shield-turret {:hp 1.5 :armor 15 :pxsize 15}
  (path-to-player-movement 13)
  (default-death-trigger)
  (shield-component 1500)
  (rotation-component)
  (image-render-component (monsterimage "counternegativeenergyturret.png"))
  (ranged-component :cooldown 4000 :attacktime 500))

(defmonster mage-skull {:hp 4.8 :armor 0 :pxsize 15}
  (default-death-trigger)
  (ranged-runaway-movement-comp 26 (rand-int-between 3 4) :ground)
  (hp-regen-component 5)
  (rotation-component)
  (image-render-component (monsterimage "core_predator.png"))
  (ranged-component :cooldown 3000 :attacktime 50))

(let [attacktime 600
      folder (monsterresrc "fly/")]
  (defmonster fly {:hp 1 :armor 0 :pxsize 9}
    (default-death-trigger)
    (ranged-randomly-moving-comp 32 1000 :ground)
    (animation-component (fn [body]
                           (if (is-attacking? (get-component body :ranged)) :attack :default))
                         {:attack (folder-animation :folder folder :prefix "attack" :duration attacktime :looping false)
                          :default (create-animation [(create-image (str folder "fly.png"))])})
    (ranged-component :cooldown 2000 :state-blocks {}))) ; move+shoot

;; Healer

(def ^:private heal-radius 6)

(defn- get-healable-monsters-around [healer]
  (remove #(= % healer)
          (get-destructible-bodies (get-position healer) heal-radius :monster)))

(defn- monsters-need-healing? [healer]
  (some #(lower-than-max? (get-hp %))
        (get-healable-monsters-around healer)))

(defn- choose-active-healer [entity skillmanager delta]
  (let [skills (:skills skillmanager)
        heal-spell (:healing skills)
        ranged-weapon (:ranged skills)]
    (if (and (update-counter! entity delta skillmanager)
             (enough-mana? heal-spell skillmanager)
             (is-ready? heal-spell)
             (monsters-need-healing? entity)
             (zero? (rand-int 5)))
      :healing
      :ranged)))

(defn- heal-nearby-monsters
  "returns the healed monsters"
  [healer]
  (doall (remove nil?
                 (map #(when (lower-than-max? (get-hp %))
                         (set-hp-to-max %) %)
                      (get-healable-monsters-around healer)))))

(defn create-healer-skillmanager []
  (let [ranged-weapon (skillmanager-skill
                        :stype :ranged
                        :cooldown 4000
                        :attacktime 400
                        :cost 0
                        (redball-projectile-skill-props))
        heal-spell (skillmanager-skill
                     :stype :healing
                     :cooldown (rand-int-between 2000 3000)
                     :attacktime 500
                     :cost 0
                     {:shoot-sound "bfxr_healmonsters.wav"
                      :do-skill (fn [healer component]
                                  (let [healed-monsters (heal-nearby-monsters healer)]
                                    (create-lines-render-effect healer healed-monsters 500)))})]
    (skillmanager-component
      :rotatefn rotate-to-player
      :choosefn choose-active-healer
      :mana 0
      :skills [heal-spell ranged-weapon]
      (blocks-component {:attacking :movement})
      ; heilen trotzdem alle auf 1mal da 1sek attacktime?
      {:counter (make-counter 500)})))

; TODO also use cached-monsters-around ?
(defmonster healer {:hp 3 :armor 0 :pxsize 15}
  (default-death-trigger)
  (ranged-runaway-movement-comp 30 (rand-int-between 1 3) :ground)
  (rotation-component)
  (single-animation-component
    (folder-animation :folder (monsterresrc "healer/") :duration 1000 :looping true))
  (create-healer-skillmanager))

;; Instant-Healer

(defcomponent :cache-nearby-monsters []
  {:counter (make-counter 1000)}
  (active [delta {:keys [counter] :as c} entity]
    (when (update-counter! entity delta c)
      (assoc-in! entity [(:type c) :nearby-monsters]
                 (doall (get-healable-monsters-around entity))))))

; TODO not checking if ray-blocked ---> can heal through walls like other healer
; navigation meshes would make ray-blocked much simpler if in the same polygon/area
(defn- healing-required-and-allowed? [entity healer radius-squared]
  (and (exists? entity)
       (not (is-dead? entity))
       (lower-than-max? (get-hp entity))
       (bodies-in-range? entity healer radius-squared)))

; TODO mehr hervorheben anstatt drawLine vlt so ein "beam" strahl... gr�n mit weiss als kontrast (not in prototype stage -> do another time!)
; rotating to which monster is healed?
(let [healradius-squared (* heal-radius heal-radius)]
  (defmonster instant-healer {:hp 2 :armor 20 :pxsize 14}
    (default-death-trigger)
    (path-to-player-movement 15)
    (rotation-component)
    (image-render-component (monsterimage "coreturret.png"))
    (cache-nearby-monsters-component)
    (monster-melee-component :cooldown 500
                             :attacktime 100
                             :hit-sound (create-sound "slash.wav")
                             :target-id (get-id player-body))
    (standalone-skill
      :stype :instantheal
      :cooldown 1000
      :attacktime 150
      :props {:shoot-sound "bfxr_instanthealer_heal.wav"
              :check-usable (fn [entity _]
                              (when-let [cached (:nearby-monsters (get-component entity :cache-nearby-monsters))]
                                (when-let [needs-heal (first (sort-by #(:current (get-hp %))
                                                                      (filter #(healing-required-and-allowed? % entity healradius-squared)
                                                                              cached)))]
                                  (assoc-in! entity [:instantheal :needs-heal] needs-heal)
                                  true)))
              :do-skill (fn [healer {needs-heal :needs-heal :as component}]
                          (when (healing-required-and-allowed? needs-heal healer healradius-squared)
                            (set-hp-to-max needs-heal)
                            (create-line-render-effect (get-position healer) (get-position needs-heal) 200 :color green)))})))


;; Nova-Melee

(def monster-nova-radius 4)

(defn player-in-nova-range? [monster]
  (circle-collides? (get-position monster) monster-nova-radius player-body))

(defn- choose-active-nova-melee [entity skillmanager delta]
  (let [skills (:skills skillmanager)
        melee (:melee skills)
        nova (:monster-nova skills)]
    (if
      (and
        (enough-mana? nova skillmanager)
        (is-ready? nova)
        (player-in-nova-range? entity)
        (not (ray-blocked? (get-position entity) (get-position player-body)))
        (zero? (rand-int 240)))
      :monster-nova
      :melee)))

(defpreload ^:private monster-nova-frames (folder-frames "effects/monsternova/"))

(defn create-nova-melee-skillmanager []
  (let [melee-weapon (skillmanager-skill
                       :stype :melee
                       :cooldown 1000
                       :attacktime 500
                       :cost 0
                       (monster-melee-props (get-id player-body) (melee-weapon [3 8] (create-sound "slash.wav"))))
        monster-nova (skillmanager-skill
                       :stype :monster-nova
                       :cooldown (rand-int-between 1000 2000)
                       :attacktime 1500
                       :cost 1
                       {:shoot-sound "bfxr_monstercast.wav"
                        :do-skill (fn [entity component]
                                    (nova-effect
                                      :position (get-position entity)
                                      :duration 400
                                      :maxradius monster-nova-radius
                                      :affects-side :player
                                      :dmg [18 22]
                                      :animation (create-animation monster-nova-frames)))})]
    (skillmanager-component
      :choosefn choose-active-nova-melee
      :mana (rand-int-between 2 4)
      :skills [melee-weapon monster-nova]
      (blocks-component {:attacking :movement}))))

(defmonster nova-melee {:hp 2.2 :armor 7 :pxsize 15}
  (default-death-trigger)
  (path-to-player-movement 32)
  (single-animation-component
    (folder-animation :folder (monsterresrc "gravturret/") :duration 1000 :looping true))
  (create-nova-melee-skillmanager))

;;


(defmonster slowdown-caster {:hp 2 :armor 25 :pxsize 12}
  (default-death-trigger)
  (path-to-player-movement 20)
  (single-animation-component
    (folder-animation :folder (monsterresrc "slowdowncaster/") :duration 200 :looping true))
  (standalone-skill        ; TODO f�r nen ranged skill mit custom hit-effects & movement sehr kompliziert!!
    :stype :slowdown-ranged
    :cooldown 2000
    :attacktime 1000
    :props {:show-cast-bar true
            :do-skill (fn [entity ranged-comp]
                        (let [speed 84
                              rotation-speed 0.1
                              starting-angle (get-angle-to-position (get-position entity) (get-position player-body))]
                          (fire-projectile
                            :startbody entity
                            :px-size 10
                            :animation (folder-animation :folder "effects/slowdownprojectile/" :duration 700 :looping true)
                            :side :monster
                            :hits-side :player
                            :movement (create-homing-movement speed player-body starting-angle rotation-speed :air)
                            :hit-effects [(dmg-effect [5 6])
                                          (slowdown-effect 1)]
                            :maxtime 16000)))}))

;TODO ray-blocked = in-sight -> cache it? expensive?
;TODO do-skill spritesheet-frames expensive without preloading the frames?
(let [maxrange-squared (* 10 10)
      attacktime 1000]
  (defmonster ray-shooter {:hp 1 :armor 25 :pxsize 15}
    (default-death-trigger)
    (path-to-player-movement 22)
    (create-comp :dealt-dmg-trigger {:do teleport-and-heal-when-low-hp})
    (single-animation-component
      (folder-animation :folder (monsterresrc "harvesterexoshield/") :duration 200 :looping true))
    (standalone-skill
      :stype :rayshoot
      :cooldown (rand-int-between 1700 2300)
      :attacktime attacktime
      :state-blocks {:attacking :movement} ; important because line rendered from these posis! so dont move when attacking!
      :props {:show-cast-bar false
              :shoot-sound "bfxr_rayshooterhit.wav"
              :target-posi (atom nil) ; REMOVE
              :check-usable (fn [entity component]
                              (let [shooter-posi (get-position entity)
                                    target-posi (get-position player-body)]
                                (when (and (in-range? shooter-posi target-posi maxrange-squared)
                                           (not (ray-blocked? shooter-posi target-posi)))
                                  (reset! (:target-posi component) target-posi)
                                  (play-sound "bfxr_powerup.wav") ; length of sound ~ length of attacktime would be nice
                                  ; TODO line render only as long as monster is alive would also make more sense ? ...
                                  ; also when slowed down ... attacktime changes ...
                                  ; => just like a component of an entity slowed down/lives with it
                                  (create-line-render-effect shooter-posi target-posi attacktime :color white :thin true)
                                  true)))
              :do-skill (fn [entity component]
                          (let [target @(:target-posi component)]
                            (if (some is-player? (get-bodies-at-position target))
                              (deal-dmg [10 15] player-body)
                              (animation-entity :position target
                                                :animation (create-animation (spritesheet-frames "effects/12_16_littleexpl.png" 12 16) :frame-duration 100)
                                                :order :on-ground))
                            (create-line-render-effect (get-position entity) target 70 :color red)))})))
;;

(defn- rand-spawn-monster [monstertype position areahw areahh]
  (let [{:keys [half-w half-h]} (get-monster-properties monstertype)
        spawn-positions (remove #(blocked-location? % half-w half-h :ground) ; get-free-posis duplicate?
                                (map translate-to-tile-middle
                                     (get-touched-tiles position areahw areahh)))]
    (when (seq spawn-positions)
      (let [target (rand-nth spawn-positions)]
        (wake-up (try-spawn target monstertype)) ; TODO try-spawn also checks if blocked °_°
        (monsterteleport-animation target)
        (create-line-render-effect position target 70 :color white)))))

(require '[game.components.ingame-loop :refer [ingame-loop-comp]])

(comment
  (let [mlist [:littlespider :xploding-drone :mine :skull-chainsaw :armored-skull :armored-skull2 :ranged :shield-turret :mage-skull :healer :nova-melee :slowdown-caster]]
  (ingame-loop-comp :randspawn
    {:counter (make-counter 10000)}
    (active [delta c entity]
      (when (and (not= @current-map :town)
                 (update-counter! entity delta c))
        (play-sound "bfxr_monstercast.wav")
        (rand-spawn-monster mlist
                            (get-position player-body)
                            half-display-w-in-tiles
                            half-display-h-in-tiles
                            :nospawn (rand-int-between 1 9))
        (assoc-in! entity [(:type c) :counter :maxcnt] (rand-int-between 1000 20000)))))))

; (remove-entity :randspawn)

(defn- fire-boss-ranged-projectile [body speed starting-angle rotation-speed effects]
  (fire-projectile
    :startbody body
    :px-size 10
    :animation (folder-animation :folder "effects/bossball/" :duration 700 :looping true)
    :side :monster
    :hits-side :player
    :movement (create-homing-movement speed player-body starting-angle rotation-speed :air)
    :hit-effects effects
    :maxtime 12000))

(defpreload ^:private boss-explosion (folder-frames "effects/bossexplosion/"))

(defmonster first-boss {:hp 20 :armor 50 :pxw 33 :pxh 63}
  (big-body-hit-effect [[12 -15] [-8 -15] [0 -23] [1 -6] [-9 -4] [-8 4] [8 8] [-8 15] [3 23] [-6 25]])
  ;(light-component :color (rgbcolor :r 0.8 :g 0.2 :b 0.2) :intensity 1 :radius 12)
  (death-trigger (fn [body]
                   (play-sound "bfxr_bossdeath.wav")
                   (animation-entity
                     :animation (create-animation boss-explosion)
                     :position (get-position body))
                   (create-item-body (get-position body) "The Golden Banana")

                   ; no lvl after this => no need to spawn an item!
                   ; (create-rand-item (get-position body) :max-lvl (:rand-item-max-lvl (get-current-map-data)))

                   (runmap add-to-removelist (:projectiles (get-component body :boss-ranged)))))
  (standalone-skill
    :stype :monster-spawner
    :cooldown 2000
    :attacktime 500
    :props {:show-cast-bar true
            :shoot-sound "bfxr_monstercast.wav"
            :do-skill (fn [entity component]
                        (rand-spawn-monster :little-bot (get-position entity) 6 3))})
  (standalone-skill
    :stype :boss-ranged
    :cooldown 3200
    :attacktime 3000
    :props {:show-cast-bar true
            :projectiles []
            :dmg [5 15]
            :shoot-sound "bfxr_monstercast.wav"
            :do-skill (fn [entity ranged-comp]
                        (let [speed 48 ; ca. player move speed
                              rotation-speed 0.05
                              effects [(dmg-effect (:dmg ranged-comp)) (stun-collision-effect 75 300)]]
                          (update-in! entity [:boss-ranged :projectiles] concat
                                      (doall
                                        (map #(fire-boss-ranged-projectile entity speed % rotation-speed effects)
                                             [0 90 180 270])))))})
  (movement-component ; TODO komische args ... mach mit defnks?!
    {:control-update (fn [body _ _] (get-vector-to-player body))}
    12
    :ground)
  (single-animation-component ; TODO gleich folder-animation auchnoch reinpacken in single-animation-component?
    (folder-animation :folder (monsterresrc "boss/") :duration 300 :looping true)))

(defmonster test-hunter {:hp 1.5 :armor 7 :pxsize 47}
  (path-to-player-movement 72)
  (rotation-component)
  (image-render-component (monsterimage "coredemonhand.png")))

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
