(ns game.start
  (:import (java.awt GridLayout Dimension)
           (javax.swing JPanel JFrame JButton ButtonGroup JRadioButton)
           (java.awt.event KeyEvent ActionListener)
           (org.newdawn.slick Image Color))
  (:require
    [engine.render.image :as g]
    [engine.render.color :as color]
    [engine.statebasedgame :refer [init-state-based-game state-based-game]]
    [game.session :as sess]
    [engine.settings :refer [jar-file?]]
    [utils.core :as utils]
    [engine.core :refer [create-and-start-app-game-container fullscreen-supported?]]
    [game.settings :refer [get-setting init-config! screen-height screen-scale screen-width]]
    game.components.body-render
    game.components.burrow
    game.maps.add
    game.media
    game.entity.teleporters
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
    [game.state.deferred-preload :refer [deferred-preload-gamestate]]
    [game.state.mainmenu :refer [mainmenu-gamestate]]
    [game.state.loading :refer [loading-gamestate]]
    [game.state.ingame :refer [ingame-gamestate]]
    [game.state.options :refer [options-gamestate]]
    [game.state.minimap :refer [minimap-gamestate]])
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
  (color/rgbcolor :r r :g g :b b :a a))

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
