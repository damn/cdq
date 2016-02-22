(ns game.resolutionsetup
  (:use [engine.core :only (fullscreen-supported?)]
        [game.settings :only (screen-width screen-height)])
  (:import (java.awt GridLayout Dimension)
           (javax.swing JPanel JFrame JButton ButtonGroup JRadioButton)
           (java.awt.event KeyEvent ActionListener)))

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

(defn resolution-setup-frame [start-the-game]
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

