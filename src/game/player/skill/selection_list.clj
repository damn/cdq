(ns game.player.skill.selection-list
  (:require
    [engine.core :refer [initialize]]
    [game.components.render :refer [rendering]]
    [game.components.ingame-loop :refer [ingame-loop-comp]]
    [game.player.skill.learnable :refer [render-skillbutton-tooltip]]
        [engine.render :refer [fill-rect render-readable-text rgbcolor]]
    [game.settings :refer [screen-height]]
    [game.gui :refer [get-absolute-posi get-bounds is-visible? make-imgbutton mouseover? remove-guicomponent set-visible]]
    [game.ingame-gui :refer [ingamestate-display]]
    [game.session :refer [atom-session]]
    [game.components.core :refer [active]]
    [game.components.skills.core :refer [is-cooling-down?]]
    [game.player.skill.skillmanager :refer [create-skill-icon get-selected-skill get-skills-for-mousebutton mousebuttons set-selected-skill]]))

(declare selected-skill-buttons skill-selection-hotkeys)

(intern 'game.player.skill.skillmanager
        'change-selected-skill-button-images
  (fn [mousebutton new-icon]
    (swap! (get selected-skill-buttons mousebutton) assoc :image new-icon)))

(def ^:private select-skills-list {})

(declare create-skills-list button-y)

(def ^:private xleftselectedskillbutton 2)
(def ^:private xrightselectedskillbutton 20)

(defn- set-visible-skills-list [mousebutton bool]
  (when bool
    (create-skills-list mousebutton (case mousebutton
                                      :left xleftselectedskillbutton
                                      :right xrightselectedskillbutton)
                        button-y))
  (doseq [{:keys [button]} (mousebutton select-skills-list)]
    (set-visible button bool)))

(defn- is-visible-skills-list? [mousebutton]
  (when-let [buttons (mousebutton select-skills-list)]
    (-> buttons first :button is-visible?)))

(defn- render-button-hotkeys [g mousebutton]
  (when (is-visible-skills-list? mousebutton)
    (doseq [{:keys [button skill]} (mousebutton select-skills-list)]
      (when-let [{:keys [mouse digit] :as hotkey}
                 (get @skill-selection-hotkeys (:type skill))]
        (when (= mousebutton mouse)
          (let [[x y] (get-absolute-posi button)]
            (render-readable-text g x y digit)))))))

(defn- render-button-tooltip [g mousebutton] ;TODO rename skillbutton...
  (when (is-visible-skills-list? mousebutton)
    (dorun (map
      #(render-skillbutton-tooltip g %)
      (mousebutton select-skills-list)))))

(let [transparent-red (rgbcolor :r 0.9 :a 0.7)]

  (defn- render-button-cooldown-overlay [g mouse]
    (when (is-cooling-down? (get-selected-skill mouse))
      (fill-rect g (get-bounds (get selected-skill-buttons mouse)) transparent-red))))

(defn some-skill-selection-list-visible? [] (some is-visible-skills-list? mousebuttons))
(defn close-skill-selection-lists [] (dorun (map #(set-visible-skills-list % false) mousebuttons)))

(ingame-loop-comp :update-skill-selection-buttons
  (rendering [g c]
    (doseq [button mousebuttons]
      (render-button-cooldown-overlay g button)
      (render-button-hotkeys g button))))

(ingame-loop-comp :skill-selection-list-tooltips
  (rendering :tooltips [g c]
    (dorun (map #(render-button-tooltip g %) mousebuttons))))

(defn- create-selected-skill-button [mousebutton x]
  (let [button (make-imgbutton :image (create-skill-icon "icons/stomp.png") ; this icon will be replaced during create-player-skillmanager and the correct icon for each mouse
                               :location [x (- screen-height 18)]
                               :pressed #(set-visible-skills-list mousebutton true)
                               :tooltip (str "Select " (name mousebutton) "-mouse skill")
                               :parent ingamestate-display)
        [x y w h] (get-bounds button)]
    (def ^:private button-height h)
    (def ^:private button-y y)
    button))

(initialize
  (def ^:private selected-skill-buttons
    {:left (create-selected-skill-button :left xleftselectedskillbutton)
     :right (create-selected-skill-button :right xrightselectedskillbutton)}))

(defn- create-skills-list [mousebutton buttonx buttony]
  (doseq [button (map :button (mousebutton select-skills-list))]
    (remove-guicomponent ingamestate-display button))
  (alter-var-root #'select-skills-list assoc-in [mousebutton]
                  (let [puffer 5
                        starty (- buttony button-height puffer)]
                    (map-indexed
                      (fn [idx skill]
                        {:skill skill
                         :button (make-imgbutton
                                   :image (:icon skill)
                                   :location [buttonx
                                              (- starty (* idx (inc button-height)))]
                                   :pressed (fn []
                                              (set-visible-skills-list mousebutton false)
                                              (set-selected-skill mousebutton skill))
                                   :parent ingamestate-display)})
                      (get-skills-for-mousebutton mousebutton)))))

;; Hotkeys

(def ^:private skill-selection-hotkeys (atom {}))
; for example {:default-hands {:digit 1 :mouse :left}}

(def hotkeys-session (atom-session skill-selection-hotkeys :save-session true))

(defn- get-skilltype-with-hotkey [digit]
  (first (filter
    #(= (:digit (get @skill-selection-hotkeys %)) digit)
    (keys @skill-selection-hotkeys))))

(defn- set-skill-hotkey [skilltype digit mousebutton]
  {:pre [(not (get-skilltype-with-hotkey digit))]}
  (swap! skill-selection-hotkeys assoc skilltype {:digit digit :mouse mousebutton}))

(defn- try-set-skill-hotkey [mousebutton digit]
  (when (is-visible-skills-list? mousebutton)
    (when-let [selected-skill-button (first (filter #(mouseover? @(:button %)) (mousebutton select-skills-list)))]
      (when-let [same-digit-hotkeyed-skilltype (get-skilltype-with-hotkey digit)]
        (swap! skill-selection-hotkeys dissoc same-digit-hotkeyed-skilltype))
      (set-skill-hotkey (:type (:skill selected-skill-button)) digit mousebutton))))

(defn- update-skill-hotkeys [digit]
  (cond
    (try-set-skill-hotkey :left digit) nil
    (try-set-skill-hotkey :right digit) nil
    :else
    (when-let [skilltype (get-skilltype-with-hotkey digit)]
      (let [mousebutton (:mouse (get @skill-selection-hotkeys skilltype))]
        (when-let [skill (first (filter #(= (:type %) skilltype)
                                     (get-skills-for-mousebutton mousebutton)))]
          (set-selected-skill mousebutton skill)
          (set-visible-skills-list mousebutton false))))))

(def ^:private numberkeys (map #(keyword (str %)) (range 10)))

(ingame-loop-comp :up-skill-hotkeys
    (active [delta c _]
      (doseq [k numberkeys
              :when (engine.input/is-key-pressed? k)]
        (update-skill-hotkeys (Integer/parseInt (name k))))))

(intern 'game.player.skill.skillmanager 'assign-unused-hotkey-and-open-skillslist
  (fn [skilltype]
    (let [unused-hotkey (first (filter #(not (get-skilltype-with-hotkey %)) [1,2,3,4,5,6,7,8,9,0]))
          mousebutton (if (= skilltype :psi-charger) :left :right)]
      (set-skill-hotkey skilltype unused-hotkey mousebutton)
      (set-visible-skills-list mousebutton true))))


