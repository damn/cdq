(ns game.player.skill.learnable
  (:require clojure.set)
  (:use
    utils.core
    (engine
      [core :only (initialize)]
      render)
    (game settings media ingame-gui session)
    game.player.skill.skillmanager
    (game.components core
                     [render :only (rendering)]
                     [ingame-loop :only (ingame-loop-comp)])
    (game.components.skills core melee)))

; This ns is skillmenu & learnable-skills

(def- skillmenu-x frame-screenborder-distance)
(def- skillmenu-y frame-screenborder-distance)

(defn learn-skill [skill]
  ; TODO assert not learned yet?
  (reset! (:learned-this skill) true)
  (update-in! player-body [:skillmanager :skills] add-player-skill skill))
; TODO add-player-skill doch net pure machen? da assoc in player-body egtl zu add-player-skill geh�rt?

; TODO jede menu-posi; skill-type nur 1x erlaubt?
(defn learnable-skill
  "Accepts a sequence of :key vals and then any number of additional props maps."
  [skill-type & more]
  (let [[{:keys [menu-posi manacost mousebutton icon info cooldown]
          :or {cooldown 0}
          :as m}
         props] (split-key-val-and-maps more)]
    (assert (clojure.set/subset? #{:menu-posi :mousebutton :icon :info} (.keySet m)))
    (skillmanager-skill
      :stype skill-type
      :cooldown cooldown
      :cost manacost
      {:learned-this (atom false) ; new or load character overwrites this
       :menu-posi menu-posi
       :mousebutton mousebutton
       :icon (create-skill-icon icon)
       :info info}
      props)))

(def- init-fns (atom []))

(defmacro deflearnable-skill [skill-type & more]
  `(swap! (deref #'game.player.skill.learnable/init-fns)
          conj
          (fn [] (learnable-skill ~(keyword skill-type) ~@more))))

(defn- init-learnable-skills []
  (def learnable-skills
    (into {} (for [skill (map #(%) @init-fns)]
               [(:type skill) skill]))))

(initialize
  (def- skillmenu-frame (make-frame :name :skillmenu
                                    :bounds [skillmenu-x skillmenu-y 100 95]
                                    :hotkey skillmenu-hotkey
                                    :visible false
                                    :parent ingamestate-display)))

(defn- init-skillmenu-buttons []
  (let [x 5
        y 20 ; 30 == fontheight?! wg. free-sk-pt str
        puffer 2]
    (def- skillmenu-buttons
      (for [{[menux menuy] :menu-posi :as skill} (vals learnable-skills)]
          {:skill skill
           :button (make-imgbutton :image (:icon skill)
                                   :location [(+ x puffer (* menux 25))
                                              (+ y puffer (* menuy 25))]
                                   :pressed (fn []
                                              (when (and (pos? (:free-skill-points (get-component player-body :skillmanager)))
                                                         (not @(:learned-this skill)))
                                                (update-in! player-body [:skillmanager :free-skill-points] dec)
                                                (learn-skill skill)
                                                (assign-unused-hotkey-and-open-skillslist (:type skill))))
                                   :parent skillmenu-frame)}))))

(initialize
  (init-learnable-skills)
  (init-skillmenu-buttons))

(defn- properties-str [props-map]
  (apply str (for [[k v] props-map]
               (if (= :dmg k)
                 (str "damage: " (variance-val-str v) "\n")
                 (str (name k) ": " (readable-number v) "\n")))))

(defcolor infostrcolor :r 0.3 :g 1 :b 0.6)

(defn- get-skillmenu-skill-info-textseq
  "for learnable skills and skillmanager skills with :info"
  [skill]
  [infostrcolor
   (:info skill)
   (rgbcolor :r 1 :g 1 :b 1)
   (when-let [info-keyseq (:show-info-for skill)]
     (properties-str (select-keys skill info-keyseq)))])

(defn render-skillbutton-tooltip [g {:keys [button skill]}]
  (when (mouseover? @button)
    (let [[x y w h] (get-bounds button)]
      (apply render-readable-text g x (+ y h)
             (get-skillmenu-skill-info-textseq skill)))))

(ingame-loop-comp :skillmenu
  (rendering [g c]
    (when (is-visible? skillmenu-frame)
      (let [x skillmenu-x
            y skillmenu-y]
        (render-readable-text g (+ x 5) (+ y 5) :background false (str "Free points: " (:free-skill-points (get-component player-body :skillmanager))))
        (doseq [{:keys [button skill]} skillmenu-buttons
                :when (not @(:learned-this skill))]
          (fill-rect g (get-bounds button) (rgbcolor :r 0.5 :g 0.5 :b 0.5 :a 0.8)))))))

; TODO use game.gui (add-tooltip ?) -> vlt. :above :below angebbar? und auch get-infostr funkt angebbar?..
(ingame-loop-comp :skill-tooltips
  (rendering :tooltips [g c]
    (when (is-visible? skillmenu-frame)
      (runmap #(render-skillbutton-tooltip g %) skillmenu-buttons))))

(def session (reify game.session/Session
               (load-session [_ {:keys [free-points learned]}]
                 (assoc-in! player-body [:skillmanager :free-skill-points] free-points)
                 (doseq [[skilltype learned-this] learned
                         :let [skill (get learnable-skills skilltype)]]
                   (if-not skill
                     (log "Savegame skill " skilltype " not known to engine.")
                     (if learned-this
                       (learn-skill skill)
                       (reset! (:learned-this skill) false))))) ; new character session all set to false
               (save-session [_]
                 {:free-points (:free-skill-points (get-component player-body :skillmanager))
                  :learned (for [[skilltype skill] learnable-skills]
                             [skilltype @(:learned-this skill)])})
               (new-session-data [_]
                 {:free-points 1
                  :learned (for [[skilltype skill] learnable-skills]
                             [skilltype false])})))

(initialize
  (def- freeskillpointsbutton
    (make-imgbutton
      :image (create-image "icons/skills_free.png" :scale buttonscale)
      :location [(+ buttonx-start (* x-dist -1)) (- screen-height 18)]
      :pressed #(switch-visible skillmenu-frame)
      :tooltip "You have free skill points"
      :parent ingamestate-display
      :visible false)))

(ingame-loop-comp :update-button-visibility
  (active [delta c _]
    (set-visible freeskillpointsbutton (pos? (:free-skill-points (get-component player-body :skillmanager))))))
