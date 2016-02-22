(ns game.player.skill.skillmanager
  (:use
    utils.core
    (engine input [render :only (create-image get-scaled-copy)])
    (game settings mouseoverbody)
    (game.components.skills core melee)
    (game.components core active body destructible misc body-effects)
    game.utils.geom))

(defn get-player-skills []
  (:skills (get-component player-body :skillmanager)))

(defn- current-selected-skill-type [mousebutton]
  (mousebutton (:selected-type (get-component player-body :skillmanager))))

(defn get-selected-skill [mousebutton]
  ((current-selected-skill-type mousebutton) (get-player-skills)))

(declare change-selected-skill-button-images
         assign-unused-hotkey-and-open-skillslist)

(def mousebuttons #{:left :right})

(defn set-selected-skill [mousebutton skill]
  {:pre [(mousebuttons mousebutton)]}
  (change-selected-skill-button-images mousebutton (:icon skill))
  (assoc-in! player-body [:skillmanager :selected-type mousebutton] (:type skill)))

(defn get-skills-for-mousebutton
  ([mousebutton]
    (get-skills-for-mousebutton (get-player-skills) mousebutton))
  ([skills mousebutton]
    (filter
      #(#{:both mousebutton} (:mousebutton %))
      (vals skills))))

(defeffectentity ^:private until-leftm-released-movement-block [body]
  :target body
  :dofn   #(swap! body add-block    :movement)
  :undofn #(swap! body remove-block :movement)
  (create-comp :notype
    (active [_ c entity]
      (when-not (is-leftbutton-down?)
        (add-to-removelist entity)))))

(defn try-set-active-skill-player-skillmanager [new-skill]
  (let [skillmanager (get-component player-body :skillmanager)]
    (if
      (and
        (:is-melee new-skill)
        (not (enough-mana? new-skill skillmanager)))
      :default-hands
      (:type new-skill))))

(defn- choose-selected-skill [entity skillmanager delta]
  (cond
    (let [mouseover-body (get-mouseover-body)]
      (and
        (is-leftbutton-down?)
        mouseover-body
        (attackable-by-player? mouseover-body)))
    (let [selected-left (get-selected-skill :left)]
      (when
        (and
          (not (:is-melee selected-left))
          (empty? (get-certain-effect-entities entity
                                               :until-leftm-released-movement-block)))
        (until-leftm-released-movement-block entity))
      (try-set-active-skill-player-skillmanager selected-left))

    (and (not (is-rightm-consumed?)) (is-rightbutton-down?))
    (try-set-active-skill-player-skillmanager (get-selected-skill :right))

    :else
    nil))

(defn is-selected-leftmouse? [skill] (= skill (get-selected-skill :left)))

(defn is-selected-rightmouse? [skill] (= skill (get-selected-skill :right)))

; TODO schau ob der neue skill �berhaupt auf leftmouse / rightmouse passt.
; oder default skills m�ssen immer left sein? -> waffen k�nnen ja auch :right
(defn- assoc-new-skill-at-index [skills new-skill idx]
  (let [skill-at-idx (get skills idx)]
    (when (is-selected-leftmouse? skill-at-idx)
      (set-selected-skill :left new-skill))
    (when (is-selected-rightmouse? skill-at-idx)
      (set-selected-skill :right new-skill))
    (assoc skills idx new-skill)))

(defn get-current-standard-skill [] (:default-hands (get-player-skills)))

(defn change-standard-attack-skill [skillmanager new-skill]
  (assoc skillmanager :skills
    (assoc-new-skill-at-index (:skills skillmanager) new-skill :default-hands)))

(defn reset-standard-attack-skill [skillmanager]
  (change-standard-attack-skill
    skillmanager
    (:default-hands skillmanager)))

; TODO type von player-skills m�ssen unique sein (item, leanrable,starting) wg hotkeys unter anderem
(defn add-player-skill [skills new-skill]
  (assoc skills (:type new-skill) new-skill))

(defn remove-player-skill [skills skill]
  (let [new-skills (dissoc skills (:type skill))]
    (if (is-selected-leftmouse? skill)
      (set-selected-skill
        :left
        (first
          (get-skills-for-mousebutton new-skills :left))))
    (if (is-selected-rightmouse? skill)
      (set-selected-skill
        :right
        (first
          (get-skills-for-mousebutton new-skills :right))))
    new-skills))

(defn- player-skillmanager-rotate [body]
  (if @saved-mouseover-body
    (rotate-to-body body @saved-mouseover-body)
    (rotate-to-mouse body)))

(defn create-skill-icon [data]
  (create-image data :scale [16 16]))

(defn create-player-skillmanager []
  (let [default-hands (skillmanager-skill :stype :default-hands
                                          :cooldown 0
                                          :cost 0
                                          (player-melee-props [])
                                          {:mousebutton :both
                                           :icon (create-skill-icon "icons/melee.png")
                                           :info "Standard melee attack."
                                           :show-info-for [:cost]})]
    ; same as set-selected-skill, also setting :selected-...-type here
    ; (but when this component is created the player-body doesnt exist yet)
    (change-selected-skill-button-images :left  (:icon default-hands))
    (change-selected-skill-button-images :right (:icon default-hands))
    (skillmanager-component
      :rotatefn player-skillmanager-rotate
      :choosefn choose-selected-skill
      :mana 100
      :skills [default-hands]
      :cast-sound "bfxr_cast.wav"
      (blocks-component {:attacking :movement})
      ; here and @ :skills a :default-hands skill; the one in :skills will be overwritten
      ; in case of a new default-hands skill
      {:default-hands       default-hands
       :selected-type {:left  (:type default-hands)
                       :right (:type default-hands)}})))
; default-hands skill exists two times as a copy ... another way to do it? maybe mark the skill that is the default hands as :default-hands true
; and all others as :false
