(ns game.components.skills.core
  (:require [engine.core :refer [make-counter update-finally-merge reset]])
  (:use
    utils.core
    (engine core render [input :only (get-mouse-pos is-rightbutton-down? is-leftbutton-down?)])
    (game.utils tilemap msg-to-player)
    (game.components core active render)))

(defn is-ready?        [{state :state}] (= state :ready))
(defn is-attacking?    [{state :state}] (= state :attacking))
(defn is-cooling-down? [{state :state}] (= state :cooldown))

(defn get-skill [body skilltype]
  (or
    (get-component body skilltype)
    (-> (get-component body :skillmanager) :skills skilltype)))

(defn get-mana [entity] (:mana (get-component entity :skillmanager)))

(defn enough-mana? [{cost :cost :as skill} {mana :mana :as skillmanager}]
  (<= cost (:current mana)))

(defn set-mana-to-max [body]
  (when (lower-than-max? (get-mana body))
    (show-gains-mana-effect body (rest-to-max (get-mana body))))
  (update-in! body [:skillmanager :mana] set-to-max))

(defn- update-cooldown [skill delta]
  (if (is-cooling-down? skill)
    (update-finally-merge skill :cooldown-counter delta
                          {:state :ready})
    skill))

(defn- update-cooldowns [skillmanager delta]
  (update-in skillmanager [:skills] mapvals #(update-cooldown % delta)))

(defn reset-skills [skills]
  (mapvals skills
           #(-> %
                (update-in [:cooldown-counter] reset)
                (assoc :state :ready))))

; Warning:
; entities which attacktime is driven through their animation and .isStopped
; check they dont have an :attack-counter!
; and entities which attack through :attack-counter and have :animation component are ok because first :Attack-counter is checked
(defn- attack-finished? [entity skill delta]
  (if (:attack-counter skill)
    (update-counter! entity delta skill :attack-counter)
    (is-stopped? (current-animation entity))))

(defn- do-skill-effect [entity {:keys [do-skill] :as skill}]
  (do-skill entity skill))

(defn- try-attack [entity skill delta]
  (when (attack-finished? entity skill delta)
    (do-skill-effect entity skill)
    (when-let [s (:shoot-sound skill)]
      (play-sound s))
    true))

(defn is-usable? [entity skill]
  (if-let [check-usable (:check-usable skill)]
    (check-usable entity skill)
    true))

(defn get-active-skill [{:keys [active-type skills] :as skillmanager}]
  (get skills active-type))

(let [props (atom nil)]

  (defn- save-casting-mouse-state []
    (reset! props {:position (get-mouse-pos)
                   :tile-position (get-mouse-tile-pos)
                   :button (cond
                             (is-leftbutton-down?) :left
                             (is-rightbutton-down?) :right)}))

  (defn get-skill-use-mouse-pos [] (:position @props))
  (defn get-skill-use-mouse-tile-pos [] (:tile-position @props))
  (defn get-skill-use-mouse-button [] (:button @props)))

(defn- pay-cost [component cost]
  (update-in component [:mana :current] - cost))

(defn- try-set-active-skill-cooldown
  "Check contains because grenade may remove skill from skillmanager and assoc-in does assoc
   all keys that are not there, which would lead to a skill with just {:state :cooldown}"
  [{active-type :active-type :as skillmanager}]
  (if (contains? (:skills skillmanager) active-type)
    (assoc-in skillmanager [:skills active-type :state] :cooldown)
    skillmanager))

(def- deniedsound (create-sound "bfxr_denied.wav"))

; skillmanager:        READY <-> ATTACKING
; skillmanager skills: READY <-> COOLDOWN
(defn update-component-skillmanager
  [delta {:keys [state choose-skill rotate-after-attack cast-sound] :as component} entity]
  (update-in! entity [:skillmanager] update-cooldowns delta)
  (let [is-player (is-player? entity)
        skillmanager (get-component entity :skillmanager)] ; after update cooldowns.
    (case state
      :ready
      (let [active-type (choose-skill entity skillmanager delta)
            active-skill (get (:skills skillmanager) active-type)]
        (when
          (and
            active-skill
            (if is-player (save-casting-mouse-state) true)
            (is-ready? active-skill)
            (is-usable? entity active-skill)
            (let [enough (enough-mana? active-skill skillmanager)]
              (if (and (not enough) is-player)
                (do
                  (playonce deniedsound)
                  (show-msg-to-player "Not enough mana!")))
              enough))
          (when (and cast-sound (not (:is-melee active-skill)))
            (play-sound cast-sound))
          (->! entity
            (assoc-in [:skillmanager :active-type] active-type)
            (update-in [:skillmanager] pay-cost (:cost active-skill))
            (switch-state :skillmanager :attacking))
          (when rotate-after-attack
            (rotate-after-attack entity))))
      :attacking
      (when (try-attack entity (get-active-skill skillmanager) delta)
        (->! entity
          (update-in [:skillmanager] try-set-active-skill-cooldown)
          (switch-state :skillmanager :ready))))))

(defn skillmanager-component
  "rotatefn may be nil"
  [& args]
  (let [[{:keys [rotatefn choosefn mana skills cast-sound]} props] (split-key-val-and-maps args)]
    (create-comp :skillmanager
      (active update-component-skillmanager)
      {:rotate-after-attack rotatefn
       :choose-skill choosefn    ; must return not-nil if attack should be tried
       :mana (min-max-val mana)
       :skills (zipmap (map :type skills) skills)
       :state :ready
       :active-type nil
       :cast-sound cast-sound
       :block-effect-reset-state (fn [entity skillm]
                                   (when (is-attacking? skillm)
                                     (when (:attack-counter (get-active-skill skillm))
                                       (update-in! entity [:skillmanager :skills (:active-type skillm) :attack-counter] reset))
                                     (->! entity
                                          (assoc-in [:skillmanager :skills (:active-type skillm) :state] :cooldown)
                                          (switch-state :skillmanager :ready))))}
      props)))

(defn update-component-skill
  [delta {:keys [state] :as skill} entity]
  (when-let [new-state (case state
                         :ready (when (is-usable? entity skill)
                                  :attacking)
                         :attacking (when (try-attack entity skill delta)
                                      :cooldown)
                         :cooldown (when (update-counter! entity delta skill :cooldown-counter)
                                     :ready))]
    (swap! entity switch-state (:type skill) new-state)))

(defn- create-skill-props [cooldown attacktime]
  (merge
    {:state :ready
     :cooldown-counter (make-counter cooldown)}
    (when attacktime
      {:attack-counter (make-counter attacktime)})))

(defnks standalone-skill
  [:stype :cooldown :props :opt :attacktime :state-blocks]
  (create-comp stype
    (active update-component-skill)
    (if state-blocks (blocks-component state-blocks) {})    ; TODO allow nil and remove nil? @ create-comp?
    (create-skill-props cooldown attacktime)
    {:block-effect-reset-state (fn [entity skill]
                                 (when (is-attacking? skill)
                                   (when (:attack-counter skill)
                                     (update-in! entity [(:type skill) :attack-counter] reset))
                                   (swap! entity switch-state (:type skill) :cooldown)))}
    props))

; TODO player-skillmanager-skill mit : animation, mousebutton, icon
; (einfach das was ben�tigt WIRD und was optional ist bei allen item , skillmanager, learnable-skills)
(defn skillmanager-skill [& more]
  (let [[{:keys [stype cooldown cost attacktime]} props] (split-key-val-and-maps more)]
    ; assert has stype cooldown cost? obligatory keys
    ; assert all keys are stype cooldown cost or attacktime? all keys in obligatory or optional?
    (create-comp stype
      (create-skill-props cooldown attacktime)
      {:cost cost}
      props)))
