(ns game.components.destructible
  (:use
    utils.core
    (engine core render)
    game.screenshake
    (game.components core position misc body render [shield :only (shield-try-consume-damage)] body-effects sleeping)
    (game.utils random)
    game.components.skills.core))

(defeffectentity create-leech-visuals [body img]
  :target body
  :duration 150
  (image-render-component img :order :air))

(defn calc-effective-spell-dmg [[basemin basemax] percent-modify]
  {:pre [(number? basemin) (number? basemax)]}
  (let [modifier (/ percent-modify 100)]
    [(int (* modifier basemin))
     (int (* modifier basemax))]))

(defn calc-effective-melee-dmg [[basemin basemax] {:keys [min-inc max-inc percent-modify]}]
  {:pre [(number? basemin) (number? basemax)]}
  (let [new-min (+ basemin min-inc)
        new-max (+ basemax max-inc)
        modifier (/ percent-modify 100)]
    [(int (* modifier new-min))
     (int (* modifier (max new-min new-max)))]))

(defn- try-leech [dmg-dealt]
  (let [{:keys [hp-leech mana-leech]} (get-component player-body :item-boni)]

    (when-not (zero? hp-leech)
      (let [value (* (/ hp-leech 100) dmg-dealt)]
        (show-gains-hp-effect player-body value)
        (update-in! player-body [:destructible :hp] increase-min-max-val value)) ; same code as in hp pot
      (create-leech-visuals player-body (create-image "effects/hp_leech.png")))

    (when-not (zero? mana-leech)
      (let [value (* (/ mana-leech 100) dmg-dealt)]
        (show-gains-mana-effect player-body value)
        (update-in! player-body [:skillmanager :mana] increase-min-max-val value)) ; same code as in mana pot
      (create-leech-visuals player-body (create-image "effects/mana_leech.png")))))

;;

(def player-standard-dmg 10) ; entspricht 1.0 multiplier monster hp also 1 schlag tot (durchschnitt)

(def player-start-hp 100)

(def monster-standard-dmg 5) ; 20 default hits to kill player

(let [armor-point-avg-reduce 0.1]

  (defn- get-armor-variant-dmg-reduce [armor]
    (variance-val
      (* armor-point-avg-reduce armor)
      0.2))

  (defn get-armor-reduce-info [armor]
    (str "Damage reduced by "
      (variance-val-str
        (get-armor-variant-dmg-reduce armor))))

  (defn- get-monster-armor [perc-reduce]
    (int ; TODO wieso int hier und bei player-armor net?
      (*
        (/ perc-reduce 100)
        player-standard-dmg
        (/ 1 armor-point-avg-reduce))))

  (defn get-player-armor "Reduces perc of standard-monsterdmg"
    [perc-reduce]
    (*
      (/ perc-reduce 100)
      monster-standard-dmg
      (/ 1 armor-point-avg-reduce))))

; TODO depends body because destructible are applied body-hit-effects?
(defcomponent destructible [hp :armor]
  {:pre [(>= hp 0) (>= armor 0)]}
  {:hp (min-max-val hp)
   :init (fn [entity]
           ; hp-bar brightness corresponds to image corner brightness
           (assert (or
                     (get-component entity :image-render)
                     (get-component entity :animation))))})

(defn destructible? [body] (get-component body :destructible))

(defn deal-dmg-allowed? [body affected-side] ; TODO also the same @ get-destructible-bodies... -> side always only 1 ??
  (and (destructible? body)
       (= (get-side body) affected-side)
       (is-affectable? body)))

(defn attackable-by-player? [body]
  (deal-dmg-allowed? body :monster))

; TODO is-affectable ... why here? I want destructible bodies.,.. does too many things...
(defn get-destructible-bodies [position radius side]
  (let [affects-side (if (keyword? side) #{side} (set side))]
    (filter #(and (destructible? %)
                  (affects-side (get-side %))
                  (is-affectable? %))
      (get-touched-bodies position radius))))

(defn get-hp [body] (:hp (get-component body :destructible)))
(defn get-armor [body] (:armor (get-component body :destructible)))

(defn is-dead? [body] (:is-dead (get-component body :destructible)))

; TODO make 2 functions -> simpler? like this more complected
(defn set-hp-to-max [body]
  (when (lower-than-max? (get-hp body))
    (show-gains-hp-effect body (rest-to-max (get-hp body))))
  (update-in! body [:destructible :hp] set-to-max))

(defn monster-destructible [hp-multiplier armor-perc-reduce]
  (destructible-component
    (rand-float-between
      (variance-val (* hp-multiplier player-standard-dmg) 0.5))
    (get-monster-armor armor-perc-reduce)))

(defpreload ^:private explosion-frames (spritesheet-frames "effects/expsmall.png" 20 20))
(defpreload ^:private blood-frames (spritesheet-frames "effects/blood.png" 20 20))

(defn- create-hit-effect [body]
  (if-let [{trigger :trigger} (get-component body :hit-effect)]
    (trigger body)
    (animation-entity
      :position (get-position body)
      :animation (create-animation (if (is-player? body)
                                     blood-frames
                                     explosion-frames)))))

(defn- show-dmg-effect [body dmg]
  (show-string-effect body 1000 (rgbcolor :r 1) (str (int dmg) "!")))

(defn- dealt-dmg-triggers [target-body actual-dmg is-crit-hit? lethal is-player-melee]

  ; hit something else triggers
  (when is-player-melee
    ((:dealt-melee-dmg-effect (get-component player-body :item-boni)) target-body)
    (try-leech actual-dmg))

  ; being hit by something else triggers
  (when-let [{trigger :do} (get-component target-body :dealt-dmg-trigger)]
    (trigger target-body lethal))
  (when-not lethal
    (when (is-player? target-body)
      (shake-screen))
    (create-hit-effect target-body)
    (play-sound
      (cond (is-player? target-body) "bfxr_playerhit.wav"
            is-crit-hit? "kabloom.wav"
            :else "bfxr_normalhit.wav"))))

(defn- critical-hit-dmg [dmg target-body]
  (let [dmg (* dmg 3)]
    (show-dmg-effect target-body dmg) ; TODO belongs to dealt-dmg-triggers & unexpected side effect ?
    dmg))

(defn- dmg-data-ok? [{mi 0 mx 1 :as dmg}]
  (and (vector? dmg)
       (= (count dmg) 2)
       (integer? mi)
       (integer? mx)
       (>= mi 0)
       (>= mx 0)
       (>= mx mi)))

(defn- calc-deal-dmg-dmg [dmg is-player-melee is-player-spell target-body]
  (let [dmg (cond (dmg-data-ok? dmg) dmg
                  (number? dmg) [dmg dmg]
                  :else (println "Dmg-data notokay: " dmg))
        {:keys [melee-crit spell-crit percent-modify-spell] :as item-boni} (get-component player-body :item-boni)
        ; +percent% increases
        dmg (if is-player-spell (calc-effective-spell-dmg dmg percent-modify-spell) dmg)
        dmg (if is-player-melee (calc-effective-melee-dmg dmg item-boni) dmg)
        ; random dmg
        dmg (rand-int-between dmg)
        ; critical hit check
        is-melee-crit (and is-player-melee (percent-chance melee-crit))
        dmg (if is-melee-crit (critical-hit-dmg dmg target-body) dmg)
        is-spell-crit (and is-player-spell (percent-chance spell-crit))
        dmg (if is-spell-crit (critical-hit-dmg dmg target-body) dmg)]
    [dmg (or is-melee-crit is-spell-crit)]))

(defn- apply-armors [dmg target-body]
  (if (shield-try-consume-damage target-body)
    (play-sound "bfxr_shield_consume.wav")
    (let [rest-dmg (->> target-body get-armor get-armor-variant-dmg-reduce rand-float-between (- dmg) (max 0))]
      (if (and (not (zero? dmg)) (zero? rest-dmg))
        (play-sound "bfxr_armorhit.wav")
        rest-dmg))))

(defn deal-dmg [dmg target-body & {:keys [is-player-melee is-player-spell]}]
  {:pre [(destructible? target-body)]}
  (when-not (is-dead? target-body) ; kill only once, call triggers only once
    (wake-up target-body)  ; hit-trigger even if armor absorbs dmg
    (let [[dmg is-crit-hit?] (calc-deal-dmg-dmg dmg is-player-melee is-player-spell target-body)]
      (when-let [dmg (apply-armors dmg target-body)]
        (let [current-hp (:current (get-hp target-body))
              new-hp (- current-hp dmg)
              lethal (<= new-hp 0)
              actual-dmg (if lethal current-hp dmg)]
          ; (show-dmg-effect target-body actual-dmg)
          (assoc-in! target-body [:destructible :hp :current] (max 0 new-hp))
          (dealt-dmg-triggers target-body actual-dmg is-crit-hit? lethal is-player-melee)
          (when lethal
            (assoc-in! target-body [:destructible :is-dead] true)
            (when-not (is-player? target-body)
              (add-to-removelist target-body))))))))




