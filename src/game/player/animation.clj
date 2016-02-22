(ns game.player.animation
  (:use
    [utils.core :only (assoc-in!)]
    engine.render
    (game.components core render destructible)
    (game.components.skills core melee)
    game.player.skill.skillmanager))

(defn- control [body]
  (let [is-moving (:play-move-animation (get-component body :movement))
        holding-gun (= :gun (:type (get-current-standard-skill)))]
    (assoc-in! body [:movement :play-move-animation] false)
    (cond
      (is-dead? body)
        :death
      (is-attacking? (get-component body :skillmanager))
        (get-player-skill-animation-key)
      is-moving
        (if holding-gun :with-gun :moving)
      :else
        (if holding-gun :with-gun :standing))))

(defn player-animation []
  (let [folder (fn [s] (str "player/" s))]
    (animation-component
      control
      {:fist        (folder-animation :folder (folder "sword/") :prefix "fist"  :duration 300  :looping false)
       :sword       (folder-animation :folder (folder "sword/") :prefix "sword" :duration 350  :looping false)
       :sword-small (folder-animation :folder (folder "sword/") :prefix "small" :duration 250  :looping false)
       :sword-big   (folder-animation :folder (folder "sword/") :prefix "big"   :duration 700  :looping false)
       :with-gun    (folder-animation :folder (folder "with_gun/")              :duration 600  :looping true)
       :moving      (folder-animation :folder (folder "running/")               :duration 1000 :looping true)
       :standing    (folder-animation :folder (folder "stand/")                 :duration 4000 :looping true)
       :meditation  (folder-animation :folder (folder "meditation/")            :duration 1500 :looping false)
       :casting     (folder-animation :folder (folder "casting/")               :duration 300  :looping false)
       :shooting    (folder-animation :folder (folder "shooting/")              :duration 150  :looping false)
       :death       (create-animation [(create-image (folder "die/l.png"))])})))

(def ^:private animation-types
  {:movement  [:moving]
   :attacking [:sword :sword-small :sword-big :fist]
   :casting   [:casting :meditation]})

(defn inc-speed [component value types]
  {:pre [(contains? animation-types types)]}
  (reduce #(update-in %1 [%2 :speed] + value)
          component
          (get animation-types types)))

(comment
  (clojure.pprint/pprint
    (let [c (get-component player-body :animation)]
      (for [[types values] animation-types]
        [types (for [atype values]
                 [atype (-> c atype :speed float)])]))))
