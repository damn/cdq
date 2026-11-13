(ns game.item.instance  ; TODO refactor dependencies wg. instance-impl abkapselung
  (:require
    [cdq.core :refer [tile-width]]
    [cdq.core :refer [make-counter play-sound tick tick-finally-merge]]
    [cdq.core :refer [animation-stopped? render-centered-animation restart-animation]]
    [cdq.core :refer [folder-animation]]
    [cdq.core :refer [get-scaled-copy]]
    [cdq.core :refer [safe-merge]]
    cdq.core
    [game.item.cells :refer [try-pickup-item]]
    [game.item.grids :refer [showing-player-inventory?]]
    [game.item.in-hand :refer [empty-item-in-hand is-item-in-hand? item-in-hand set-item-in-hand]]
    [cdq.core :refer [active add-to-removelist create-entity player-body]]
    [cdq.core :refer [position-component]]
    [game.components.body :refer [blocked-location? create-body]]
    [game.components.render :refer [image-render-component render-on-map]]
    [game.components.pressable :refer [pressable-component]]
    [game.utils.msg-to-player :refer [show-msg-to-player]]))

;;; glittering

(defn glittering-component []
  (safe-merge
    {:type :glittering
     :counter (make-counter 2000)
     :active false
     :animation (folder-animation :folder "effects/itemglitter/"
                                  :looping false
                                  :duration 300)}
    (active [delta {:keys [active counter animation] :as c} entity]
      (swap! entity assoc-in [(:type c)]
                 (if active
                   (let [animation (tick animation delta)]
                     (if (animation-stopped? animation)
                       (assoc c :active false :animation (restart-animation animation))
                       (assoc c :animation animation)))
                   (tick-finally-merge c :counter delta
                                         {:active true}))))
    (render-on-map :air [g _ {:keys [active animation] :as c} render-posi]
      (when active
        (render-centered-animation animation render-posi)))))

;;; instance

(def item-create-fns {})

(defmacro defitem [item-name & body]
  `(let [~'this-name ~item-name]
     (alter-var-root #'item-create-fns assoc ~item-name
       (fn ~@body))))

(defn create-item-instance ; TODO & data -> besser? da dann net ne extra map
  ([item-name]
    (create-item-instance item-name nil))
  ([item-name data]
    (let [create-fn (or (get item-create-fns item-name)
                        (throw (Error. (str "Could not find item: " item-name))))]
      (with-meta
        (assoc (create-fn data) :name item-name)
        {:pr :item}))))

(defmacro def-usable-item [namestr & {:keys [effect info image use-sound]}]
  `(defitem ~namestr [{cnt# :count}]
     {:effect ~effect
      :use-sound ~use-sound
      :info (str "Use: Rightclick or in belt with hotkeys.\n" ~info)
      :type :usable
      :image ~image
      :count (or cnt# 1)}))

(defn- pressable [item-instance]
  (pressable-component
    (or (:pretty-name item-instance) (:name item-instance))
    (fn [entity]
      (when-not @item-in-hand
        (cond
          (showing-player-inventory?) (do
                                        (play-sound "bfxr_takeit.wav")
                                        (add-to-removelist entity)
                                        (set-item-in-hand item-instance))

          (try-pickup-item item-instance) (do
                                            (play-sound "bfxr_pickup.wav")
                                            (add-to-removelist entity))

          :else (do
                  (play-sound "bfxr_denied.wav")
                  (show-msg-to-player "Your Inventory is full")))))
    :color (:color item-instance)))

(comment
  (cdq.core/do-in-game-loop
    (create-item-body [42 22] "Cyber-Implant")))

(def ^:private item-body-dimensions [8 8])

(defn- item-entity [position item-instance]
  (create-entity
    (position-component position)
    (create-body :solid false
                 :dimensions item-body-dimensions
                 :mouseover-outline true)
    (pressable item-instance)
    (image-render-component (apply get-scaled-copy (:image item-instance)
                                   item-body-dimensions)
                            :order :on-ground :apply-light false)
    (glittering-component)))

(defn create-item-body [position item]
  (item-entity position
               (if (string? item) (create-item-instance item) item)))

(defn put-item-on-ground []
  {:pre [(is-item-in-hand?)]}
  (let [{x 0 y 1 :as posi} (:value (:position @player-body))
        [w _] item-body-dimensions
        half-size (/ w tile-width 2)
        below-posi [x (+ 0.7 y)] ; put here so player sees that item is put on ground (confusing trying to put heal pot on player)
        blocked (blocked-location? below-posi half-size half-size :ground)
        ; blocked location checks if other solid bodies ... if put under player would block from player
        ;_ (println "BLOCKED? " (boolean blocked))
        position (if-not blocked below-posi posi)]
    (create-item-body position @item-in-hand))
  (empty-item-in-hand))

