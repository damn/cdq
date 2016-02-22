(ns game.item.instance  ; TODO refactor dependencies wg. instance-impl abkapselung
  (:use
    utils.core
    (engine core render)
    (game [settings :only (tile-width)])
    game.maps.data
    game.item.cells
    (game.components core position body misc render glittering pressable)
    game.utils.msg-to-player))

(def- defitem-prefix "item-")

(defn create-fn [type]
  (find-prefixed-var
    :namespace 'game.item.instance-impl
    :prefix defitem-prefix
    :prefixed-type type))

(defmacro defitem [item-name & body]
  `(let [~'this-name ~item-name]
     (defn ~(symbol (str defitem-prefix item-name))
       ~@body)))

(defn create-item-instance ; TODO & data -> besser? da dann net ne extra map
  ([item-name]
    (create-item-instance item-name nil))
  ([item-name data]
    (with-meta
      (assoc ((create-fn item-name) data) :name item-name)
      {:pr :item})))

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
  (game.components.ingame-loop/do-in-game-loop
    (create-item-body [42 22] "Cyber-Implant")))

(def- item-body-dimensions [8 8])

(defentity ^:private item-entity [position item-instance]
  (position-component position)
  (create-body :solid false
               :dimensions item-body-dimensions
               :mouseover-outline true)
  (pressable item-instance)
  (image-render-component (apply get-scaled-copy (:image item-instance)
                                 item-body-dimensions)
                          :order :on-ground :apply-light false)
  (glittering-component))

(defn create-item-body [position item]
  (item-entity position
               (if (string? item) (create-item-instance item) item)))

(defn put-item-on-ground []
  {:pre [(is-item-in-hand?)]}
  (let [{x 0 y 1 :as posi} (get-position player-body)
        [w _] item-body-dimensions
        half-size (/ w tile-width 2)
        below-posi [x (+ 0.7 y)] ; put here so player sees that item is put on ground (confusing trying to put heal pot on player)
        blocked (blocked-location? below-posi half-size half-size :ground)
        ; blocked location checks if other solid bodies ... if put under player would block from player
        ;_ (println "BLOCKED? " (boolean blocked))
        position (if-not blocked below-posi posi)]
    (create-item-body position @item-in-hand))
  (empty-item-in-hand))

