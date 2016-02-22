(ns game.player.status-gui
  (:require [engine.render :as color])
  (:use
    utils.core
    (engine core render)
    (game media settings ingame-gui [mouseoverbody :only (get-mouseover-body)])
    (game.components core destructible
      [render :only (rendering)]
      ingame-loop)
    (game.components.skills core [melee :only (get-current-player-melee-dmg)])
    game.item.cells
    [game.player.session-data :only (current-character-name)]
    game.player.skill.skillmanager))

(defn- render-infostr-on-bar [g infostr y h]
  (render-readable-text g half-screen-w (+ y 3) :background false :centerx true :centery false
                        color/white
                        infostr))

(initialize
  (def- rahmen (create-image "gui/rahmen.png"))
  (def- rahmenw (first (get-dimensions rahmen)))
  (def- rahmenh (second (get-dimensions rahmen)))
  (def- hpcontent (create-image "gui/hp.png"))
  (def- manacontent (create-image "gui/mana.png")))

(defn- render-hpmana-bar [g x y contentimg minmaxval name]
  (draw-image rahmen x y)
  (draw-image (get-sub-image contentimg 0 0 (* rahmenw (get-ratio minmaxval)) rahmenh) x y)
  (render-infostr-on-bar g (str (readable-number (:current minmaxval)) "/" (int (:max minmaxval)) " " name) y rahmenh))

(defn- render-player-stats [g]
  (let [x (- half-screen-w (/ rahmenw 2))
        y-hp (- screen-height rahmenh)
        y-mana (- y-hp rahmenh)]
    (render-hpmana-bar g x y-hp hpcontent (get-hp player-body) "HP")
    (render-hpmana-bar g x y-mana manacontent (get-mana player-body) "MP")))

(ingame-loop-comp :player-hp-mana
  (rendering :hpmanabar [g c]
    (render-player-stats g)))

; TODO hat nix mit player zu tun
(ingame-loop-comp :render-armor-status
  (rendering [g c]
    (when-let [mouseover-body (get-mouseover-body)]
      (when-let [armor (get-armor mouseover-body)]
        (render-readable-text g half-screen-w 0 :centerx true
                              (str (readable-number armor) " Armor\n" (get-armor-reduce-info armor)))))))

(initialize
  (def- character-frame (make-frame :name :character
                                    :bounds [(- screen-width frame-screenborder-distance 160) (+ inventoryry inventory-height 5) 160 30]
                                    :hotkey char-hotkey
                                    :visible false
                                    :parent ingamestate-display)))

(defn- dmg-info [{:keys [is-melee dmg-info] :as skill}]
  (cond
    is-melee (variance-val-str (get-current-player-melee-dmg))
    dmg-info (dmg-info skill)))

(ingame-loop-comp :player-character-status
  (rendering [g c]
    (when (is-visible? character-frame)
      (let [[leftx topy] (:bounds @character-frame)]
        (render-readable-text g (+ leftx 2) (+ topy 2) :background false
          (str "Name: " @current-character-name)
          (str "Leftmouse Damage: " (dmg-info (get-selected-skill :left)))
          (str "Rightmouse Damage: " (dmg-info (get-selected-skill :right))))))))


