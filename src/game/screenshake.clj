(ns game.screenshake
  (:require [engine.render :as g]))

(def shake-delay 45) ; Delay in ms between each new shake.

(def shake-decay 0.25) ; The rate at which the shake "dies down." 0.01 = 1 percent every new shake

(def shake-intensity 6) ; How far the shake should extend in pixels.

(def shake-snap false)  ; Whether to snap pixels to integers (translating to a decimal number
                       ; will result in subtle blurring!).

; shake-snap is not required when we set the shakex shakey translations to 0 after a finished shaking
; else they will be at values shakex = 0,00..something and the image is blurred permanently after screenshakes
; because we always translate-shake-before-render...

(def shake-time (atom shake-delay))
(def shake-amt (atom 0))
(def shakex (atom 0))
(def shakey (atom 0))

(defn- shake []
  (reset! shakex (* @shake-amt (Math/random)))
  (reset! shakey (* @shake-amt (Math/random)))
  (when shake-snap
    (swap! shakex int)
    (swap! shakey int))
  (reset! shake-time shake-delay)
  (swap! shake-amt - (* shake-decay shake-intensity))
  (when (<= @shake-amt 0)
    (reset! shakex 0)
    (reset! shakey 0)
    (reset! shake-amt 0)))

(defn shake-screen []
  (reset! shake-amt shake-intensity)
  (shake))

(defn update-shake [delta]
;  (when (engine.input/is-key-pressed? :M)
;    (shake-screen))
  (when (> @shake-amt 0)
    (swap! shake-time - delta)
    (when (<= @shake-time 0)
      (shake))))

(defn translate-shake-before-render [g]
  (when (and
          (not= 0 @shakex)
          (not= 0 @shakey))
    (g/translate g @shakex @shakey)))

(defn translate-shake-after-render [g]
  (when (and
          (not= 0 @shakex)
          (not= 0 @shakey))
    (g/translate g (- @shakex) (- @shakey))))



