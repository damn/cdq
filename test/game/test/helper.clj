(ns game.test.helper
  (:require game.start
            [game.components.ingame-loop :refer (do-in-game-loop)]
            [clojure.test :refer (deftest)]))

(defn start-game-once-fixture [f]
  (game.start/start :config "config/test.clj")
  (f))

(defn do-in-game-loop-await [f]
  (let [latch (java.util.concurrent.CountDownLatch. 1)]
    (do-in-game-loop (f)
                     (.countDown latch))
    (.await latch)))

(defmacro deftest-ingame [nm & exprs]
  `(deftest ~nm
     (do-in-game-loop-await
       (bound-fn [] ~@exprs))))

; Need to use bound-fn here
; otherwise the clojure.test stats are not counting number of
; assertions & failures in another thread.

; Alternative approach restarting the whole game:
; .setForceExit app-game-container - > AL is not inited again!
; problem is soundstore is not 'init' -> either set to false, call init yourself or make soundstore fresh and new
; Soundstore.clear ! -> works, but then on restart all images are just white ... maybe image store also need to clear?
;
; (.clear (org.newdawn.slick.openal.SoundStore/get))
; (.init (org.newdawn.slick.openal.SoundStore/get))
; Images have no context anymore ? they all have to be set to inited=false? all have to be reloaded??? ... this is hard;
