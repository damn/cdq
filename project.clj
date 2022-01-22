(defproject cyberdungeonquest "alpha 3-SNAPSHOT"
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/tools.macro "0.1.2"]
                 [org.lwjgl.lwjgl/lwjgl          "2.9.3"]
                 [org.lwjgl.lwjgl/lwjgl-platform "2.9.3" :classifier "natives-osx" :native-prefix ""]
                 [com.nothingtofind/slick2d "customized-0.1.0-SNAPSHOT"]
                 [grid2d "0.1.0-SNAPSHOT"]]
  :java-source-paths ["src"]
  :aot [engine.render] ; read-string of Animation record
  :main game.start
  :uberjar-name "cdq_3.jar"
  :omit-source true
  :manifest {"Launcher-Main-Class" "game.start"
             "SplashScreen-Image" "splash.gif"
             "Launcher-VM-Args" "-Xms256m -Xmx256m"}
  :jvm-opts ["-Xms256m"
             "-Xmx256m"
             "-Dvisualvm.display.name=CDQ"]
  :profiles {:uberjar {:aot [game.starter game.start]
                       :main game.starter}}
  :aliases {"build" ["do" "clean" "uberjar"]})

; :main mapgen.test
;
; TODO: set correct natives for OSX, linux

; TODO
; * seeking slowdown missle shoots even if no line of sight to player
; * check spells ignore armor?
; * nova should need line of sight to damage?
