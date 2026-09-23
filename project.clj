(defproject cyberdungeonquest "alpha 3-SNAPSHOT"
  ;; Legacy Fabric's LWJGL 2 fork: stock 2.9.3 creates a Display on modern
  ;; macOS but never shows a window (spinning in MacOSXDisplay.update).
  :repositories [["legacyfabric" "https://maven.legacyfabric.net/"]]
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/tools.macro "0.1.2"]
                 ;; Exclude transitive platform jars so lein doesn't unpack
                 ;; windows/linux natives into target/native (overwriting OSX).
                 [org.lwjgl.lwjgl/lwjgl "2.9.4+legacyfabric.17"
                  :exclusions [org.lwjgl.lwjgl/lwjgl-platform
                               net.java.jinput/jinput-platform]]
                 [org.lwjgl.lwjgl/lwjgl-platform "2.9.4+legacyfabric.17" :classifier "natives-osx" :native-prefix ""]
                 [net.java.jinput/jinput-platform "2.0.5" :classifier "natives-osx" :native-prefix ""]
                 [com.nothingtofind/slick2d "customized-0.1.0-SNAPSHOT"]
                 [grid2d "0.1.0-SNAPSHOT"]]
  :java-source-paths ["src"]
  :aot [engine.render] ; read-string of Animation record
  :main game.start
  :uberjar-name "cdq_3.jar"
  :omit-source true
  :manifest {"Launcher-Main-Class" "game.start"
             "SplashScreen-Image" "splash.gif"
             "Launcher-VM-Args" "-Xms256m -Xmx256m -XstartOnFirstThread"}
  :jvm-opts ["-Xms256m"
             "-Xmx256m"
             "-XstartOnFirstThread"
             "-Dvisualvm.display.name=CDQ"]
  :profiles {:uberjar {:aot [game.starter game.start]
                       :main game.starter}}
  :aliases {"build" ["do" "clean" "uberjar"]})

; :main mapgen.test
;
; Swap lwjgl-platform / jinput-platform classifier for other OSes:
;   natives-osx | natives-windows | natives-linux

; TODO
; * seeking slowdown missle shoots even if no line of sight to player
; * check spells ignore armor?
; * nova should need line of sight to damage?
