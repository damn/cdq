(defproject cyberdungeonquest "alpha 3-SNAPSHOT"
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/tools.macro "0.1.2"]
                 [org.lwjgl.lwjgl/lwjgl          "2.9.3"]
                 ;; natives extracted manually to native/macosx (lein :native-prefix was pulling wrong platform)
                 [org.lwjgl.lwjgl/lwjgl-platform "2.9.3" :classifier "natives-osx"]
                 ;; inlined from com.nothingtofind/slick2d (customized-0.1.0-SNAPSHOT) -> slick/src
                 [org.jcraft/jorbis "0.0.17"]
                 [org.clojars.aseipp/ibxm "0.0.1"]
                 [grid2d "0.1.0-SNAPSHOT"]]
  :java-source-paths ["src" "slick/src"]
  :aot [engine.render] ; read-string of Animation record
  :main game.start
  :uberjar-name "cdq_3.jar"
  :omit-source true
  :manifest {"Launcher-Main-Class" "game.start"
             "SplashScreen-Image" "splash.gif"
             "Launcher-VM-Args" "-Xms256m -Xmx256m"}
  :jvm-opts ~(let [libpath (.getAbsolutePath (java.io.File. "native/macosx"))]
               ["-Xms256m"
                "-Xmx256m"
                (str "-Djava.library.path=" libpath)
                (str "-Dorg.lwjgl.librarypath=" libpath)
                "-Dvisualvm.display.name=CDQ"])
  :profiles {:uberjar {:aot [game.starter game.start]
                       :main game.starter}}
  :plugins [[lein-hiera "2.0.0"]]
  :aliases {"build" ["do" "clean" "uberjar"]})

; :main mapgen.test
;
; TODO: set correct natives for OSX, linux

; TODO
; * seeking slowdown missle shoots even if no line of sight to player
; * check spells ignore armor?
; * nova should need line of sight to damage?
