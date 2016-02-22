(ns engine.settings)

(defn- running-as-jar? []
  (= "jar" (.getProtocol (clojure.java.io/resource "splash.gif"))))

(def jar-file? (running-as-jar?))
