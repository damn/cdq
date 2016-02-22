(ns game.serialization
  (:require [engine.render :as g]
            [game.session :as session])
  (:import (org.newdawn.slick Image Color)))

;; Image

(defn- relevant-data [^Image image]
  {:transparent-color (.transparent image)
   :iref              (.getResourceReference image)
   :width             (.getWidth          image)
   :height            (.getHeight         image)
   :texture-width     (.getTextureWidth   image)
   :texture-height    (.getTextureHeight  image)
   :texture-offset-x  (.getTextureOffsetX image)
   :texture-offset-y  (.getTextureOffsetY image)
   :texture (let [^org.newdawn.slick.opengl.Texture texture (.getTexture image)]
              {:texture-width  (.getTextureWidth  texture)
               :texture-height (.getTextureHeight texture)})})

(defn- save-image-data [image]
  (let [{:keys [iref
                transparent-color
                width
                height
                texture-width
                texture-height
                texture-offset-x
                texture-offset-y
                texture]}
        (relevant-data image)

        in-px-w (fn [texels] (int (* texels (:texture-width  texture))))
        in-px-h (fn [texels] (int (* texels (:texture-height texture))))
        sw (in-px-w texture-width)
        sh (in-px-h texture-height)
        sx (in-px-w texture-offset-x)
        sy (in-px-h texture-offset-y)]
    {:iref              iref
     :transparent-color transparent-color
     :sub-bounds        [sx sy sw sh]
     :bounds            [width height]}))

; could not call subImage or scaledCopy if it is not necessary
(defn- recreate-image [{iref              :iref
                        transparent-color :transparent-color
                        [sx sy sw sh]     :sub-bounds
                        [width height]    :bounds}]
  (-> (g/create-image iref :transparent transparent-color)
      (g/get-sub-image sx sy sw sh)
      (g/get-scaled-copy width height)))

(defmethod session/write-to-disk Image [^Image i]
  (with-meta (save-image-data i)
             {:pr :image}))

(defmethod session/load-from-disk :image [data]
  (recreate-image data))

;; Color

(defmethod session/write-to-disk Color [^Color c]
  (with-meta [(.r c) (.g c) (.b c) (.a c)]
             {:pr :color}))

(defmethod session/load-from-disk :color [[r g b a]]
  (g/rgbcolor :r r :g g :b b :a a))
