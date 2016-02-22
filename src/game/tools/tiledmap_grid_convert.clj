(ns game.tools.tiledmap-grid-convert
  (:use data.grid2d
        utils.prxml)
  (:import (org.newdawn.slick.tiled TiledMap TileSet)))

(defn convert-to-localid [[x y] sheetw]
  (+ x (* y sheetw)))

; increase spritesheet width from 4 to 7 and update localids:
; (convert-to-localid (convert-to-spriteposi localid 4) 7)

(defn convert-to-spriteposi [localid sheetw]
  {:post [(integer? (% 0))
          (integer? (% 1))]}
  (assert (and (integer? localid) (>= localid 0)) (str "localid: " localid))
  [(mod localid sheetw)
   (int (/ localid sheetw))])

(defn get-spriteidx [[x y] ^TiledMap tiled-map layer-idx]
  (let [gid (.getTileId tiled-map x y layer-idx)]
    (when-not (zero? gid)
      (let [tileset ^TileSet (.getTileSetByGID tiled-map gid)]
        (convert-to-spriteposi (- gid (.firstGID tileset)) (.tilesAcross tileset))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(comment
      "A thing to note about the tileset is the firstgid attribute. The map
		  layer data is an array of 32-bit integers storing global tile ids.
		  Global means that they span the possibly multiple tilesets that the
		  map is using. Each tileset internally uses local tile ids, and the
		  firstgid attributes specifies how the local ids are mapped to global
		  ids. Namely, a tile with id A in the tileset will be referred to with
		  a number equal to firstgid+A in the layer data.")
		
(comment "Comment from mapeditor.org WIKI: The GIDs are arranged from left to
		     right and top to bottom. For example, if you have a map with a width
		     of 4 and a height of 3, then the GIDs in the data stream will be in
		     the following order (0,0) (1,0) (2,0) (3,0) (0,1) (1,1) (2,1) (3,1)
		     (0,2) (1,2) (2,2) (3,2)")
		
(defn make-tiledmap-file [tilesize targetfile grid calc-gid & tilesets]
  (let [width (width grid)
        height (height grid)]
    (spit targetfile
          (xml-str
            [:decl! {:version "1.0"}]
            [:map {:version 1.0 :orientation "orthogonal" :width width :height height :tilewidth tilesize :tileheight tilesize}
             (for [[firstgid tsx-source] tilesets]
               [:tileset {:firstgid firstgid :source tsx-source}])
             [:layer {:name "ground" :width width :height height}
              [:data
               (for [y (range height) ; y,x because rightmost fastest
                     x (range width)
                     :let [posi [x y]
                           value (get grid posi)
                           gid (calc-gid grid posi value)]]
                 ; <tile gid="74"/>
                 [:tile {:gid gid}])]]]))))

(defn resource [s] (str "maps/test/" s))


