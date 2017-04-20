(ns path-finding.core
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [path-finding.map-loading :as ml]
            [path-finding.path-finding :as pf]))

;; Ambiente:  Terreno com pesos, robo, ponto de destino
;; Sensores:  Ver tipo do terreno, posição atual, posição objetivo, para onde pode andar
;; Atuadores: Movimento
;; Medida:    Custo do caminho, nós expandidos
;; Parcialmente observável(?), determinístico, sequencial, agente único, estático, discreto
;; Fazer definição do sistema de produção: Estados, estado final, regras

(def window-size 400)
(def tile-size (dec (/ (float window-size) (count pf/tilemap))))

(def path-finding-result (pf/calculate-path [0 0] [10 15]))

(defn get-color-from-id [id]
  (get {0 0xFFABFF4F
        1 0xFFD99D5C
        2 0xFF08BDBD
        3 0xFFF20B2F}
       id))

(defn discrete-to-screen [x]
  (+ x (* tile-size x)))

(defn draw-tile
  ([[x y]]
   (let [x (discrete-to-screen x)
         y (discrete-to-screen y)]
     (q/rect x y (inc tile-size) (inc tile-size))))
  ([[x y] pad r]
   (let [x (+ (/ pad 2) (discrete-to-screen x))
         y (+ (/ pad 2) (discrete-to-screen y))]
     (q/rect x y (- (inc tile-size) pad) (- (inc tile-size) pad) r))))

(defn draw-tiles [pad r]
  (dorun
   (for [x (range 0 (count pf/tilemap))
         y (range 0 (count pf/tilemap))]
     (do
       (q/fill (get-color-from-id (get-in pf/tilemap [y x])))
       (draw-tile [x y] pad r)))))

(defn draw-connection [[x0 y0] [x1 y1]]
  (apply q/line (map #(+ (/ tile-size 2) (discrete-to-screen %)) [x0 y0 x1 y1])))

(defn draw-path [path]
  (reduce #(do (draw-connection %1 %2) %2) path)
  )

(defn draw-highlight-tiles [tiles]
  (run! #(draw-tile % 0 0) tiles))

(defn setup []
  (q/frame-rate 60)
  )

(defn draw []
  (q/background 20)

  ;; Draw map
  (q/stroke-weight 0)
  (draw-tiles 0 0)
  (q/fill 20 20 20 120)
  (q/rect 0 0 window-size window-size)
  (draw-tiles 0.0 0.0)

  ;; Draw Path
  (q/stroke-cap :square)
  (q/stroke-weight 2.0) 
  (q/stroke 100 60 60 255)
  (draw-path (:path path-finding-result))

  ;; Draw Discovered
  (q/fill 20 20 100 80) 
  (q/stroke 20 20 100 20) 
  (q/stroke-weight 1.5) 
  (draw-highlight-tiles (:discovered path-finding-result))
  
  ;; Draw Chicken  
  )

(q/defsketch path-finding
  :title "Path finding fun :D"
  :size [window-size window-size]
  :renderer :p2d
  :setup setup
  :settings #(q/smooth 2)
  :draw draw
  :features [:keep-on-top]
  :middleware [m/pause-on-error])
