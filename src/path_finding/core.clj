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

(def start-pos (atom [1 0]))
(def end-pos (atom [20 20]))
(def path-finding-result (atom (pf/calculate-path @start-pos @end-pos)))

(defn cost [] (pf/calculate-path-cost (:path @path-finding-result)))

(defn get-color-from-id [id]
  (get {0 0xFFABFF4F
        1 0xFFD99D5C
        2 0xFF08BDBD
        3 0xFFF20B2F}
       id))

(defn grid-to-screen [x]
  (+ x (* tile-size x)))

(defn screen-to-grid [x]
  (int (/ x (inc tile-size))))

(defn draw-tile
  ([[x y]]
   (let [x (grid-to-screen x)
         y (grid-to-screen y)]
     (q/rect x y (inc tile-size) (inc tile-size))))
  ([[x y] pad r]
   (let [x (+ (/ pad 2) (grid-to-screen x))
         y (+ (/ pad 2) (grid-to-screen y))]
     (q/rect x y (- (inc tile-size) pad) (- (inc tile-size) pad) r))))

(defn draw-tiles [pad r]
  (dorun
   (for [x (range 0 (count pf/tilemap))
         y (range 0 (count pf/tilemap))]
     (do
       (q/fill (get-color-from-id (get-in pf/tilemap [y x])))
       (draw-tile [x y] pad r)))))

(defn draw-connection [[x0 y0] [x1 y1]]
  (apply q/line (map #(+ (/ tile-size 2) (grid-to-screen %)) [x0 y0 x1 y1])))

(defn draw-path [path]
  (reduce #(do (draw-connection %1 %2) %2) path))

(defn draw-highlight-tiles [tiles]
  (run! #(draw-tile % 0 0) tiles))

(defn setup []
  (q/frame-rate 60)
  (q/text-font (q/create-font "DejaVu Sans" 11 true)))

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
  (draw-path (:path @path-finding-result))

  ;; Draw Discovered
  (q/fill 20 20 100 80) 
  (q/stroke 20 20 100 20) 
  (q/stroke-weight 1.5) 
  (draw-highlight-tiles (:discovered @path-finding-result))

  ;; Draw Start and End points
  (q/fill 230 230 230 255)
  (q/stroke 50 50 50 50)
  (q/stroke-weight 2)
  (let [start-x (+ (/ tile-size 2) (grid-to-screen (first @start-pos)))
        start-y (+ (/ tile-size 2) (grid-to-screen (second @start-pos)))
        end-x   (+ (/ tile-size 2) (grid-to-screen (first @end-pos)))
        end-y   (+ (/ tile-size 2) (grid-to-screen (second @end-pos)))]
    (q/ellipse start-x
               start-y
               (dec tile-size)
               (dec tile-size))
    (q/stroke 50 50 50 250)
    (q/stroke-weight 1.5)
    (q/ellipse end-x
               end-y
               (dec tile-size)
               (dec tile-size))    )
  
  ;; Start and end point defining
  (if (q/mouse-pressed?)
    (let [x (max 0 (min (dec (q/width))  (q/mouse-x)))
          y (max 0 (min (dec (q/height)) (q/mouse-y)))
          grid-x (screen-to-grid x)
          grid-y (screen-to-grid y)]
      (q/text (str grid-x "," grid-y) x y)
      (case (q/mouse-button)
        :left
        (reset! end-pos [grid-x grid-y])
        :right
        (reset! start-pos [grid-x grid-y]))))
  
  ;; Draw HUD
  (q/fill 30 30 30 255) 
  (q/stroke 255 255 255 255) 
  (q/stroke-weight 20.5) 
  (q/text (str "Cost:" (cost)) 0 (dec (q/height))))

(q/defsketch path-finding
  :title "Path finding fun :D"
  :size [window-size window-size]
  :renderer :p2d
  :setup setup
  :settings #(q/smooth 2)
  :draw draw
  :mouse-released #(reset! path-finding-result (pf/calculate-path @start-pos @end-pos))
  :features [:keep-on-top]
  :middleware [m/pause-on-error])
